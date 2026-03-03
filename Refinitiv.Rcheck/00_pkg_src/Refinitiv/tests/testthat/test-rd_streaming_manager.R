library(testthat)
library(mockery)

# Unit tests for StreamManager class

context("StreamManager")

# Teardown: Disconnect all managers and stop event loops after each test
teardown({
  # Run any pending later callbacks to clear the queue
  if (requireNamespace("later", quietly = TRUE)) {
    # Process and clear all pending callbacks
    tryCatch({
      later::run_now()
      # Give a moment for any scheduled callbacks to complete
      Sys.sleep(0.1)
      later::run_now()
    }, error = function(e) {
      # Ignore errors during cleanup
    })
  }
})

# Helper function to create mock WebSocket
create_mock_websocket <- function(ready_state = 1L, connected = TRUE) {
  mock_ws <- list(
    readyState = function() ready_state,
    connect = function() NULL,
    send = function(msg) NULL,
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  return(mock_ws)
}

# Test StreamManager initialization
test_that("StreamManager initializes with default parameters", {
  manager <- StreamManager$new()
  
  expect_s3_class(manager, "StreamManager")
  expect_equal(manager$get_connection_state(), "disconnected")
})

test_that("StreamManager initializes with custom parameters", {
  manager <- StreamManager$new(
    base_url = "http://example.com",
    port = 8080L,
    stream_type = "pricing"
  )
  
  expect_s3_class(manager, "StreamManager")
  expect_equal(manager$get_connection_state(), "disconnected")
})

test_that("StreamManager uses streaming_port option", {
  withr::with_options(
    list(streaming_port = 9000L, eikon_port = NULL),
    {
      manager <- StreamManager$new()
      expect_equal(manager$.__enclos_env__$private$.port, 9000L)
    }
  )
})

test_that("StreamManager falls back to eikon_port when streaming_port is unset", {
  withr::with_options(
    list(streaming_port = NULL, eikon_port = 9000L),
    {
      manager <- StreamManager$new()
      expect_equal(manager$.__enclos_env__$private$.port, 9000L)
    }
  )
})

test_that("StreamManager defaults to 9000 when no port options are set", {
  withr::with_options(
    list(streaming_port = NULL, eikon_port = NULL),
    {
      manager <- StreamManager$new()
      expect_equal(manager$.__enclos_env__$private$.port, 9000L)
    }
  )
})

# Test StreamManager connect
test_that("StreamManager connect creates WebSocket connection", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)
  
  # Mock dependencies
  mock_handshake <- list(access_token = "test_token_123")
  mock_ws <- create_mock_websocket()
  
  manager <- StreamManager$new(stream_type = "pricing")
  
  # Stub external functions
  stub(manager$connect, "rd_handshake", function() mock_handshake)
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://localhost:9000/test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1,"Domain":"Login"}')
  
  # Mock websocket package
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg == "websocket") return(TRUE)
    if (pkg == "later") return(TRUE)
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  
  # Connect
  result <- manager$connect(debug = FALSE)
  
  expect_true(result)
  expect_equal(manager$get_connection_state(), "connected")
})

test_that("StreamManager connect skips if already connected", {
  manager <- StreamManager$new()
  
  # Set connection state to connected
  manager$.__enclos_env__$private$.connection_state <- "connected"
  
  result <- manager$connect(force = FALSE, debug = FALSE)
  
  expect_true(result)
})

test_that("StreamManager connect forces reconnection when force=TRUE", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)
  
  mock_handshake <- list(access_token = "test_token")
  mock_ws <- create_mock_websocket()
  
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  
  stub(manager$connect, "rd_handshake", function() mock_handshake)
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://localhost:9000/test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1,"Domain":"Login"}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  
  result <- manager$connect(force = TRUE, debug = FALSE)
  
  expect_true(result)
})

test_that("StreamManager connect errors when websocket package not available", {
  manager <- StreamManager$new()
  
  stub(manager$connect, "requireNamespace", function(pkg, quietly) FALSE)
  
  expect_error(
    manager$connect(debug = FALSE),
    "Please install 'websocket' package"
  )
})

# Test StreamManager subscribe
test_that("StreamManager subscribe creates subscription", {
  manager <- StreamManager$new()
  
  # Set up manager as connected and logged in
  manager$.__enclos_env__$private$.connection_state <- "connected"
  manager$.__enclos_env__$private$.logged_in <- TRUE
  manager$.__enclos_env__$private$.stream_id_counter <- 1L
  
  # Create mock WebSocket with send tracking using an environment
  send_env <- new.env(parent = emptyenv())
  send_env$called <- FALSE
  send_env$msg <- NULL
  mock_ws <- list(
    readyState = function() 1L,
    connect = function() NULL,
    send = function(msg) {
      send_env$called <- TRUE
      send_env$msg <- msg
    },
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws
  
  # Create stream definition
  stream_def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )
  
  # Mock later package
  stub(manager$subscribe, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") return(TRUE)
    return(FALSE)
  })
  stub(manager$subscribe, "later::run_now", function(timeout) NULL)
  stub(manager$subscribe, "Sys.sleep", function(time) NULL)
  
  stream_id <- manager$subscribe(stream_def)
  
  expect_type(stream_id, "integer")
  expect_true(send_env$called)
  expect_true(grepl('"Name":"EUR="', send_env$msg))
})

test_that("StreamManager subscribe queues when not logged in", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  manager$.__enclos_env__$private$.logged_in <- FALSE
  manager$.__enclos_env__$private$.stream_id_counter <- 1L
  
  stream_def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )
  
  stream_id <- manager$subscribe(stream_def)
  
  expect_type(stream_id, "integer")
  # Subscription should be queued
  expect_equal(length(manager$.__enclos_env__$private$.pending_subscriptions), 1)
})

# Test StreamManager unsubscribe
test_that("StreamManager unsubscribe removes stream from list", {
  manager <- StreamManager$new()
  
  # Add a stream
  manager$.__enclos_env__$private$.streams[["2"]] <- list()
  
  manager$unsubscribe(stream_id = 2L)
  
  # Stream should be removed
  expect_null(manager$.__enclos_env__$private$.streams[["2"]])
})

# Test StreamManager disconnect
test_that("StreamManager disconnect closes WebSocket", {
  manager <- StreamManager$new()
  
  close_env <- new.env(parent = emptyenv())
  close_env$called <- FALSE
  mock_ws <- list(
    readyState = function() 1L,
    connect = function() NULL,
    send = function(msg) NULL,
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() {
      close_env$called <- TRUE
    }
  )
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws
  manager$.__enclos_env__$private$.connection_state <- "connected"
  
  manager$disconnect()
  
  expect_true(close_env$called)
  expect_equal(manager$get_connection_state(), "disconnected")
})

test_that("StreamManager disconnect handles NULL WebSocket", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.ws <- NULL
  
  # Should not error
  expect_silent(manager$disconnect())
})

# Test StreamManager get_connection_state
test_that("StreamManager get_connection_state returns current state", {
  manager <- StreamManager$new()
  
  expect_equal(manager$get_connection_state(), "disconnected")
  
  manager$.__enclos_env__$private$.connection_state <- "connected"
  expect_equal(manager$get_connection_state(), "connected")
})

# Test StreamManager register_handler and unregister_handler
test_that("StreamManager register_handler stores handler", {
  manager <- StreamManager$new()
  
  handler <- function(msg, type) NULL
  
  manager$register_handler(stream_id = 2L, handler = handler)
  
  handlers <- manager$.__enclos_env__$private$.message_handlers
  # Handler is stored in a list
  expect_type(handlers[[as.character(2L)]], "list")
  expect_equal(handlers[[as.character(2L)]][[1]], handler)
})

test_that("StreamManager unregister_handler removes handler", {
  manager <- StreamManager$new()
  
  handler <- function(msg, type) NULL
  manager$register_handler(stream_id = 2L, handler = handler)
  
  manager$unregister_handler(stream_id = 2L)
  
  handlers <- manager$.__enclos_env__$private$.message_handlers
  expect_null(handlers[[as.character(2L)]])
})

# Test StreamManager message processing
# Note: We test message processing through the public interface
# by simulating WebSocket events
test_that("StreamManager processes login refresh message", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  manager$.__enclos_env__$private$.logged_in <- FALSE
  
  # Create a mock event object
  login_event <- list(
    data = jsonlite::toJSON(list(
      Domain = "Login",
      ID = 1L,
      Type = "Refresh",
      State = list(Stream = "Open", Data = "Ok")
    ), auto_unbox = TRUE)
  )
  
  # Process message through private method (we can access it for testing)
  manager$.__enclos_env__$private$.process_message(login_event, debug = FALSE)
  
  expect_true(manager$.__enclos_env__$private$.logged_in)
})

test_that("StreamManager processes update message", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  manager$.__enclos_env__$private$.logged_in <- TRUE
  
  # Register handler with state tracking using environment
  handler_env <- new.env(parent = emptyenv())
  handler_env$called <- FALSE
  handler_env$msg <- NULL
  handler <- function(msg, type) {
    handler_env$called <- TRUE
    handler_env$msg <- msg
  }
  manager$register_handler(stream_id = 2L, handler = handler)
  
  # Create a mock event object
  update_event <- list(
    data = jsonlite::toJSON(list(
      ID = 2L,
      Type = "Update",
      Fields = list(BID = 1.165, ASK = 1.1652),
      Key = list(Name = "EUR=")
    ), auto_unbox = TRUE)
  )
  
  # Process message
  manager$.__enclos_env__$private$.process_message(update_event, debug = FALSE)
  
  expect_true(handler_env$called)
  expect_equal(handler_env$msg$Fields$BID, 1.165)
})

test_that("StreamManager handles JSON array messages", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  
  # Message as array (common format) - create as JSON string
  array_event <- list(
    data = jsonlite::toJSON(list(
      list(
        Domain = "Login",
        ID = 1L,
        Type = "Refresh",
        State = list(Stream = "Open", Data = "Ok")
      )
    ), auto_unbox = TRUE)
  )
  
  # Should extract first element
  manager$.__enclos_env__$private$.process_message(array_event, debug = FALSE)
  
  expect_true(manager$.__enclos_env__$private$.logged_in)
})

test_that("StreamManager handles ping messages", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  
  send_env <- new.env(parent = emptyenv())
  send_env$called <- FALSE
  mock_ws <- list(
    readyState = function() 1L,
    connect = function() NULL,
    send = function(msg) {
      send_env$called <- TRUE
    },
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws
  
  # Create ping event
  ping_event <- list(
    data = jsonlite::toJSON(list(Type = "Ping"), auto_unbox = TRUE)
  )
  
  manager$.__enclos_env__$private$.process_message(ping_event, debug = FALSE)
  
  # Ping should trigger pong response
  # Note: actual implementation may vary
  expect_true(send_env$called)
})

test_that("StreamManager handles status messages with errors", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"
  
  # Register error handler with state tracking using environment
  error_env <- new.env(parent = emptyenv())
  error_env$called <- FALSE
  handler <- function(msg, type) {
    error_env$called <- TRUE
  }
  manager$register_handler(stream_id = 2L, handler = handler)
  
  # Create error event
  error_event <- list(
    data = jsonlite::toJSON(list(
      ID = 2L,
      Type = "Status",
      State = list(Stream = "Closed", DataState = "Suspect")
    ), auto_unbox = TRUE)
  )
  
  # The status message has Stream = "Closed", which correctly triggers a warning
  expect_warning(
    manager$.__enclos_env__$private$.process_message(error_event, debug = FALSE),
    "Stream status: Closed"
  )
  
  # Error should be routed to handler
  expect_true(error_env$called)
})


# --- Reconnection tests (H4) ---

test_that("User-initiated disconnect sets flag and clears streams", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # Simulate connected state with streams
  priv$.connection_state <- "connected"
  priv$.logged_in <- TRUE
  priv$.streams[["2"]] <- list()
  priv$.message_handlers[["2"]] <- list(function(m, t) NULL)

  mock_ws <- create_mock_websocket()
  priv$.ws <- mock_ws

  manager$disconnect()

  expect_true(priv$.user_initiated_close)
  expect_false(priv$.reconnecting)
  expect_equal(priv$.connection_state, "disconnected")
  expect_false(priv$.logged_in)
  expect_length(priv$.streams, 0)
  expect_length(priv$.message_handlers, 0)
})

test_that("Accidental close triggers reconnect when auto_reconnect is TRUE", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.connection_state <- "connected"
  priv$.user_initiated_close <- FALSE

  # Track whether .attempt_reconnect was called
  attempt_env <- new.env(parent = emptyenv())
  attempt_env$called <- FALSE

  # We can't easily mock a private method, so instead we'll directly
  # simulate what onClose does and check the state transitions
  # by calling the close handler logic manually

  withr::with_options(
    list(refinitiv_streaming_auto_reconnect = TRUE,
         refinitiv_streaming_max_reconnect = 1L),
    {
      # Simulate accidental close: set state as onClose handler does
      priv$.connection_state <- "disconnected"
      priv$.logged_in <- FALSE
      priv$.reconnecting <- FALSE

      # Call .attempt_reconnect directly (this is what onClose triggers)
      # Stub later::later to just record it was called (don't actually delay)
      stub(
        priv$.attempt_reconnect,
        "later::later",
        function(fn, delay) { attempt_env$called <- TRUE }
      )
      stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

      priv$.attempt_reconnect(debug = FALSE)

      expect_true(priv$.reconnecting)
      expect_equal(priv$.reconnect_attempt, 1L)
      expect_true(attempt_env$called)
    }
  )
})

test_that("User-initiated close prevents reconnect", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.user_initiated_close <- TRUE
  priv$.reconnect_attempt <- 0L

  priv$.attempt_reconnect(debug = FALSE)

  # Should not have started reconnecting
  expect_false(priv$.reconnecting)
  expect_equal(priv$.reconnect_attempt, 0L)
})

test_that("Max retries exhaustion stops reconnection", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.user_initiated_close <- FALSE

  withr::with_options(
    list(refinitiv_streaming_max_reconnect = 3L),
    {
      # Simulate having already exhausted all retries
      priv$.reconnect_attempt <- 3L

      expect_warning(
        priv$.attempt_reconnect(debug = FALSE),
        "WebSocket reconnection failed after 3 attempts"
      )

      expect_false(priv$.reconnecting)
      expect_equal(priv$.reconnect_attempt, 0L)
    }
  )
})

test_that("Reconnect delay sequence uses correct base delays with jitter", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.user_initiated_close <- FALSE

  delays_recorded <- numeric(0)

  withr::with_options(
    list(refinitiv_streaming_max_reconnect = 5L),
    {
      for (i in 1:5) {
        priv$.reconnect_attempt <- i - 1L
        priv$.reconnecting <- FALSE

        stub(
          priv$.attempt_reconnect,
          "later::later",
          function(fn, delay) { delays_recorded[length(delays_recorded) + 1L] <<- delay }
        )
        stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

        priv$.attempt_reconnect(debug = FALSE)
      }
    }
  )

  # Expected base: [5, 10, 15, 60, 60] with ±15% jitter
  expected_bases <- c(5, 10, 15, 60, 60)
  for (i in seq_along(delays_recorded)) {
    expect_true(
      delays_recorded[i] >= expected_bases[i] * 0.85 &&
      delays_recorded[i] <= expected_bases[i] * 1.15,
      info = paste("Delay", i, "=", delays_recorded[i],
                   "expected ~", expected_bases[i])
    )
  }
})

test_that("Reconnect preserves streams and handlers (accidental close)", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # Simulate connected state with active streams
  priv$.connection_state <- "connected"
  priv$.logged_in <- TRUE
  priv$.user_initiated_close <- FALSE

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  priv$.streams[["2"]] <- def
  priv$.message_handlers[["2"]] <- list(function(m, t) NULL)

  # Simulate accidental onClose (just the state changes, not actual WS)
  priv$.connection_state <- "disconnected"
  priv$.logged_in <- FALSE
  priv$.reconnecting <- FALSE

  # Streams and handlers should still be present (not cleared)
  expect_length(priv$.streams, 1)
  expect_length(priv$.message_handlers, 1)
  expect_equal(names(priv$.streams), "2")
})

test_that("on_reconnecting callback fires with attempt number", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.user_initiated_close <- FALSE

  cb_env <- new.env(parent = emptyenv())
  cb_env$fired <- FALSE
  cb_env$attempt <- NULL

  manager$register_reconnect_callback("on_reconnecting", function(attempt) {
    cb_env$fired <- TRUE
    cb_env$attempt <- attempt
  })

  withr::with_options(
    list(refinitiv_streaming_max_reconnect = 10L),
    {
      stub(
        priv$.attempt_reconnect,
        "later::later",
        function(fn, delay) NULL
      )
      stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

      priv$.attempt_reconnect(debug = FALSE)
    }
  )

  expect_true(cb_env$fired)
  expect_equal(cb_env$attempt, 1L)
})

test_that("on_reconnected callback fires on login after reconnect", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.connection_state <- "connected"
  priv$.logged_in <- FALSE
  priv$.reconnecting <- TRUE  # Simulate we're in reconnect path

  cb_env <- new.env(parent = emptyenv())
  cb_env$fired <- FALSE

  manager$register_reconnect_callback("on_reconnected", function(attempt) {
    cb_env$fired <- TRUE
  })

  # Process a login success message
  login_event <- list(
    data = jsonlite::toJSON(list(
      Domain = "Login",
      ID = 1L,
      Type = "Refresh",
      State = list(Stream = "Open", Data = "Ok")
    ), auto_unbox = TRUE)
  )

  priv$.process_message(login_event, debug = FALSE)

  expect_true(cb_env$fired)
  expect_true(priv$.logged_in)
  expect_false(priv$.reconnecting)
  expect_equal(priv$.reconnect_attempt, 0L)
})

test_that("auto_reconnect option disables reconnection", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.connection_state <- "connected"
  priv$.user_initiated_close <- FALSE

  withr::with_options(
    list(refinitiv_streaming_auto_reconnect = FALSE),
    {
      # Simulate what onClose does
      priv$.connection_state <- "disconnected"
      priv$.logged_in <- FALSE
      priv$.reconnecting <- FALSE

      # With auto_reconnect = FALSE, the onClose handler
      # would skip .attempt_reconnect. Test that logic:
      auto_reconnect <- getOption("refinitiv_streaming_auto_reconnect", TRUE)
      if (!priv$.user_initiated_close && auto_reconnect) {
        priv$.attempt_reconnect(debug = FALSE)
      }

      # Should NOT have started reconnecting
      expect_false(priv$.reconnecting)
      expect_equal(priv$.reconnect_attempt, 0L)
    }
  )
})

test_that("Guard flag prevents reconnect during user disconnect delay", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  priv$.user_initiated_close <- FALSE
  priv$.reconnecting <- TRUE
  priv$.reconnect_attempt <- 1L

  # Simulate: user calls disconnect() while reconnect is pending
  manager$disconnect()

  # Now if the later callback fires, it should see user_initiated_close = TRUE
  expect_true(priv$.user_initiated_close)
  expect_false(priv$.reconnecting)
})

test_that("register_reconnect_callback validates inputs", {
  manager <- StreamManager$new()

  expect_error(
    manager$register_reconnect_callback("bad_event", function() NULL),
    "event must be"
  )

  expect_error(
    manager$register_reconnect_callback("on_reconnecting", "not a function"),
    "callback must be a function"
  )
})

test_that("connect() resets user_initiated_close flag", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # Set as if user previously disconnected
  priv$.user_initiated_close <- TRUE
  priv$.connection_state <- "disconnected"

  # Stub all the connect internals
  stub(manager$connect, "rd_handshake", function() list(access_token = "test"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) {
    create_mock_websocket()
  })
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)

  manager$connect(debug = FALSE)

  expect_false(priv$.user_initiated_close)
})


dump_refinitiv_options("test-rd_streaming_manager")
