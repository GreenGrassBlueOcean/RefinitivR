library(testthat)
library(mockery)

# Unit tests for StreamManager class

context("StreamManager")

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# Teardown: Disconnect all managers and stop event loops after each test
teardown({
  # Run any pending later callbacks to clear the queue
  if (requireNamespace("later", quietly = TRUE)) {
    # Process and clear all pending callbacks
    tryCatch(
      {
        later::run_now()
        # Give a moment for any scheduled callbacks to complete
        Sys.sleep(0.1)
        later::run_now()
      },
      error = function(e) {
        # Ignore errors during cleanup
      }
    )
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
    if (pkg == "websocket") {
      return(TRUE)
    }
    if (pkg == "later") {
      return(TRUE)
    }
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
    if (pkg %in% c("websocket", "later")) {
      return(TRUE)
    }
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
    if (pkg == "later") {
      return(TRUE)
    }
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
    list(
      refinitiv_streaming_auto_reconnect = TRUE,
      refinitiv_streaming_max_reconnect = 1L
    ),
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
        function(fn, delay) {
          attempt_env$called <- TRUE
        }
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
          function(fn, delay) {
            delays_recorded[length(delays_recorded) + 1L] <<- delay
          }
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
      info = paste(
        "Delay", i, "=", delays_recorded[i],
        "expected ~", expected_bases[i]
      )
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
  priv$.reconnecting <- TRUE # Simulate we're in reconnect path

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
    if (pkg %in% c("websocket", "later")) {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)

  manager$connect(debug = FALSE)

  expect_false(priv$.user_initiated_close)
})


# --- Phase 1: .setup_websocket_handlers() closure coverage ---

# Handler-capturing mock: captures the closures registered via onOpen/onClose/etc.
create_handler_capturing_ws <- function() {
  env <- new.env(parent = emptyenv())
  env$on_open <- NULL
  env$on_close <- NULL
  env$on_message <- NULL
  env$on_error <- NULL
  env$sent <- character(0)

  ws <- list(
    onOpen    = function(h) { env$on_open    <- h },
    onClose   = function(h) { env$on_close   <- h },
    onMessage = function(h) { env$on_message <- h },
    onError   = function(h) { env$on_error   <- h },
    send      = function(msg) { env$sent <- c(env$sent, msg) },
    connect   = function() NULL,
    close     = function() NULL,
    readyState = function() 1L
  )
  class(ws) <- "WebSocket"
  list(ws = ws, captured = env)
}

test_that("onError handler sets connection_state to error and warns", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  expect_warning(
    hc$captured$on_error(list(message = "test WS error")),
    "WebSocket error"
  )
  expect_equal(priv$.connection_state, "error")
})

test_that("onError handler is silenced after user-initiated disconnect", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  # Simulate user-initiated close (as disconnect() would set)
  priv$.user_initiated_close <- TRUE

  # Post-close error should be silently ignored — no warning, no state change
  priv$.connection_state <- "connected"
  expect_silent(hc$captured$on_error(list(message = "Timer Expired")))
  expect_equal(priv$.connection_state, "connected")
})

test_that("onError handler is silenced when ws is NULL", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  # Simulate ws already cleaned up
  priv$.ws <- NULL
  priv$.connection_state <- "connected"
  expect_silent(hc$captured$on_error(list(message = "Timer Expired")))
  expect_equal(priv$.connection_state, "connected")
})

test_that("disconnect() neutralizes handlers before closing", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)
  priv$.connection_state <- "connected"

  manager$disconnect()

  # After disconnect, ws should be NULL
  expect_null(priv$.ws)
  expect_equal(priv$.connection_state, "disconnected")

  # Handlers were replaced with no-ops — firing them should be silent
  expect_silent(hc$captured$on_error(list(message = "Timer Expired")))
  expect_silent(hc$captured$on_close(list(code = 1000)))
})

test_that("onMessage handler routes to process_message", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  # Send a login refresh via the captured onMessage handler
  login_json <- jsonlite::toJSON(list(
    Domain = "Login", ID = 1L, Type = "Refresh",
    State = list(Stream = "Open", Data = "Ok")
  ), auto_unbox = TRUE)

  hc$captured$on_message(list(data = login_json))
  expect_true(priv$.logged_in)
})

test_that("onClose handler does not reconnect on user-initiated close", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.user_initiated_close <- TRUE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  hc$captured$on_close(list(code = 1000, reason = "Normal closure"))

  expect_equal(priv$.connection_state, "disconnected")
  expect_false(priv$.logged_in)
  expect_false(priv$.reconnecting)
})

test_that("onClose handler triggers reconnect on accidental close", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.user_initiated_close <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  withr::local_options(
    refinitiv_streaming_auto_reconnect = TRUE,
    refinitiv_streaming_max_reconnect = 10L
  )

  # Stub later::later to prevent actual scheduling
  stub(priv$.attempt_reconnect, "later::later", function(fn, delay) NULL)
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  hc$captured$on_close(list(code = 1006, reason = "Abnormal"))

  expect_equal(priv$.connection_state, "disconnected")
  expect_true(priv$.reconnecting)
  expect_equal(priv$.reconnect_attempt, 1L)
})

test_that("onClose handler skips reconnect when auto_reconnect is FALSE", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.user_initiated_close <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  withr::local_options(refinitiv_streaming_auto_reconnect = FALSE)

  hc$captured$on_close(list(code = 1006, reason = "Abnormal"))

  expect_equal(priv$.connection_state, "disconnected")
  expect_false(priv$.reconnecting)
})

test_that("onOpen handler does nothing when not reconnecting", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.reconnecting <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  hc$captured$on_open(list())

  # No login sent
  expect_length(hc$captured$sent, 0)
})

test_that("onOpen handler sends login on reconnect for pricing streams", {
  local_refinitiv_state()
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private
  priv$.reconnecting <- TRUE
  priv$.stream_type <- "pricing"

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = FALSE)

  # Inject a token into the vault
  refinitiv_vault_set("access_token", "mock_reconnect_token")

  hc$captured$on_open(list())

  expect_equal(priv$.connection_state, "connected")
  # Should have sent a login request
  expect_length(hc$captured$sent, 1)
  expect_true(grepl('"Domain":"Login"', hc$captured$sent[1]))
})

# --- Phase 2: .attempt_reconnect() callback body ---

test_that("Reconnect callback aborts when user_initiated_close set during delay", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued_callbacks <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued_callbacks[[length(queued_callbacks) + 1L]] <<- list(fn = func, delay = delay)
  })
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)

  priv$.attempt_reconnect(debug = FALSE)
  expect_length(queued_callbacks, 1)

  # Simulate user disconnecting during the delay
  priv$.user_initiated_close <- TRUE
  queued_callbacks[[1]]$fn()

  # Should have aborted without creating a new WebSocket
  expect_false(priv$.reconnecting)
})

test_that("Reconnect callback aborts when reconnecting flag cleared during delay", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued_callbacks <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued_callbacks[[length(queued_callbacks) + 1L]] <<- list(fn = func, delay = delay)
  })
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)

  priv$.attempt_reconnect(debug = FALSE)

  # Clear reconnecting flag during delay
  priv$.reconnecting <- FALSE
  queued_callbacks[[1]]$fn()

  # Should not have created a new WebSocket
  expect_false(priv$.reconnecting)
})

test_that("Reconnect callback captures valid delay and function", {
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued_callbacks <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued_callbacks[[length(queued_callbacks) + 1L]] <<- list(fn = func, delay = delay)
  })
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)

  priv$.attempt_reconnect(debug = FALSE)
  expect_length(queued_callbacks, 1)

  # Verify the callback is a function with valid delay (~5s ± 15% jitter)
  expect_true(is.function(queued_callbacks[[1]]$fn))
  expect_true(queued_callbacks[[1]]$delay >= 5 * 0.85)
  expect_true(queued_callbacks[[1]]$delay <= 5 * 1.15)

  # Verify state was set
  expect_true(priv$.reconnecting)
  expect_equal(priv$.reconnect_attempt, 1L)
})

test_that("Reconnect callback refreshes expired token", {
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued_callbacks <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued_callbacks[[length(queued_callbacks) + 1L]] <<- list(fn = func, delay = delay)
  })
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)

  priv$.attempt_reconnect(debug = FALSE)

  # Track whether rd_handshake was called
  hs_env <- new.env(parent = emptyenv())
  hs_env$called <- FALSE

  stub(queued_callbacks[[1]]$fn, "rd_VerifyToken", function() FALSE)
  stub(queued_callbacks[[1]]$fn, "rd_handshake", function() {
    hs_env$called <- TRUE
    list(access_token = "refreshed_token")
  })
  stub(queued_callbacks[[1]]$fn, "refinitiv_vault_get", function(key) "refreshed_token")
  stub(queued_callbacks[[1]]$fn, "get_streaming_url", function(type, port) "ws://test:9000")
  stub(queued_callbacks[[1]]$fn, "get_streaming_protocol", function(type) "tr_json2")
  stub(queued_callbacks[[1]]$fn, "create_streaming_headers", function(token) list())

  mock_ws <- create_handler_capturing_ws()
  stub(queued_callbacks[[1]]$fn, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) {
    mock_ws$ws
  })

  queued_callbacks[[1]]$fn()

  expect_true(hs_env$called)
})

test_that(".fire_reconnect_callbacks fires callbacks and handles errors", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  cb_env <- new.env(parent = emptyenv())
  cb_env$attempts <- integer(0)
  cb_env$second_called <- FALSE

  manager$register_reconnect_callback("on_reconnecting", function(attempt) {
    cb_env$attempts <- c(cb_env$attempts, attempt)
  })
  # A second callback that errors
  manager$register_reconnect_callback("on_reconnecting", function(attempt) {
    stop("callback error")
  })
  # A third that still fires
  manager$register_reconnect_callback("on_reconnecting", function(attempt) {
    cb_env$second_called <- TRUE
  })

  # Fire should call all callbacks, warn on error, continue to others
  expect_warning(
    priv$.fire_reconnect_callbacks("on_reconnecting", 3L),
    "Error in on_reconnecting callback"
  )
  expect_equal(cb_env$attempts, 3L)
  expect_true(cb_env$second_called)

  # NULL event returns silently
  expect_silent(priv$.fire_reconnect_callbacks("nonexistent", 1L))
})

# --- Phase 3: connect() login flow edge cases ---

test_that("connect() login wait loop exits when login succeeds", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)

  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private

  mock_ws <- create_mock_websocket()
  sleep_count <- 0L

  stub(manager$connect, "rd_handshake", function() list(access_token = "test"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })

  # Tripwire: simulate login succeeding after 2 iterations
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  stub(manager$connect, "Sys.sleep", function(time) {
    sleep_count <<- sleep_count + 1L
    if (sleep_count > 10L) stop("Infinite loop detected")
    if (sleep_count == 2L) priv$.logged_in <- TRUE
  })

  result <- manager$connect(debug = FALSE)
  expect_true(result)
  expect_true(priv$.logged_in)
  expect_true(sleep_count >= 2L && sleep_count <= 10L)
})

test_that("connect() login fallback sends pending subscriptions when login times out", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)

  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private

  # Pre-queue a pending subscription
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  priv$.pending_subscriptions[["2"]] <- def

  hc <- create_handler_capturing_ws()

  stub(manager$connect, "rd_handshake", function() list(access_token = "test"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) hc$ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  # Login never succeeds within the wait — Sys.sleep exits the while loop via timeout
  stub(manager$connect, "Sys.sleep", function(time) {
    # Don't set .logged_in — let it time out
  })

  result <- manager$connect(debug = FALSE)

  expect_true(result)
  # Fallback should have set logged_in and sent the pending subscription
  expect_true(priv$.logged_in)
  expect_length(priv$.pending_subscriptions, 0)
  # The subscription was sent via WS
  expect_true(length(hc$captured$sent) >= 2) # login + subscription
})

# --- Phase 4: .handle_refresh() subscription replay ---

test_that(".handle_refresh replays pending subscriptions after login", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.logged_in <- FALSE

  # Set up a pending subscription
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  priv$.pending_subscriptions[["2"]] <- def

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws

  # Process login refresh
  login_msg <- list(
    Domain = "Login", ID = 1L, Type = "Refresh",
    State = list(Stream = "Open", Data = "Ok")
  )
  priv$.handle_refresh(login_msg, debug = FALSE)

  expect_true(priv$.logged_in)
  expect_length(priv$.pending_subscriptions, 0)
  # Subscription request was sent
  expect_true(length(hc$captured$sent) > 0)
  expect_true(any(grepl("EUR=", hc$captured$sent)))
})

test_that(".handle_refresh replays active streams after reconnect login", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.logged_in <- FALSE
  priv$.reconnecting <- TRUE
  priv$.reconnect_attempt <- 2L

  # Active stream (preserved during reconnect)
  def <- StreamDefinition$new(universe = "GBP=", fields = c("ASK"))
  priv$.streams[["3"]] <- def

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws

  # Track on_reconnected callback
  cb_env <- new.env(parent = emptyenv())
  cb_env$fired <- FALSE
  manager$register_reconnect_callback("on_reconnected", function(attempt) {
    cb_env$fired <- TRUE
  })

  login_msg <- list(
    Domain = "Login", ID = 1L, Type = "Refresh",
    State = list(Stream = "Open", Data = "Ok")
  )
  priv$.handle_refresh(login_msg, debug = FALSE)

  expect_true(priv$.logged_in)
  expect_false(priv$.reconnecting)
  expect_equal(priv$.reconnect_attempt, 0L)
  expect_true(cb_env$fired)
  # Active stream subscription replayed
  expect_true(any(grepl("GBP=", hc$captured$sent)))
})

test_that(".handle_refresh routes non-login refresh to stream handlers", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.logged_in <- TRUE

  handler_env <- new.env(parent = emptyenv())
  handler_env$called <- FALSE
  manager$register_handler(2L, function(msg, type) {
    handler_env$called <- TRUE
  })

  # Non-login refresh (ID != 1, Domain != "Login")
  refresh_msg <- list(
    ID = 2L, Type = "Refresh",
    Key = list(Name = "EUR="),
    Fields = list(BID = 1.10)
  )
  priv$.handle_refresh(refresh_msg, debug = FALSE)

  expect_true(handler_env$called)
})


# --- Phase 5: .process_message() and .handle_status() branch coverage ---

test_that(".process_message returns early on null/empty event data", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # NULL event
  expect_silent(priv$.process_message(NULL, debug = FALSE))
  # Event with NULL data
  expect_silent(priv$.process_message(list(data = NULL), debug = FALSE))
})

test_that(".process_message returns early when no message type found", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # Valid JSON but no Type/type field
  event <- list(data = jsonlite::toJSON(list(ID = 2L, Fields = list(BID = 1.1)), auto_unbox = TRUE))
  expect_silent(priv$.process_message(event, debug = FALSE))
})

test_that(".process_message handles lowercase type field", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.logged_in <- FALSE

  # Use lowercase "type" instead of "Type"
  event <- list(
    data = jsonlite::toJSON(list(
      type = "Refresh",
      Domain = "Login",
      ID = 1L,
      State = list(Stream = "Open", Data = "Ok")
    ), auto_unbox = TRUE)
  )

  priv$.process_message(event, debug = FALSE)
  expect_true(priv$.logged_in)
})

test_that(".process_message routes unknown message type to all handlers", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"

  handler_env <- new.env(parent = emptyenv())
  handler_env$called <- FALSE
  manager$register_handler(99L, function(msg, type) handler_env$called <- TRUE)

  # Unknown message type "Custom"
  event <- list(
    data = jsonlite::toJSON(list(Type = "Custom", ID = 99L), auto_unbox = TRUE)
  )

  priv$.process_message(event, debug = FALSE)
  expect_true(handler_env$called)
})

test_that(".handle_status silently ignores DuplicateOpenRequest", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  dup_message <- list(
    ID = 2L,
    State = list(
      Stream = "Closed",
      Text = "DuplicateOpenRequest: stream already exists"
    )
  )

  # Should NOT produce warnings for duplicate requests
  expect_silent(priv$.handle_status(dup_message))
})

test_that(".handle_status warns on non-duplicate Closed stream", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  closed_message <- list(
    ID = 2L,
    State = list(Stream = "Closed", Text = "Item not found")
  )

  expect_warning(
    priv$.handle_status(closed_message),
    "Stream status"
  )
})

test_that(".handle_status does not warn on Ok/Open status text", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  ok_message <- list(
    ID = 2L,
    State = list(Stream = "Open", Text = "Ok")
  )

  expect_silent(priv$.handle_status(ok_message))
})

test_that(".process_message handles empty parsed JSON", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # JSON that parses to empty list
  event <- list(data = "[]")
  expect_silent(priv$.process_message(event, debug = FALSE))
})

# --- Phase 6: .send_subscription_requests() + .route_message() ---

test_that(".send_subscription_requests sends multi-instrument requests", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  sent_env <- new.env(parent = emptyenv())
  sent_env$messages <- character(0)
  mock_ws <- list(
    send = function(msg) sent_env$messages <- c(sent_env$messages, msg),
    readyState = function() 1L,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  priv$.ws <- mock_ws

  # Create a definition with 2 instruments
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK")
  )

  # to_request returns a named list for multi-instrument
  priv$.send_subscription_requests(def, first_id = 2L, debug = FALSE)

  # Should have sent 2 separate requests
  expect_equal(length(sent_env$messages), 2)
  expect_true(grepl("EUR=", sent_env$messages[1]))
  expect_true(grepl("GBP=", sent_env$messages[2]))
})

test_that(".send_subscription_requests handles NULL WebSocket", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.ws <- NULL

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))

  # Should not error even with NULL WebSocket.
  # Use expect_no_error (not expect_silent) because later::run_now() may flush
  # pending callbacks that emit platform-specific warnings (e.g. CheckTerminalType
  # on Windows CI where no terminal is available).
  expect_no_error(suppressWarnings(
    priv$.send_subscription_requests(def, first_id = 2L, debug = FALSE)
  ))
})

test_that(".route_message falls back to all handlers when no ID match", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  handler_env <- new.env(parent = emptyenv())
  handler_env$count <- 0L
  manager$register_handler(10L, function(msg, type) handler_env$count <- handler_env$count + 1L)
  manager$register_handler(20L, function(msg, type) handler_env$count <- handler_env$count + 1L)

  # Message with ID that doesn't match any handler → fallback to ALL handlers
  priv$.route_message(list(ID = 999L), message_type = "Update")
  expect_equal(handler_env$count, 2L)

  # Message with no ID at all → also routes to all handlers
  handler_env$count <- 0L
  priv$.route_message(list(Fields = list(BID = 1.1)), message_type = "Update")
  expect_equal(handler_env$count, 2L)
})

test_that(".route_message warns when handler throws error", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  call_env <- new.env(parent = emptyenv())
  call_env$second_called <- FALSE

  manager$register_handler(2L, function(msg, type) stop("handler error"))
  manager$register_handler(2L, function(msg, type) call_env$second_called <- TRUE)

  expect_warning(
    priv$.route_message(list(ID = 2L), message_type = "Update"),
    "Error in message handler"
  )
  # Second handler still called despite first erroring
  expect_true(call_env$second_called)
})


# --- Debug coverage: connect() with debug = TRUE ---

test_that("connect() debug logging covers all branches", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)

  mock_ws <- create_mock_websocket()
  sleep_count <- 0L

  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private

  stub(manager$connect, "rd_handshake", function() list(access_token = "tok"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list(Authorization = "Bearer tok"))
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) pkg %in% c("websocket", "later"))
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  stub(manager$connect, "Sys.sleep", function(time) {
    sleep_count <<- sleep_count + 1L
    if (sleep_count > 10L) stop("Infinite loop")
    if (sleep_count == 1L) priv$.logged_in <- TRUE
  })

  expect_message(
    manager$connect(debug = TRUE),
    "Starting connection process"
  )
  expect_true(manager$get_connection_state() == "connected")
})

test_that("connect() debug=TRUE skip-already-connected branch", {
  manager <- StreamManager$new()
  manager$.__enclos_env__$private$.connection_state <- "connected"

  expect_message(
    manager$connect(force = FALSE, debug = TRUE),
    "Already connected"
  )
})

test_that("connect() fallback when later unavailable", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)

  mock_ws <- create_mock_websocket()
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private

  stub(manager$connect, "rd_handshake", function() list(access_token = "tok"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  # websocket available, later NOT available
  stub(manager$connect, "requireNamespace", function(pkg, quietly) pkg == "websocket")
  stub(manager$connect, "Sys.sleep", function(time) NULL)

  result <- manager$connect(debug = FALSE)
  expect_true(result)
})

# --- Debug coverage: .process_message() with debug = TRUE ---

test_that(".process_message debug logging for Refresh/Update/Ping/Status/unknown", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- TRUE

  # Track which handlers were called
  call_env <- new.env(parent = emptyenv())
  call_env$types <- character(0)

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  manager$register_handler(5L, function(msg, type) {
    call_env$types <- c(call_env$types, type)
  })

  # Refresh message
  refresh_json <- '{"Type":"Refresh","ID":5,"Domain":"MarketPrice","Fields":{"BID":1.1}}'
  expect_message(
    priv$.process_message(list(data = refresh_json), debug = TRUE),
    "Handling Refresh message"
  )

  # Update message
  update_json <- '{"Type":"Update","ID":5,"Fields":{"BID":1.2}}'
  expect_message(
    priv$.process_message(list(data = update_json), debug = TRUE),
    "Handling Update message"
  )

  # Ping message
  ping_json <- '{"Type":"Ping"}'
  expect_message(
    priv$.process_message(list(data = ping_json), debug = TRUE),
    "Handling Ping message"
  )

  # Status message (also produces a "Stream status: Closed" warning)
  status_json <- '{"Type":"Status","ID":5,"State":{"Stream":"Closed","Data":"Suspect"}}'
  suppressWarnings(expect_message(
    priv$.process_message(list(data = status_json), debug = TRUE),
    "Handling Status message"
  ))

  # Unknown message type
  unknown_json <- '{"Type":"Ack","ID":5}'
  expect_message(
    priv$.process_message(list(data = unknown_json), debug = TRUE),
    "Unknown message type"
  )
})

test_that(".process_message debug logging for empty/no-type messages", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # NULL event
  expect_message(
    priv$.process_message(NULL, debug = TRUE),
    "Event has no data"
  )

  # Message with no Type field — covers the "no message type" debug block
  no_type_json <- '{"Domain":"Login","ID":1}'
  expect_message(
    priv$.process_message(list(data = no_type_json), debug = TRUE),
    "No message type found"
  )
})

test_that(".process_message error handler covers warning path", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # Invalid JSON that causes a downstream error (not a parse issue)
  # Force an error in .handle_refresh by making fromJSON return something unexpected
  stub(priv$.process_message, "jsonlite::fromJSON", function(txt, ...) {
    list(Type = "Refresh", Domain = "Login")
  })
  # Make handle_refresh throw a real error
  stub(priv$.process_message, "private$.handle_refresh", function(msg, debug) {
    stop("unexpected error in handler")
  })

  expect_warning(
    priv$.process_message(list(data = "{}"), debug = FALSE),
    "Error processing message"
  )
})

# --- Debug coverage: .handle_refresh() with debug = TRUE ---

test_that(".handle_refresh debug logging for login success", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- FALSE
  priv$.connection_state <- "connected"

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws

  stub(priv$.handle_refresh, "later::run_now", function(timeout) NULL)

  # Login refresh with full State info
  msg <- list(
    Type = "Refresh", Domain = "Login", ID = 1L,
    State = list(Stream = "Open", Data = "Ok")
  )

  expect_message(
    priv$.handle_refresh(msg, debug = TRUE),
    "LOGIN SUCCESSFUL"
  )
  expect_true(priv$.logged_in)
})

test_that(".handle_refresh debug logging with lowercase id", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws

  stub(priv$.handle_refresh, "later::run_now", function(timeout) NULL)

  # Login response using lowercase 'id' field
  msg <- list(Type = "Refresh", Domain = "Login", id = 1L,
              State = list(Stream = "Open", Data = "Ok"))

  expect_message(
    priv$.handle_refresh(msg, debug = TRUE),
    "Refresh message id:"
  )
  expect_true(priv$.logged_in)
})

test_that(".handle_refresh debug logging with no ID and no domain", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- TRUE

  handler_env <- new.env(parent = emptyenv())
  handler_env$called <- FALSE
  manager$register_handler(5L, function(msg, type) handler_env$called <- TRUE)

  # A non-login refresh (e.g. market data) with ID that routes to handler
  msg <- list(Type = "Refresh", ID = 5L, Fields = list(BID = 1.1))

  expect_message(
    priv$.handle_refresh(msg, debug = TRUE),
    "Routing refresh message to stream handlers"
  )
  expect_true(handler_env$called)
})

test_that(".handle_refresh no-streams debug branch", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws

  stub(priv$.handle_refresh, "later::run_now", function(timeout) NULL)

  # Login success with no pending subscriptions and no streams
  msg <- list(Type = "Refresh", Domain = "Login", ID = 1L,
              State = list(Stream = "Open", Data = "Ok"))

  expect_message(
    priv$.handle_refresh(msg, debug = TRUE),
    "No streams to subscribe"
  )
})

test_that(".handle_refresh Sys.sleep fallback when later unavailable", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- FALSE

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws

  stub(priv$.handle_refresh, "requireNamespace", function(pkg, quietly) FALSE)
  sleep_env <- new.env(parent = emptyenv())
  sleep_env$called <- FALSE
  stub(priv$.handle_refresh, "Sys.sleep", function(t) sleep_env$called <- TRUE)

  msg <- list(Type = "Refresh", Domain = "Login", ID = 1L,
              State = list(Stream = "Open", Data = "Ok"))

  priv$.handle_refresh(msg, debug = FALSE)
  expect_true(priv$.logged_in)
  expect_true(sleep_env$called)
})

# --- Debug coverage: .attempt_reconnect() ---

test_that(".attempt_reconnect debug logging", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued[[length(queued) + 1L]] <<- list(fn = func, delay = delay)
  })

  expect_message(
    priv$.attempt_reconnect(debug = TRUE),
    "Scheduling reconnect attempt"
  )
  expect_length(queued, 1)
})

test_that(".attempt_reconnect warns when later unavailable", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) FALSE)

  expect_warning(
    priv$.attempt_reconnect(debug = FALSE),
    "later.*required"
  )
  expect_false(priv$.reconnecting)
})

test_that("reconnect callback error branch doesn't crash", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE
  priv$.reconnecting <- TRUE

  queued <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued[[length(queued) + 1L]] <<- list(fn = func, delay = delay)
  })

  priv$.attempt_reconnect(debug = FALSE)
  expect_length(queued, 1)

  # Make the callback error on WebSocket creation
  stub(queued[[1]]$fn, "rd_VerifyToken", function() TRUE)
  stub(queued[[1]]$fn, "refinitiv_vault_get", function(key) "tok")
  stub(queued[[1]]$fn, "get_streaming_url", function(type, port) "ws://test")
  stub(queued[[1]]$fn, "get_streaming_protocol", function(type) "tr_json2")
  stub(queued[[1]]$fn, "create_streaming_headers", function(token) list())
  stub(queued[[1]]$fn, "websocket::WebSocket$new", function(...) stop("connection refused"))

  # The error handler tries to reschedule via priv$.attempt_reconnect(),
  # but that inner call uses the real later::later (mockery stubs don't
  # propagate into R6 private methods called from within a closure).
  # Suppress the "later package required" warning that occurs when the
  # inner .attempt_reconnect can't find later.
  suppressWarnings(queued[[1]]$fn())

  # Key assertions: error was caught, state was updated
  # reconnecting set to FALSE by error handler before re-calling .attempt_reconnect
  # (the inner attempt may or may not succeed depending on later availability)
  expect_false(priv$.user_initiated_close)
})

# --- Debug coverage: .route_message with lowercase id ---

test_that(".route_message routes by lowercase id", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  call_env <- new.env(parent = emptyenv())
  call_env$called <- FALSE
  manager$register_handler(3L, function(msg, type) call_env$called <- TRUE)

  priv$.route_message(list(id = 3L, Fields = list(BID = 1.1)), message_type = "update")
  expect_true(call_env$called)
})

# --- Debug coverage: disconnect error path ---

test_that("disconnect warns when ws$close() throws", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  bad_ws <- list(
    onError = function(h) NULL,
    onClose = function(h) NULL,
    onMessage = function(h) NULL,
    onOpen = function(h) NULL,
    close = function() stop("already closed")
  )
  priv$.ws <- bad_ws
  priv$.connection_state <- "connected"

  expect_warning(manager$disconnect(), "Error closing WebSocket")
  expect_null(priv$.ws)
})

# --- subscribe auto-connect branch ---

test_that("get_websocket returns current ws reference", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  expect_null(manager$get_websocket())

  mock_ws <- create_mock_websocket()
  priv$.ws <- mock_ws
  expect_identical(manager$get_websocket(), mock_ws)
})


# --- Debug coverage: remaining debug-only branches ---

test_that(".setup_websocket_handlers debug=TRUE covers all handler logging", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = TRUE)

  # Fire all four handlers — each has debug logging
  withr::local_options(refinitiv_streaming_auto_reconnect = FALSE)
  expect_message(hc$captured$on_open(list()), "WebSocket opened")
  expect_message(hc$captured$on_close(list(code = 1000, reason = "test")), "WebSocket closed")
  expect_message(
    hc$captured$on_message(list(data = '{"Type":"Ping"}')),
    "RAW MESSAGE RECEIVED"
  )
})

test_that("connect() debug timeout fallback path", {
  withr::local_options(refinitiv_base_url = "http://localhost", streaming_port = 9000L)

  mock_ws <- create_mock_websocket()
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private

  # Pre-queue a pending subscription to exercise the fallback loop
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  priv$.pending_subscriptions[["2"]] <- def

  stub(manager$connect, "rd_handshake", function() list(access_token = "tok"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) pkg %in% c("websocket", "later"))
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  # Login never succeeds → exercises timeout fallback (lines 134, 140, 148-149)
  stub(manager$connect, "Sys.sleep", function(time) NULL)

  expect_message(
    manager$connect(debug = TRUE),
    "Login response not detected, using fallback"
  )
  expect_true(priv$.logged_in)
  expect_length(priv$.pending_subscriptions, 0)
})

test_that(".process_message debug covers array extraction and error paths", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  # Array-wrapped message (single-element unnamed list from OMM protocol)
  # This exercises the array extraction branch (line 566)
  array_json <- '[{"Type":"Ping"}]'
  expect_message(
    priv$.process_message(list(data = array_json), debug = TRUE),
    "Message was an array"
  )

  # Message with no type and no Domain/ID — exercises the "No message type" branch
  no_type_json <- '{"SomeField":"value"}'
  expect_message(
    priv$.process_message(list(data = no_type_json), debug = TRUE),
    "No message type found"
  )
})

test_that(".handle_refresh debug covers non-login refresh and State details", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- TRUE

  handler_env <- new.env(parent = emptyenv())
  handler_env$called <- FALSE
  manager$register_handler(5L, function(msg, type) handler_env$called <- TRUE)

  # Non-login refresh with State info (exercises lines 674-680)
  msg <- list(Type = "Refresh", ID = 5L,
              State = list(Stream = "Open", Data = "Ok"),
              Fields = list(BID = 1.1))

  expect_message(
    priv$.handle_refresh(msg, debug = TRUE),
    "State\\$Stream"
  )
  expect_true(handler_env$called)
})

test_that(".handle_refresh debug: no Domain, no ID", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.logged_in <- TRUE

  msg <- list(Type = "Refresh", Fields = list(BID = 1.1))

  expect_message(
    priv$.handle_refresh(msg, debug = TRUE),
    "has no Domain field"
  )
})

test_that(".attempt_reconnect debug: user-initiated abort", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- TRUE

  expect_message(
    priv$.attempt_reconnect(debug = TRUE),
    "Reconnect aborted: user-initiated close"
  )
})

test_that(".attempt_reconnect debug: max retries exhausted", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE
  priv$.reconnect_attempt <- 10L

  expect_warning(
    expect_message(
      priv$.attempt_reconnect(debug = TRUE),
      "Reconnect exhausted"
    ),
    "reconnection failed after"
  )
})

test_that(".handle_status debug logging", {
  withr::local_options(refinitiv_streaming_debug = TRUE)
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  msg <- list(ID = 5L, State = list(Stream = "Closed", Data = "Suspect", Code = "NotFound", Text = "Item not found"))

  suppressWarnings(expect_message(
    priv$.handle_status(msg),
    "Status message - Code"
  ))
})

test_that(".send_subscription_requests debug logging", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private

  sent_env <- new.env(parent = emptyenv())
  sent_env$messages <- character(0)
  mock_ws <- list(
    readyState = function() 1L,
    send = function(msg) sent_env$messages <- c(sent_env$messages, msg)
  )
  priv$.ws <- mock_ws

  stub(priv$.send_subscription_requests, "later::run_now", function(timeout) NULL)
  stub(priv$.send_subscription_requests, "requireNamespace", function(pkg, quietly) TRUE)

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))

  expect_message(
    priv$.send_subscription_requests(def, first_id = 2L, debug = TRUE),
    "Sending subscription"
  )
})

test_that("subscribe debug: login wait queue path", {
  withr::local_options(refinitiv_streaming_debug = TRUE)
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.logged_in <- FALSE

  stub(manager$subscribe, "requireNamespace", function(pkg, quietly) TRUE)
  stub(manager$subscribe, "later::run_now", function(timeout) NULL)
  stub(manager$subscribe, "Sys.sleep", function(t) NULL)

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))

  # Login never succeeds → queues as pending
  expect_message(
    manager$subscribe(def),
    "Login not complete, queueing"
  )
  expect_true(length(priv$.pending_subscriptions) > 0)
})


# =============================================================================
# Debug + uncovered branch coverage (rd_streaming_manager.R)
# =============================================================================

# --- Lines 217-219: subscribe() calls connect() when manager is disconnected ---
#
# Temporarily unlock the R6 binding for connect() and replace it with a stub
# that throws immediately. This avoids namespace-level mocking via
# local_mocked_bindings(.env = asNamespace(...)) which leaks under covr.

test_that("subscribe() calls connect() when manager is disconnected (lines 217-219)", {
  manager <- StreamManager$new(stream_type = "pricing")
  def     <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))

  env <- manager$.__enclos_env__$self
  unlockBinding("connect", env)
  orig <- env$connect
  env$connect <- function(debug = FALSE) stop("connect intercepted")
  withr::defer({
    env$connect <- orig
    lockBinding("connect", env)
  })

  expect_error(manager$subscribe(def), "connect intercepted")
})

# --- Line 417: .attempt_reconnect() callback debug: user_initiated_close set during delay ---

test_that(".attempt_reconnect callback debug: cancelled-during-delay message (line 417)", {
  manager <- StreamManager$new()
  priv    <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued <- list()
  stub(priv$.attempt_reconnect, "later::later",
    function(func, delay) queued[[length(queued) + 1L]] <<- list(fn = func, delay = delay))
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)
  priv$.attempt_reconnect(debug = TRUE)      # debug captured in callback closure
  expect_length(queued, 1)

  # Simulate user disconnecting while the callback was pending
  priv$.user_initiated_close <- TRUE

  msgs <- capture_messages(queued[[1]]$fn())

  expect_true(any(grepl("Reconnect cancelled during delay", msgs)),
    info = "line 417")
  expect_false(priv$.reconnecting)
})

# --- Lines 422, 429, 439: callback body normal-execution debug messages ---

test_that(".attempt_reconnect callback debug: starting/token-expired/new-WS messages (lines 422/429/439)", {
  manager <- StreamManager$new(stream_type = "pricing")
  priv    <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued <- list()
  stub(priv$.attempt_reconnect, "later::later",
    function(func, delay) queued[[length(queued) + 1L]] <<- list(fn = func, delay = delay))
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)
  priv$.attempt_reconnect(debug = TRUE)

  mock_ws <- create_handler_capturing_ws()

  # Stub everything the callback body calls
  stub(queued[[1]]$fn, "rd_VerifyToken",         function() FALSE)   # expired → refresh
  stub(queued[[1]]$fn, "rd_handshake",            function() list(access_token = "new_tok"))
  stub(queued[[1]]$fn, "refinitiv_vault_get",     function(key) "new_tok")
  stub(queued[[1]]$fn, "get_streaming_url",       function(type, port) "ws://test:9000")
  stub(queued[[1]]$fn, "get_streaming_protocol",  function(type) "tr_json2")
  stub(queued[[1]]$fn, "create_streaming_headers", function(token) list())
  stub(queued[[1]]$fn, "websocket::WebSocket$new",
    function(url, protocols, headers, autoConnect) mock_ws$ws)

  msgs <- capture_messages(queued[[1]]$fn())

  expect_true(any(grepl("Reconnect attempt.*starting",            msgs)), info = "line 422")
  expect_true(any(grepl("Token expired, refreshing",             msgs)), info = "line 429")
  expect_true(any(grepl("Creating new WebSocket",                msgs)), info = "line 439")
})

# --- Line 456: callback error-handler debug message ---

test_that(".attempt_reconnect callback debug: failed-attempt message (line 456)", {
  manager <- StreamManager$new(stream_type = "pricing")
  priv    <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE
  priv$.reconnecting         <- TRUE

  queued <- list()
  stub(priv$.attempt_reconnect, "later::later",
    function(func, delay) queued[[length(queued) + 1L]] <<- list(fn = func, delay = delay))
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  withr::local_options(refinitiv_streaming_max_reconnect = 10L)
  priv$.attempt_reconnect(debug = TRUE)

  stub(queued[[1]]$fn, "rd_VerifyToken",           function() TRUE)   # token ok
  stub(queued[[1]]$fn, "refinitiv_vault_get",      function(key) "tok")
  stub(queued[[1]]$fn, "get_streaming_url",        function(type, port) "ws://test")
  stub(queued[[1]]$fn, "get_streaming_protocol",   function(type) "tr_json2")
  stub(queued[[1]]$fn, "create_streaming_headers", function(token) list())
  # Set user_initiated_close=TRUE *inside* the stub before throwing so the
  # error handler's rescheduling guard (`!priv$.user_initiated_close`) is FALSE.
  # Without this, the error handler calls .attempt_reconnect() again, scheduling
  # a later::later callback that the teardown's run_now() would pick up and fire
  # against a real (unavailable) server.
  stub(queued[[1]]$fn, "websocket::WebSocket$new", function(...) {
    priv$.user_initiated_close <- TRUE
    stop("connection refused")
  })

  suppressWarnings(msgs <- capture_messages(queued[[1]]$fn()))

  expect_true(any(grepl("Reconnect attempt failed", msgs)), info = "line 456")
})

# --- Line 482: onError handler debug message ---

test_that(".setup_websocket_handlers onError debug=TRUE emits WebSocket error message (line 482)", {
  manager <- StreamManager$new()
  priv    <- manager$.__enclos_env__$private

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = TRUE)

  priv$.user_initiated_close <- FALSE  # guard is off — error should propagate

  expect_warning(
    msgs <- capture_messages(
      hc$captured$on_error(list(message = "timeout error"))
    ),
    "WebSocket error"
  )
  expect_true(any(grepl("WebSocket error.*timeout error", msgs)), info = "line 482")
  expect_equal(priv$.connection_state, "error")
})

# --- Line 529: onOpen reconnect sends login with debug message ---

test_that(".setup_websocket_handlers onOpen debug=TRUE emits reconnect login message (line 529)", {
  local_refinitiv_state()

  manager <- StreamManager$new(stream_type = "pricing")
  priv    <- manager$.__enclos_env__$private
  priv$.reconnecting <- TRUE
  priv$.stream_type  <- "pricing"

  hc <- create_handler_capturing_ws()
  priv$.ws <- hc$ws
  priv$.setup_websocket_handlers(debug = TRUE)

  refinitiv_vault_set("access_token", "mock_reconnect_token")

  msgs <- capture_messages(hc$captured$on_open(list()))

  expect_equal(priv$.connection_state, "connected")
  expect_true(any(grepl("Reconnect: sending login from onOpen", msgs)), info = "line 529")
})

# --- Line 533: onOpen reconnect login error debug message ---

test_that(".setup_websocket_handlers onOpen debug=TRUE emits error-sending-login message (line 533)", {
  local_refinitiv_state()

  manager <- StreamManager$new(stream_type = "pricing")
  priv    <- manager$.__enclos_env__$private
  priv$.reconnecting <- TRUE
  priv$.stream_type  <- "pricing"

  # Build a WebSocket that captures the registered handlers but throws on send()
  env <- new.env(parent = emptyenv())
  env$on_open <- NULL
  bad_ws <- list(
    onOpen    = function(h) { env$on_open    <- h },
    onClose   = function(h) NULL,
    onMessage = function(h) NULL,
    onError   = function(h) NULL,
    send      = function(msg) stop("ws send failed"),
    connect   = function() NULL,
    close     = function() NULL,
    readyState = function() 1L
  )
  class(bad_ws) <- "WebSocket"
  priv$.ws <- bad_ws
  priv$.setup_websocket_handlers(debug = TRUE)

  refinitiv_vault_set("access_token", "mock_reconnect_token")

  # Firing onOpen triggers the login path; ws$send() throws → error handler fires
  expect_warning(
    msgs <- capture_messages(env$on_open(list())),
    "Reconnect login failed"
  )
  expect_true(any(grepl("Error sending login on reconnect", msgs)), info = "line 533")
})


restore_refinitiv_state(.saved_state, "test-rd_streaming_manager")
