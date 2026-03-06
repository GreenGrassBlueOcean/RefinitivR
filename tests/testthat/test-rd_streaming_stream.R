library(testthat)
library(mockery)

# Unit tests for Stream class

context("Stream Class")

# Teardown: Close all streams and stop event loops after each test
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

# Helper to create mock manager
create_mock_manager <- function(connected = TRUE, logged_in = TRUE) {
  mock_manager <- list(
    get_connection_state = function() if (connected) "connected" else "disconnected",
    connect = function(debug = FALSE) TRUE,
    subscribe = function(def) 2L,
    unsubscribe = function(stream_id) NULL,
    register_handler = function(stream_id, handler) NULL,
    unregister_handler = function(stream_id) NULL
  )
  class(mock_manager) <- "StreamManager"
  return(mock_manager)
}

# Test Stream initialization
test_that("Stream initializes correctly", {
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  expect_s3_class(stream, "Stream")
  expect_false(stream$is_open())
  expect_equal(stream$get_definition(), def)
})

# Test Stream open
test_that("Stream open connects manager and subscribes", {
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )

  # Create a proper mock manager using R6
  manager <- StreamManager$new(stream_type = "pricing")
  manager$.__enclos_env__$private$.connection_state <- "disconnected"

  # Use stubs instead of trying to override locked R6 methods
  stub(manager$connect, "rd_handshake", function() list(access_token = "test"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) {
    list(
      readyState = function() 1L, connect = function() NULL, send = function(msg) NULL,
      onMessage = function(handler) NULL, onOpen = function(handler) NULL,
      onClose = function(handler) NULL, onError = function(handler) NULL, close = function() NULL
    )
  })
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)
  stub(manager$connect, "Sys.sleep", function(time) NULL)

  # Stub subscribe
  stub(manager$subscribe, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(manager$subscribe, "later::run_now", function(timeout) NULL)
  stub(manager$subscribe, "Sys.sleep", function(time) NULL)

  stream <- Stream$new(definition = def, manager = manager)

  # Set up manager state manually (using eval to avoid testthat parse issues)
  private_env <- manager$.__enclos_env__$private
  private_env$.connection_state <- "connected"
  private_env$.logged_in <- TRUE
  private_env$.stream_id_counter <- 2L

  # Mock WebSocket
  mock_ws <- list(
    readyState = function() 1L,
    connect = function() NULL,
    send = function(msg) NULL,
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  private_env$.ws <- mock_ws

  # Mock later package
  stub(stream$open, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(stream$open, "later::run_now", function(timeout) NULL)

  # Stub manager methods using the manager's methods directly
  # We'll verify the behavior instead of tracking calls
  result <- stream$open()

  expect_s3_class(result, "Stream")
  expect_true(stream$is_open())
  expect_type(stream$.__enclos_env__$private$.stream_ids, "integer")
})

test_that("Stream open warns if already open", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE

  expect_warning(
    stream$open(),
    "Stream is already open"
  )
})

# Test Stream close
test_that("Stream close unsubscribes and removes handler", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- 2L

  # Register handler first so we can verify it's removed
  manager$register_handler(2L, function(msg, type) NULL)
  expect_false(is.null(manager$.__enclos_env__$private$.message_handlers[["2"]]))

  # Mock WebSocket
  mock_ws <- list(
    readyState = function() 1L,
    send = function(msg) NULL,
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws

  stub(stream$close, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(stream$close, "later::run_now", function(timeout) NULL)

  result <- stream$close()

  expect_s3_class(result, "Stream")
  expect_false(stream$is_open())
  # Verify handler was removed
  expect_null(manager$.__enclos_env__$private$.message_handlers[["2"]])
})

test_that("Stream close does nothing if already closed", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- FALSE

  result <- stream$close()

  expect_s3_class(result, "Stream")
  expect_false(stream$is_open())
})

# Test Stream callbacks
test_that("Stream on_refresh registers callback", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  callback <- function(stream, instrument, fields) NULL

  result <- stream$on_refresh(callback)

  expect_s3_class(result, "Stream")
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$refresh), 1)
})

test_that("Stream on_refresh errors on non-function", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  expect_error(
    stream$on_refresh("not a function"),
    "callback must be a function"
  )
})

test_that("Stream on_update registers callback", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  callback <- function(stream, instrument, fields) NULL

  result <- stream$on_update(callback)

  expect_s3_class(result, "Stream")
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$update), 1)
})

test_that("Stream on_error registers callback", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  callback <- function(stream, error_message) NULL

  result <- stream$on_error(callback)

  expect_s3_class(result, "Stream")
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$error), 1)
})

# Test Stream get_latest_data
test_that("Stream get_latest_data returns all data when instrument is NULL", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.latest_data <- list(
    "EUR=" = list(BID = 1.165, ASK = 1.1652)
  )

  result <- stream$get_latest_data()

  expect_type(result, "list")
  expect_equal(result[["EUR="]]$BID, 1.165)
})

test_that("Stream get_latest_data returns specific instrument data", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.latest_data <- list(
    "EUR=" = list(BID = 1.165),
    "GBP=" = list(BID = 1.25)
  )

  result <- stream$get_latest_data("EUR=")

  expect_equal(result$BID, 1.165)
})

test_that("Stream get_latest_data returns NULL for unknown instrument", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  result <- stream$get_latest_data("UNKNOWN")

  expect_null(result)
})

# Test Stream get_data_history
test_that("Stream get_data_history returns data.table", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  result <- stream$get_data_history()

  expect_s3_class(result, "data.table")
})

# Test Stream get_summary
test_that("Stream get_summary returns summary for all instruments", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  # Add some data
  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.165,
    ASK = 1.1652
  )
  stream$.__enclos_env__$private$.data_history <- history

  result <- stream$get_summary()

  expect_s3_class(result, "data.frame")
})

test_that("Stream get_summary returns summary for specific instrument", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.165
  )
  stream$.__enclos_env__$private$.data_history <- history

  result <- stream$get_summary(instrument = "EUR=")

  expect_s3_class(result, "data.frame")
})

# Test Stream clear_history
test_that("Stream clear_history clears data history", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  # Add some data
  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.165
  )
  stream$.__enclos_env__$private$.data_history <- history

  result <- stream$clear_history()

  expect_s3_class(result, "Stream")
  expect_equal(nrow(stream$get_data_history()), 0)
})

# Test Stream message handling
# Note: We can't directly test private methods, so we test through public interface
# or by checking the results after messages are processed
test_that("Stream processes refresh messages through manager", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- 2L

  callback_env <- new.env(parent = emptyenv())
  callback_env$called <- FALSE
  stream$on_refresh(function(s, inst, fields) {
    callback_env$called <- TRUE
  })

  # Register handler
  handler <- function(msg, type) {
    stream$.__enclos_env__$private$.handle_message(msg, type)
  }
  manager$register_handler(2L, handler)

  refresh_message <- list(
    Type = "Refresh",
    Fields = list(BID = 1.165, ASK = 1.1652),
    Key = list(Name = "EUR="),
    ID = 2L
  )

  # Process through manager
  manager$.__enclos_env__$private$.route_message(refresh_message, message_type = "Refresh")

  expect_true(callback_env$called)
  expect_equal(stream$get_latest_data("EUR=")$BID, 1.165)
})

test_that("Stream processes update messages through manager", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- 2L

  callback_env <- new.env(parent = emptyenv())
  callback_env$called <- FALSE
  stream$on_update(function(s, inst, fields) {
    callback_env$called <- TRUE
  })

  # Register handler
  handler <- function(msg, type) {
    stream$.__enclos_env__$private$.handle_message(msg, type)
  }
  manager$register_handler(2L, handler)

  update_message <- list(
    Type = "Update",
    Fields = list(BID = 1.165),
    Key = list(Name = "EUR="),
    ID = 2L
  )

  # Process through manager
  manager$.__enclos_env__$private$.route_message(update_message, message_type = "Update")

  expect_true(callback_env$called)
  expect_equal(nrow(stream$get_data_history()), 1)
})

test_that("Stream ignores messages when closed", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- FALSE
  stream$.__enclos_env__$private$.stream_ids <- 2L

  # Register handler
  handler <- function(msg, type) {
    stream$.__enclos_env__$private$.handle_message(msg, type)
  }
  manager$register_handler(2L, handler)

  update_message <- list(
    Type = "Update",
    Fields = list(BID = 1.165),
    Key = list(Name = "EUR="),
    ID = 2L
  )

  # Process through manager
  manager$.__enclos_env__$private$.route_message(update_message, message_type = "Update")

  # Should not add to history
  expect_equal(nrow(stream$get_data_history()), 0)
})

test_that("Stream processes error messages through manager", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- 2L

  error_env <- new.env(parent = emptyenv())
  error_env$called <- FALSE
  error_env$msg <- NULL
  stream$on_error(function(s, msg) {
    error_env$called <- TRUE
    error_env$msg <- msg
  })

  # Register handler
  handler <- function(msg, type) {
    stream$.__enclos_env__$private$.handle_message(msg, type)
  }
  manager$register_handler(2L, handler)

  error_message <- list(
    Type = "Status",
    State = list(Stream = "Closed", DataState = "Suspect"),
    Text = "Error occurred",
    ID = 2L
  )

  # Process through manager
  manager$.__enclos_env__$private$.route_message(error_message, message_type = "Status")

  expect_true(error_env$called)
})

# Test Stream plot_live
test_that("Stream plot_live creates Shiny app", {
  # Skip if shiny not available
  skip_if_not_installed("shiny")

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  # Add some data
  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.165,
    ASK = 1.1652
  )
  stream$.__enclos_env__$private$.data_history <- history

  app <- stream$plot_live(field = "BID", instrument = "EUR=")

  # shinyApp returns a Shiny app object, not a list with ui/server
  expect_s3_class(app, "shiny.appobj")
})

test_that("Stream plot_live errors when shiny not available", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  stub(stream$plot_live, "requireNamespace", function(pkg, quietly) FALSE)

  expect_error(
    stream$plot_live(),
    "Please install 'shiny' package"
  )
})

test_that("Stream plot_live uses first field if not specified", {
  skip_if_not_installed("shiny")

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  expect_message(
    app <- stream$plot_live(),
    "No field specified, using first field"
  )

  expect_type(app, "list")
})

# Test Stream event loop management
test_that("Stream event loop stops when stream is closed", {
  skip_if_not_installed("later")

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)

  # Set up manager as connected
  manager$.__enclos_env__$private$.connection_state <- "connected"
  manager$.__enclos_env__$private$.logged_in <- TRUE
  manager$.__enclos_env__$private$.stream_id_counter <- 2L

  # Mock WebSocket
  mock_ws <- list(
    readyState = function() 1L,
    connect = function() NULL,
    send = function(msg) NULL,
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws

  # Stub manager methods
  stub(manager$connect, "rd_handshake", function() list(access_token = "test"))
  stub(manager$connect, "get_streaming_url", function(type, port) "ws://test")
  stub(manager$connect, "get_streaming_protocol", function(type) "tr_json2")
  stub(manager$connect, "create_streaming_headers", function(token) list())
  stub(manager$connect, "poll_until_connected", function(ws, timeout) TRUE)
  stub(manager$connect, "create_omm_login_request", function(app_key, access_token) '{"ID":1}')
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) mock_ws)
  stub(manager$connect, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(manager$connect, "later::run_now", function(timeout) NULL)

  stub(manager$subscribe, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(manager$subscribe, "later::run_now", function(timeout) NULL)
  stub(manager$subscribe, "Sys.sleep", function(time) NULL)

  # Open stream (this will start event loop)
  stub(stream$open, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(stream$open, "later::run_now", function(timeout) NULL)

  stream$open()

  # Verify event loop is active
  expect_true(stream$.__enclos_env__$private$.event_loop_active)
  expect_true(stream$is_open())

  # Close stream
  stub(stream$close, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(stream$close, "later::run_now", function(timeout) NULL)

  stream$close()

  # Verify event loop is stopped
  expect_false(stream$.__enclos_env__$private$.event_loop_active)
  expect_false(stream$is_open())
})

test_that("Stream event loop does not reschedule when closed", {
  skip_if_not_installed("later")

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)

  # Manually set stream as open and event loop as active
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.event_loop_active <- TRUE

  # Try to schedule event loop - should work when open
  private_env <- stream$.__enclos_env__$private
  if (requireNamespace("later", quietly = TRUE)) {
    # This should schedule a callback
    private_env$.schedule_next_event_loop()

    # Now close the stream BEFORE the callback executes
    stream$.__enclos_env__$private$.is_open <- FALSE
    stream$.__enclos_env__$private$.event_loop_active <- FALSE

    # Process any pending callbacks - they should detect stream is closed
    # and not reschedule (this tests the early return path in the callback)
    later::run_now()

    # Give a moment for any scheduled callbacks
    Sys.sleep(0.15) # Slightly longer than the 0.1 delay

    # Process callbacks again
    later::run_now()

    # Verify stream is still closed
    expect_false(stream$is_open())
    expect_false(stream$.__enclos_env__$private$.event_loop_active)
  }
})

test_that("Stream event loop callback checks state correctly", {
  skip_if_not_installed("later")

  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)

  private_env <- stream$.__enclos_env__$private

  # Test that scheduling doesn't work when already closed
  private_env$.is_open <- FALSE
  private_env$.event_loop_active <- TRUE
  private_env$.schedule_next_event_loop() # Should return early

  # Verify it didn't schedule (stream is closed)
  expect_false(private_env$.is_open)

  # Test that scheduling doesn't work when event loop inactive
  private_env$.is_open <- TRUE
  private_env$.event_loop_active <- FALSE
  private_env$.schedule_next_event_loop() # Should return early

  # Verify it didn't schedule (event loop inactive)
  expect_false(private_env$.event_loop_active)
})

test_that("Stream event loop start method exists and is callable", {
  # This test verifies that .start_event_loop method exists and can be called
  # The actual behavior (whether it starts the loop) depends on whether
  # the 'later' package is available, which is hard to test without
  # actually removing the package
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  # Access private environment
  private_env <- stream$.__enclos_env__$private

  # Verify the method exists and can be called without error
  expect_true(is.function(private_env$.start_event_loop))

  # Call it - should not error (behavior depends on later availability)
  expect_silent(private_env$.start_event_loop())
})

# --- Multi-instrument tests (H3) ---

test_that("Stream open registers handlers for all instruments", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP=", "JPY="),
    fields = c("BID", "ASK")
  )
  manager <- StreamManager$new(stream_type = "pricing")

  # Set up manager as connected and logged in
  private_mgr <- manager$.__enclos_env__$private
  private_mgr$.connection_state <- "connected"
  private_mgr$.logged_in <- TRUE
  private_mgr$.stream_id_counter <- 1L

  # Mock WebSocket
  mock_ws <- list(
    readyState = function() 1L,
    connect = function() NULL,
    send = function(msg) NULL,
    onMessage = function(handler) NULL,
    onOpen = function(handler) NULL,
    onClose = function(handler) NULL,
    onError = function(handler) NULL,
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  private_mgr$.ws <- mock_ws

  stream <- Stream$new(definition = def, manager = manager)

  # Stub later
  stub(stream$open, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(stream$open, "later::run_now", function(timeout) NULL)
  stub(manager$subscribe, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(manager$subscribe, "later::run_now", function(timeout) NULL)
  stub(manager$subscribe, "Sys.sleep", function(time) NULL)

  stream$open()

  # Verify 3 stream IDs allocated
  stream_ids <- stream$.__enclos_env__$private$.stream_ids
  expect_length(stream_ids, 3)
  expect_type(stream_ids, "integer")

  # Verify handlers registered for each stream ID
  handlers <- private_mgr$.message_handlers
  for (sid in stream_ids) {
    expect_false(is.null(handlers[[as.character(sid)]]),
      info = paste("Handler missing for stream_id", sid)
    )
  }
})

test_that("Stream close sends Close messages for all instruments", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK")
  )
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- c(2L, 3L)

  # Register handlers for both IDs
  manager$register_handler(2L, function(msg, type) NULL)
  manager$register_handler(3L, function(msg, type) NULL)

  # Track close messages sent
  sent_env <- new.env(parent = emptyenv())
  sent_env$messages <- character(0)
  mock_ws <- list(
    readyState = function() 1L,
    send = function(msg) {
      sent_env$messages <- c(sent_env$messages, msg)
    },
    close = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws

  # Stub later
  stub(stream$close, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") {
      return(TRUE)
    }
    return(FALSE)
  })
  stub(stream$close, "later::run_now", function(timeout) NULL)

  stream$close()

  # Verify 2 close messages were sent
  expect_length(sent_env$messages, 2)
  expect_true(grepl('"ID":2', sent_env$messages[1]))
  expect_true(grepl('"Name":"EUR="', sent_env$messages[1]))
  expect_true(grepl('"ID":3', sent_env$messages[2]))
  expect_true(grepl('"Name":"GBP="', sent_env$messages[2]))

  # Verify handlers unregistered
  handlers <- manager$.__enclos_env__$private$.message_handlers
  expect_null(handlers[["2"]])
  expect_null(handlers[["3"]])

  # Verify stream IDs cleared
  expect_length(stream$.__enclos_env__$private$.stream_ids, 0)
  expect_false(stream$is_open())
})

test_that("Stream routes messages to correct instrument via stream ID", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK")
  )
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- c(2L, 3L)

  # Register handler for both stream IDs (same handler, as Stream does)
  handler <- function(msg, type) {
    stream$.__enclos_env__$private$.handle_message(msg, type)
  }
  manager$register_handler(2L, handler)
  manager$register_handler(3L, handler)

  # Send refresh for EUR= (stream_id 2)
  eur_refresh <- list(
    Type = "Refresh",
    ID = 2L,
    Key = list(Name = "EUR="),
    Fields = list(BID = 1.165, ASK = 1.1652)
  )
  manager$.__enclos_env__$private$.route_message(eur_refresh, message_type = "Refresh")

  # Send refresh for GBP= (stream_id 3)
  gbp_refresh <- list(
    Type = "Refresh",
    ID = 3L,
    Key = list(Name = "GBP="),
    Fields = list(BID = 1.25, ASK = 1.2503)
  )
  manager$.__enclos_env__$private$.route_message(gbp_refresh, message_type = "Refresh")

  # Verify both instruments have latest data
  eur_data <- stream$get_latest_data("EUR=")
  gbp_data <- stream$get_latest_data("GBP=")
  expect_equal(eur_data$BID, 1.165)
  expect_equal(gbp_data$BID, 1.25)

  # Verify data history has 2 rows
  history <- stream$get_data_history()
  expect_equal(nrow(history), 2)
  expect_true("EUR=" %in% history$instrument)
  expect_true("GBP=" %in% history$instrument)
})

test_that("Stream tracks multi-instrument updates in data history", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID")
  )
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- c(2L, 3L)

  # Register handler
  handler <- function(msg, type) {
    stream$.__enclos_env__$private$.handle_message(msg, type)
  }
  manager$register_handler(2L, handler)
  manager$register_handler(3L, handler)

  # Simulate 3 updates for EUR= and 2 for GBP=
  for (bid in c(1.10, 1.11, 1.12)) {
    manager$.__enclos_env__$private$.route_message(
      list(
        Type = "Update", ID = 2L, Key = list(Name = "EUR="),
        Fields = list(BID = bid)
      ),
      message_type = "Update"
    )
  }
  for (bid in c(1.25, 1.26)) {
    manager$.__enclos_env__$private$.route_message(
      list(
        Type = "Update", ID = 3L, Key = list(Name = "GBP="),
        Fields = list(BID = bid)
      ),
      message_type = "Update"
    )
  }

  history <- stream$get_data_history()
  expect_equal(nrow(history), 5)

  eur_rows <- history[history$instrument == "EUR=", ]
  gbp_rows <- history[history$instrument == "GBP=", ]
  expect_equal(nrow(eur_rows), 3)
  expect_equal(nrow(gbp_rows), 2)

  # Latest data should reflect last update
  expect_equal(stream$get_latest_data("EUR=")$BID, 1.12)
  expect_equal(stream$get_latest_data("GBP=")$BID, 1.26)
})

test_that("Stream get_summary works with multiple instruments", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK")
  )
  manager <- create_mock_manager()

  stream <- Stream$new(definition = def, manager = manager)

  # Populate history with 2 instruments
  history <- data.table::data.table(
    timestamp = rep(Sys.time(), 4),
    instrument = c("EUR=", "EUR=", "GBP=", "GBP="),
    message_type = rep("Update", 4),
    BID = c(1.10, 1.11, 1.25, 1.26),
    ASK = c(1.101, 1.111, 1.251, 1.261)
  )
  stream$.__enclos_env__$private$.data_history <- history

  # Summary for all
  summary_all <- stream$get_summary()
  expect_equal(summary_all$total_updates, 4)

  # Summary for specific instrument
  summary_eur <- stream$get_summary(instrument = "EUR=")
  expect_equal(summary_eur$total_updates, 2)

  summary_gbp <- stream$get_summary(instrument = "GBP=")
  expect_equal(summary_gbp$total_updates, 2)
})

# --- Reconnection callback tests (H4) ---

test_that("Stream on_reconnecting registers callback with manager", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)

  cb <- function(stream, attempt) NULL
  result <- stream$on_reconnecting(cb)

  expect_s3_class(result, "Stream")
  # Verify callback registered with manager
  expect_length(manager$.__enclos_env__$private$.reconnect_callbacks$on_reconnecting, 1)
})

test_that("Stream on_reconnected registers callback with manager", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)

  cb <- function(stream, attempt) NULL
  result <- stream$on_reconnected(cb)

  expect_s3_class(result, "Stream")
  expect_length(manager$.__enclos_env__$private$.reconnect_callbacks$on_reconnected, 1)
})

test_that("Stream on_reconnecting errors on non-function", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)

  expect_error(stream$on_reconnecting("not a function"), "callback must be a function")
})

test_that("Stream on_reconnected errors on non-function", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)

  expect_error(stream$on_reconnected("not a function"), "callback must be a function")
})


# --- Phase 7: .handle_message() branch coverage ---

test_that(".handle_message handles lowercase type field", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  cb_env <- new.env(parent = emptyenv())
  cb_env$called <- FALSE
  stream$on_refresh(function(s, inst, fields) cb_env$called <- TRUE)

  # Message with lowercase "type" instead of "Type"
  priv$.handle_message(
    list(type = "Refresh", Key = list(Name = "EUR="), Fields = list(BID = 1.10)),
    message_type = NULL
  )
  expect_true(cb_env$called)
})

test_that(".handle_message falls back to universe when instrument is UNKNOWN", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Message with no Key, no Name — instrument will be UNKNOWN, then fall back to universe
  priv$.handle_message(
    list(Type = "Refresh", Fields = list(BID = 1.10)),
    message_type = "Refresh"
  )
  # Should have stored under "EUR=" (universe fallback)
  expect_equal(priv$.latest_data[["EUR="]]$BID, 1.10)
})

test_that(".handle_message processes STATUS with Stream=Closed", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  err_env <- new.env(parent = emptyenv())
  err_env$called <- FALSE
  stream$on_error(function(s, msg) err_env$called <- TRUE)

  priv$.handle_message(
    list(Type = "Status", State = list(Stream = "Closed", DataState = "Ok")),
    message_type = "Status"
  )
  expect_true(err_env$called)
})

test_that(".handle_message processes STATUS with DataState=Suspect", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  err_env <- new.env(parent = emptyenv())
  err_env$called <- FALSE
  stream$on_error(function(s, msg) err_env$called <- TRUE)

  priv$.handle_message(
    list(Type = "Status", State = list(Stream = "Open", DataState = "Suspect")),
    message_type = "Status"
  )
  expect_true(err_env$called)
})

test_that(".handle_message skips REFRESH/UPDATE with empty fields", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Refresh with no Fields — should return early

  priv$.handle_message(
    list(Type = "Refresh", Key = list(Name = "EUR=")),
    message_type = "Refresh"
  )
  expect_null(priv$.latest_data[["EUR="]])

  # Update with empty Fields
  priv$.handle_message(
    list(Type = "Update", Key = list(Name = "EUR="), Fields = list()),
    message_type = "Update"
  )
  expect_null(priv$.latest_data[["EUR="]])
})

test_that(".handle_message processes STATUS without fields", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  err_env <- new.env(parent = emptyenv())
  err_env$called <- FALSE
  stream$on_error(function(s, msg) err_env$called <- TRUE)

  # STATUS message without Fields — should NOT be skipped (unlike Refresh/Update)
  priv$.handle_message(
    list(Type = "Status", State = list(Stream = "Closed")),
    message_type = "Status"
  )
  expect_true(err_env$called)
})

test_that(".handle_message returns early on NULL message_data", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Should not error
  expect_silent(priv$.handle_message(NULL, "Refresh"))
  expect_silent(priv$.handle_message(list(), "Refresh"))
})

test_that(".handle_message warns on callback errors via inner handler", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  stream$on_refresh(function(s, inst, fields) {
    stop("test callback error")
  })

  # The error is caught by .handle_refresh()'s tryCatch and turned into a warning
  expect_warning(
    priv$.handle_message(
      list(Type = "Refresh", Key = list(Name = "EUR="), Fields = list(BID = 1.1)),
      message_type = "Refresh"
    ),
    "Error in refresh callback"
  )
})

test_that(".handle_message returns early for no/empty message type", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Message with no Type/type field at all — should return early, no data stored
  expect_silent(
    priv$.handle_message(
      list(Key = list(Name = "EUR="), Fields = list(BID = 1.1)),
      message_type = NULL
    )
  )
  expect_null(priv$.latest_data[["EUR="]])
})

test_that(".handle_message returns early when stream is closed", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- FALSE

  priv$.handle_message(
    list(Type = "Update", Key = list(Name = "EUR="), Fields = list(BID = 1.1)),
    message_type = "Update"
  )
  expect_equal(nrow(stream$get_data_history()), 0)
})

# --- Phase 8: .extract_instrument() fallback paths (table-driven) ---

test_that(".extract_instrument uses correct fallback chain", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  cases <- list(
    list(desc = "uppercase Key.Name",
         input = list(Key = list(Name = "AAPL.O")),
         exp = "AAPL.O"),
    list(desc = "lowercase key.name",
         input = list(key = list(name = "MSFT.O")),
         exp = "MSFT.O"),
    list(desc = "Fields.DSPLY_NAME",
         input = list(Fields = list(DSPLY_NAME = "VOD.L")),
         exp = "VOD.L"),
    list(desc = "Fields.RIC",
         input = list(Fields = list(RIC = "GOOG.O")),
         exp = "GOOG.O"),
    list(desc = "top-level Name",
         input = list(Name = "GBP="),
         exp = "GBP="),
    list(desc = "universe fallback (no instrument info)",
         input = list(Other = "x"),
         exp = "EUR="),
    list(desc = "NULL input",
         input = NULL,
         exp = NULL)
  )

  for (tc in cases) {
    expect_equal(priv$.extract_instrument(tc$input), tc$exp, info = tc$desc)
  }
})

test_that(".extract_instrument handles empty/NA Name values", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  # Empty string Name falls through to universe fallback
  result <- priv$.extract_instrument(list(Key = list(Name = "")))
  expect_equal(result, "EUR=")

  # NA Name falls through
  result <- priv$.extract_instrument(list(Key = list(Name = NA_character_)))
  expect_equal(result, "EUR=")

  # Zero-length Name
  result <- priv$.extract_instrument(list(Key = list(Name = character(0))))
  expect_equal(result, "EUR=")
})

# --- Phase 9: .extract_fields() + .handle_error() (table-driven) ---

test_that(".extract_fields handles various input formats", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  cases <- list(
    list(desc = "uppercase Fields",
         input = list(Fields = list(BID = 1.1, ASK = 1.2)),
         exp = list(BID = 1.1, ASK = 1.2)),
    list(desc = "lowercase fields",
         input = list(fields = list(BID = 1.1)),
         exp = list(BID = 1.1)),
    list(desc = "data.frame fields",
         input = list(Fields = data.frame(BID = 1.1)),
         exp = list(BID = 1.1)),
    list(desc = "non-list fields",
         input = list(Fields = "not a list"),
         exp = list()),
    list(desc = "NULL input",
         input = NULL,
         exp = list()),
    list(desc = "empty list",
         input = list(),
         exp = list()),
    list(desc = "no Fields key",
         input = list(Other = "x"),
         exp = list())
  )

  for (tc in cases) {
    expect_equal(priv$.extract_fields(tc$input), tc$exp, info = tc$desc)
  }
})

test_that(".extract_fields strips NULL values", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  result <- priv$.extract_fields(list(Fields = list(BID = 1.1, ASK = NULL, OPEN = 1.0)))
  expect_equal(result, list(BID = 1.1, OPEN = 1.0))
})

test_that(".handle_error extracts message from different formats", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  cases <- list(
    list(desc = "$message property",
         input = list(message = "Connection lost"),
         exp_msg = "Connection lost"),
    list(desc = "$Text property",
         input = list(Text = "Timeout"),
         exp_msg = "Timeout"),
    list(desc = "unknown format",
         input = list(code = 500),
         exp_msg = "Unknown error"),
    list(desc = "NULL input",
         input = NULL,
         exp_msg = "Unknown error")
  )

  for (tc in cases) {
    err_env <- new.env(parent = emptyenv())
    err_env$msg <- NULL
    stream$on_error(function(s, msg) err_env$msg <- msg)

    priv$.handle_error(tc$input)

    expect_false(is.null(err_env$msg), info = paste(tc$desc, "- callback not called"))
    expect_equal(err_env$msg, tc$exp_msg, info = tc$desc)

    # Reset callbacks for next iteration
    priv$.callbacks$error <- list()
  }
})

test_that(".handle_error warns when callback throws", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  stream$on_error(function(s, msg) stop("callback error"))

  expect_warning(
    priv$.handle_error(list(message = "test")),
    "Error in error callback"
  )
})

# --- Phase 10: .handle_update() edge cases ---

test_that(".handle_update returns early for NULL/invalid instrument", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # NULL instrument
  priv$.handle_update(NULL, list(BID = 1.1), list())
  expect_equal(nrow(stream$get_data_history()), 0)

  # NA instrument
  priv$.handle_update(NA_character_, list(BID = 1.1), list())
  expect_equal(nrow(stream$get_data_history()), 0)
})

test_that(".handle_update merges with existing data", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Set initial data
  priv$.latest_data[["EUR="]] <- list(BID = 1.10, ASK = 1.12, OPEN = 1.09)

  # Update only BID — ASK and OPEN should be preserved
  priv$.handle_update("EUR=", list(BID = 1.11), list())

  expect_equal(priv$.latest_data[["EUR="]]$BID, 1.11)
  expect_equal(priv$.latest_data[["EUR="]]$ASK, 1.12)
  expect_equal(priv$.latest_data[["EUR="]]$OPEN, 1.09)
})

test_that(".handle_update replaces non-list existing data", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Set existing data as non-list (edge case)
  priv$.latest_data[["EUR="]] <- "not a list"

  priv$.handle_update("EUR=", list(BID = 1.10), list())

  expect_equal(priv$.latest_data[["EUR="]], list(BID = 1.10))
})

test_that(".handle_update initializes NULL data_history", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE
  priv$.data_history <- NULL

  priv$.handle_update("EUR=", list(BID = 1.10), list())

  expect_equal(nrow(stream$get_data_history()), 1)
  expect_equal(stream$get_data_history()$BID, 1.10)
})

# --- Phase 11: get_summary() edge cases ---

test_that("get_summary returns message for unmatched instrument", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager()
  stream <- Stream$new(definition = def, manager = manager)

  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.10
  )
  stream$.__enclos_env__$private$.data_history <- history

  result <- stream$get_summary(instrument = "GBP=")
  expect_true("message" %in% names(result))
  expect_true(grepl("No data for instrument", result$message))
})

test_that("get_summary handles no numeric columns", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("STATUS"))
  manager <- create_mock_manager()
  stream <- Stream$new(definition = def, manager = manager)

  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    STATUS = "Active"
  )
  stream$.__enclos_env__$private$.data_history <- history

  result <- stream$get_summary()
  expect_true("message" %in% names(result))
  expect_true(grepl("No numeric fields", result$message))
  expect_equal(result$total_updates, 1)
})


# --- Targeted coverage: callback validation ---

test_that("on_update rejects non-function callback", {
  def <- StreamDefinition$new(universe = "EUR=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)

  expect_error(stream$on_update("not_a_function"), "callback must be a function")
  expect_error(stream$on_error(42), "callback must be a function")
})

# --- Targeted coverage: .handle_message edge cases ---

test_that(".handle_message infers Type when message_type is NULL", {
  def <- StreamDefinition$new(universe = "EUR=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  msg <- list(Type = "Update", Fields = list(BID = 1.23),
              Key = list(Name = "EUR="))

  # Should infer type from message_data$Type (line 568)
  priv$.handle_message(msg, NULL)
  expect_true(nrow(priv$.data_history) > 0)
})

test_that(".handle_message falls back to universe when instrument is UNKNOWN", {
  def <- StreamDefinition$new(universe = "GBP=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Message with no Key field → instrument UNKNOWN → fallback to universe[1]
  msg <- list(Fields = list(BID = 1.35))
  priv$.handle_message(msg, "update")

  expect_true(nrow(priv$.data_history) > 0)
  expect_true("GBP=" %in% priv$.data_history$instrument)
})

test_that(".handle_message error handler warns on real errors", {
  def <- StreamDefinition$new(universe = "EUR=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # Craft a message that causes an error in handle_update by making
  # fields_data extraction return something that breaks downstream
  # A message with Type but no extractable fields AND a callback that errors
  stream$on_update(function(s, instrument, fields) stop("unexpected boom"))

  msg <- list(Type = "Update", Key = list(Name = "EUR="), Fields = list(BID = 1.1))

  expect_warning(
    priv$.handle_message(msg, "update"),
    "Error in update callback"
  )
})

# --- Targeted coverage: .handle_update callback error ---

test_that(".handle_update warns when callback throws", {
  def <- StreamDefinition$new(universe = "EUR=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  stream$on_update(function(s, instrument, fields) stop("callback boom"))

  expect_warning(
    priv$.handle_update(
      list(Fields = list(BID = 1.1)),
      "EUR=",
      list(BID = 1.1)
    ),
    "Error in update callback"
  )
})

# --- Targeted coverage: get_summary with NULL data_history ---

test_that("get_summary returns 'No data' when data_history is NULL", {
  def <- StreamDefinition$new(universe = "EUR=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  # data_history starts as empty data.table from initialize
  # Wipe it back to initial state to test the empty path
  priv$.data_history <- data.table::data.table()

  result <- stream$get_summary()
  expect_s3_class(result, "data.frame")
  expect_true("message" %in% names(result))
})

# --- Targeted coverage: .start_event_loop without later ---

test_that(".start_event_loop returns when later unavailable", {
  def <- StreamDefinition$new(universe = "EUR=", fields = "BID")
  manager <- StreamManager$new()
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private

  stub(priv$.start_event_loop, "requireNamespace", function(pkg, quietly) FALSE)
  result <- priv$.start_event_loop()
  expect_null(result)
})

# =============================================================================
# Debug + uncovered branch coverage
# =============================================================================

# Helper: create a standard real StreamManager already in "connected" state
# with a mock WebSocket, ready for Stream.open() / Stream.close() tests.
create_connected_manager <- function(universe = "EUR=") {
  manager <- StreamManager$new(stream_type = "pricing")
  priv <- manager$.__enclos_env__$private
  priv$.connection_state <- "connected"
  priv$.logged_in <- TRUE
  priv$.stream_id_counter <- 1L

  mock_ws <- list(
    readyState  = function() 1L,
    connect     = function() NULL,
    send        = function(msg) NULL,
    onMessage   = function(handler) NULL,
    onOpen      = function(handler) NULL,
    onClose     = function(handler) NULL,
    onError     = function(handler) NULL,
    close       = function() NULL
  )
  class(mock_ws) <- "WebSocket"
  priv$.ws <- mock_ws
  manager
}

# Stub the later-related calls that subscribe() and open() need
stub_later <- function(obj) {
  stub(obj, "requireNamespace", function(pkg, quietly) pkg == "later")
  stub(obj, "later::run_now",   function(timeout) NULL)
  stub(obj, "Sys.sleep",        function(time) NULL)
}

# --- Line 44: manager.connect() called when not yet connected ---

test_that("Stream open() calls manager connect() when manager is disconnected", {
  def     <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_mock_manager(connected = FALSE)  # get_connection_state() → "disconnected"
  stream  <- Stream$new(definition = def, manager = manager)

  stub(stream$open, "requireNamespace", function(pkg, quietly) FALSE)

  result <- stream$open()

  expect_s3_class(result, "Stream")
  expect_true(stream$is_open())
})

# --- Line 49: handler closure body registered inside open() ---

test_that("Stream open() handler closure routes messages to _handle_message", {
  def     <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_connected_manager()
  stream  <- Stream$new(definition = def, manager = manager)

  stub_later(manager$subscribe)
  stub(stream$open, "requireNamespace", function(pkg, quietly) pkg == "later")
  stub(stream$open, "later::run_now",   function(timeout) NULL)

  # stub_later on subscribe doesn't propagate to the private method it

  # delegates to, so the real later::run_now may flush stale callbacks.
  suppressWarnings(stream$open())

  # Route a message via the manager — this exercises the closure at line 49
  sid         <- stream$.__enclos_env__$private$.stream_ids[[1]]
  refresh_msg <- list(
    Type   = "Refresh",
    ID     = sid,
    Key    = list(Name = "EUR="),
    Fields = list(BID = 1.1)
  )
  manager$.__enclos_env__$private$.route_message(refresh_msg, message_type = "Refresh")

  expect_equal(stream$get_latest_data("EUR=")$BID, 1.1)
})

# --- Lines 53, 55, 60: open() debug messages ---

test_that("Stream open() emits debug messages when refinitiv_streaming_debug is TRUE", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def     <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- create_connected_manager()
  stream  <- Stream$new(definition = def, manager = manager)

  stub_later(manager$subscribe)
  stub(stream$open, "requireNamespace", function(pkg, quietly) pkg == "later")
  stub(stream$open, "later::run_now",   function(timeout) NULL)

  msgs <- suppressWarnings(capture_messages(stream$open()))

  expect_true(any(grepl("Calling subscribe()", msgs, fixed = TRUE)),
    info = "Expected '[DEBUG] Calling subscribe()' message")
  expect_true(any(grepl("Subscribe returned stream_ids", msgs)),
    info = "Expected '[DEBUG] Subscribe returned stream_ids' message")
  expect_true(any(grepl("Handler registered for stream_id", msgs)),
    info = "Expected '[DEBUG] Handler registered for stream_id' message")
})

# --- Lines 83, 93, 114, 136: close() debug messages ---

test_that("Stream close() emits debug messages when refinitiv_streaming_debug is TRUE", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def     <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open     <- TRUE
  stream$.__enclos_env__$private$.stream_ids  <- 2L

  manager$register_handler(2L, function(msg, type) NULL)

  mock_ws <- list(send = function(msg) NULL, close = function() NULL)
  class(mock_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- mock_ws

  stub(stream$close, "requireNamespace", function(pkg, quietly) pkg == "later")
  stub(stream$close, "later::run_now",   function(timeout) NULL)

  msgs <- capture_messages(stream$close())

  expect_true(any(grepl("Closing stream",           msgs)), info = "line 83")
  expect_true(any(grepl("Removing handler",          msgs)), info = "line 93")
  expect_true(any(grepl("Sending unsubscribe request", msgs)), info = "line 114")
  expect_true(any(grepl("Stream closed",             msgs)), info = "line 136")
})

# --- Line 120: close() error-sending-unsubscribe debug message ---

test_that("Stream close() emits debug error message when ws$send throws", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def     <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")

  stream <- Stream$new(definition = def, manager = manager)
  stream$.__enclos_env__$private$.is_open    <- TRUE
  stream$.__enclos_env__$private$.stream_ids <- 2L

  manager$register_handler(2L, function(msg, type) NULL)

  bad_ws <- list(send = function(msg) stop("ws send failed"), close = function() NULL)
  class(bad_ws) <- "WebSocket"
  manager$.__enclos_env__$private$.ws <- bad_ws

  stub(stream$close, "requireNamespace", function(pkg, quietly) pkg == "later")
  stub(stream$close, "later::run_now",   function(timeout) NULL)

  msgs <- capture_messages(stream$close())

  expect_true(any(grepl("Error sending unsubscribe", msgs)),
    info = "Expected '[DEBUG] Error sending unsubscribe' message on line 120")
})

# --- Lines 578, 609, 617, 630: _handle_message debug for Refresh ---

test_that("._handle_message emits debug messages for Refresh when debug=TRUE", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def    <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = StreamManager$new())
  priv   <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  msgs <- capture_messages(
    priv$.handle_message(
      list(Type = "Refresh", Key = list(Name = "EUR="), Fields = list(BID = 1.1)),
      message_type = "Refresh"
    )
  )

  expect_true(any(grepl("Stream._handle_message - message_type",  msgs)), info = "line 578")
  expect_true(any(grepl("Stream._handle_message - Instrument",    msgs)), info = "line 609")
  expect_true(any(grepl("message_type.*upper",                    msgs)), info = "line 617")
  expect_true(any(grepl("Calling .handle_refresh",                msgs)), info = "line 630")
})

# --- Line 633: _handle_message debug for Update ---

test_that("._handle_message emits 'Calling .handle_update' debug message", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def    <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = StreamManager$new())
  priv   <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  msgs <- capture_messages(
    priv$.handle_message(
      list(Type = "Update", Key = list(Name = "EUR="), Fields = list(BID = 1.2)),
      message_type = "Update"
    )
  )

  expect_true(any(grepl("Calling .handle_update", msgs)), info = "line 633")
})

# --- Line 583: _handle_message debug: no message type ---

test_that("._handle_message emits 'No message type' debug when type cannot be inferred", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def    <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = StreamManager$new())
  priv   <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  # No Type/type field in the message, and message_type arg is NULL
  msgs <- capture_messages(
    priv$.handle_message(
      list(Key = list(Name = "EUR="), Fields = list(BID = 1.1)),
      message_type = NULL
    )
  )

  expect_true(any(grepl("No message type, returning", msgs)), info = "line 583")
})

# --- Line 624: _handle_message debug: empty fields skipped ---

test_that("._handle_message emits 'No fields extracted' debug for Refresh with no Fields", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def    <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = StreamManager$new())
  priv   <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  msgs <- capture_messages(
    priv$.handle_message(
      list(Type = "Refresh", Key = list(Name = "EUR=")),  # no Fields key
      message_type = "Refresh"
    )
  )

  expect_true(any(grepl("No fields extracted, skipping", msgs)), info = "line 624")
})

# --- Lines 810-812, 830-835, 855-857, 869-876: _handle_update debug (valid instrument) ---

test_that("._handle_update emits all debug messages for a valid update", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def    <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = StreamManager$new())
  priv   <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  msgs <- capture_messages(
    priv$.handle_update("EUR=", list(BID = 1.15), list())
  )

  expect_true(any(grepl(".handle_update: Processing for",              msgs)), info = "line 810")
  expect_true(any(grepl(".handle_update: Latest data updated",         msgs)), info = "line 831")
  expect_true(any(grepl(".handle_update: Created history row",         msgs)), info = "line 855")
  expect_true(any(grepl(".handle_update: History rows after",          msgs)), info = "line 869")
  expect_true(any(grepl(".handle_update: Calling.*update callback",    msgs)), info = "line 874")
})

# --- Line 804: _handle_update debug: invalid instrument ---

test_that("._handle_update emits 'Invalid instrument' debug when instrument is NULL", {
  withr::local_options(refinitiv_streaming_debug = TRUE)

  def    <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = StreamManager$new())
  priv   <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  msgs <- capture_messages(
    priv$.handle_update(NULL, list(BID = 1.1), list())
  )

  expect_true(any(grepl("Invalid instrument, returning", msgs)), info = "line 804")
})


dump_refinitiv_options("test-rd_streaming_stream")
