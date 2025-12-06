library(testthat)
library(mockery)

# Unit tests for rd_streaming_debug function

context("rd_streaming_debug")

# Test rd_streaming_debug
test_that("rd_streaming_debug returns debug information", {
  # Create a stream
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  
  # Set up some state
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_id <- 2L
  manager$.__enclos_env__$private$.connection_state <- "connected"
  manager$.__enclos_env__$private$.logged_in <- TRUE
  
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
  
  # Add some data
  stream$.__enclos_env__$private$.latest_data <- list("EUR=" = list(BID = 1.165))
  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.165
  )
  stream$.__enclos_env__$private$.data_history <- history
  
  # Call debug function
  debug_info <- rd_streaming_debug(stream)
  
  expect_type(debug_info, "list")
  expect_true(debug_info$stream_open)
  expect_equal(debug_info$connection_state, "connected")
  expect_true(debug_info$logged_in)
  expect_equal(debug_info$stream_id, 2L)
  expect_equal(debug_info$websocket_ready, 1L)
  expect_equal(debug_info$data_history_rows, 1)
  expect_equal(debug_info$latest_data_count, 1)
})

test_that("rd_streaming_debug with verbose includes detailed info", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  
  # Set up state
  stream$.__enclos_env__$private$.is_open <- TRUE
  stream$.__enclos_env__$private$.stream_id <- 2L
  
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
  
  # Add data
  stream$.__enclos_env__$private$.latest_data <- list("EUR=" = list(BID = 1.165))
  history <- data.table::data.table(
    timestamp = Sys.time(),
    instrument = "EUR=",
    message_type = "Update",
    BID = 1.165
  )
  stream$.__enclos_env__$private$.data_history <- history
  
  # Call with verbose
  debug_info <- rd_streaming_debug(stream, verbose = TRUE)
  
  expect_type(debug_info, "list")
  expect_true("definition" %in% names(debug_info))
  expect_true("latest_data" %in% names(debug_info))
  expect_true("data_history_sample" %in% names(debug_info))
  expect_equal(debug_info$definition$universe, "EUR=")
  expect_equal(debug_info$definition$fields, c("BID", "ASK"))
})

test_that("rd_streaming_debug handles NULL WebSocket", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  
  # Set state but no WebSocket
  stream$.__enclos_env__$private$.is_open <- FALSE
  manager$.__enclos_env__$private$.ws <- NULL
  
  debug_info <- rd_streaming_debug(stream)
  
  expect_null(debug_info$websocket_ready)
})

test_that("rd_streaming_debug handles empty data history", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  
  # No data
  stream$.__enclos_env__$private$.data_history <- data.table::data.table()
  stream$.__enclos_env__$private$.latest_data <- list()
  
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
  
  debug_info <- rd_streaming_debug(stream, verbose = TRUE)
  
  expect_equal(debug_info$data_history_rows, 0)
  expect_null(debug_info$data_history_sample)
})

test_that("rd_streaming_debug errors on non-Stream object", {
  expect_error(
    rd_streaming_debug("not a stream"),
    "stream must be a Stream object"
  )
  
  expect_error(
    rd_streaming_debug(list()),
    "stream must be a Stream object"
  )
})

test_that("rd_streaming_debug shows pending subscriptions", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  
  # Add pending subscription
  manager$.__enclos_env__$private$.pending_subscriptions[["2"]] <- def
  
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
  
  debug_info <- rd_streaming_debug(stream)
  
  expect_equal(debug_info$pending_subscriptions, 1)
})

test_that("rd_streaming_debug shows active streams count", {
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID"))
  manager <- StreamManager$new(stream_type = "pricing")
  stream <- Stream$new(definition = def, manager = manager)
  
  # Add active streams
  manager$.__enclos_env__$private$.streams[["2"]] <- def
  manager$.__enclos_env__$private$.streams[["3"]] <- def
  
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
  
  debug_info <- rd_streaming_debug(stream)
  
  expect_equal(debug_info$active_streams, 2)
})

# Teardown: Clear any pending event loops after each test
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


