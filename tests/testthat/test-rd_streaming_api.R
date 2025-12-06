library(testthat)
library(mockery)

# Unit tests for high-level streaming API

context("Streaming API - rd_get_streaming_data")

# Teardown: Close all streams and stop event loops after each test
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

# Test rd_get_streaming_data
test_that("rd_get_streaming_data creates stream with valid parameters", {
  # Mock dependencies
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  stream <- rd_get_streaming_data(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK")
  )
  
  expect_s3_class(stream, "Stream")
  expect_equal(stream$get_definition()$get_universe(), c("EUR=", "GBP="))
  expect_equal(stream$get_definition()$get_fields(), c("BID", "ASK"))
})

test_that("rd_get_streaming_data validates inputs", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  expect_error(
    rd_get_streaming_data(universe = NULL, fields = c("BID")),
    "universe must be a non-empty character vector"
  )
  
  expect_error(
    rd_get_streaming_data(universe = c("EUR="), fields = NULL),
    "fields must be a non-empty character vector"
  )
})

test_that("rd_get_streaming_data errors when websocket not available", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) FALSE)
  
  expect_error(
    rd_get_streaming_data(universe = "EUR=", fields = c("BID")),
    "Please install 'websocket' package"
  )
})

test_that("rd_get_streaming_data errors when later not available", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg == "websocket") return(TRUE)
    return(FALSE)
  })
  
  expect_error(
    rd_get_streaming_data(universe = "EUR=", fields = c("BID")),
    "Please install 'later' package"
  )
})

test_that("rd_get_streaming_data registers on_refresh callback", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  callback_called <- FALSE
  callback <- function(stream, instrument, fields) {
    callback_called <<- TRUE
  }
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID"),
    on_refresh = callback
  )
  
  # Verify callback is registered
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$refresh), 1)
})

test_that("rd_get_streaming_data registers on_update callback", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  callback_called <- FALSE
  callback <- function(stream, instrument, fields) {
    callback_called <<- TRUE
  }
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID"),
    on_update = callback
  )
  
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$update), 1)
})

test_that("rd_get_streaming_data registers on_error callback", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  callback_called <- FALSE
  callback <- function(stream, error_message) {
    callback_called <<- TRUE
  }
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID"),
    on_error = callback
  )
  
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$error), 1)
})

test_that("rd_get_streaming_data handles NULL callbacks", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID"),
    on_refresh = NULL,
    on_update = NULL,
    on_error = NULL
  )
  
  expect_s3_class(stream, "Stream")
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$refresh), 0)
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$update), 0)
  expect_equal(length(stream$.__enclos_env__$private$.callbacks$error), 0)
})

test_that("rd_get_streaming_data uses pricing stream type by default", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID")
  )
  
  # Verify it's a pricing stream definition
  expect_s3_class(stream$get_definition(), "PricingStreamDefinition")
})

test_that("rd_get_streaming_data errors on unknown stream type", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  expect_error(
    rd_get_streaming_data(
      universe = "EUR=",
      fields = c("BID"),
      stream_type = "unknown"
    ),
    "Stream type unknown not yet implemented"
  )
})

test_that("rd_get_streaming_data accepts custom domain", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID"),
    domain = "MarketPrice"
  )
  
  expect_equal(stream$get_definition()$get_domain(), "MarketPrice")
})

test_that("rd_get_streaming_data accepts parameters", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  params <- list(param1 = "value1", param2 = 123)
  
  stream <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID"),
    parameters = params
  )
  
  expect_equal(stream$get_definition()$get_parameters(), params)
})

test_that("rd_get_streaming_data creates independent streams", {
  stub(rd_get_streaming_data, "requireNamespace", function(pkg, quietly) {
    if (pkg %in% c("websocket", "later")) return(TRUE)
    return(FALSE)
  })
  
  stream1 <- rd_get_streaming_data(
    universe = "EUR=",
    fields = c("BID")
  )
  
  stream2 <- rd_get_streaming_data(
    universe = "GBP=",
    fields = c("ASK")
  )
  
  expect_false(identical(stream1, stream2))
  expect_equal(stream1$get_definition()$get_universe(), "EUR=")
  expect_equal(stream2$get_definition()$get_universe(), "GBP=")
})



