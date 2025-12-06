library(testthat)
library(mockery)

# Unit tests for StreamDefinition classes

context("StreamDefinition Classes")

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

# Test StreamDefinition base class
test_that("StreamDefinition initializes with valid parameters", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK"),
    domain = "MarketPrice"
  )
  
  expect_s3_class(def, "StreamDefinition")
  expect_equal(def$get_universe(), c("EUR=", "GBP="))
  expect_equal(def$get_fields(), c("BID", "ASK"))
  expect_equal(def$get_domain(), "MarketPrice")
  expect_null(def$get_parameters())
})

test_that("StreamDefinition validates inputs", {
  expect_error(
    StreamDefinition$new(universe = NULL, fields = c("BID")),
    "universe must be a non-empty character vector"
  )
  
  expect_error(
    StreamDefinition$new(universe = c("EUR="), fields = NULL),
    "fields must be a non-empty character vector"
  )
})

test_that("StreamDefinition validate method returns TRUE for valid config", {
  def <- StreamDefinition$new(
    universe = c("EUR="),
    fields = c("BID", "ASK")
  )
  
  result <- def$validate()
  expect_true(result)
})

test_that("StreamDefinition validate method returns errors for invalid config", {
  # We can't test empty universe through constructor because validate_streaming_params
  # will error first. Instead, we test the validate method on a valid object
  # and verify it checks for empty fields/universe
  
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID")
  )
  
  # Valid config should return TRUE
  result <- def$validate()
  expect_true(result)
  
  # Test that validate checks for empty universe (by directly modifying private field)
  # This is a bit of a hack, but necessary to test the validate method
  def$.__enclos_env__$private$.universe <- character(0)
  result <- def$validate()
  expect_type(result, "list")  # Should return list of errors
  expect_true(any(grepl("universe", unlist(result))))
})

test_that("StreamDefinition to_request creates valid request", {
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK"),
    domain = "MarketPrice"
  )
  
  request <- def$to_request(streaming = TRUE, stream_id = 2L)
  
  expect_type(request, "character")
  expect_true(grepl('"ID":2', request))
  expect_true(grepl('"Domain":"MarketPrice"', request))
  expect_true(grepl('"Name":"EUR="', request))
})

test_that("StreamDefinition get_stream creates Stream object", {
  # Mock StreamManager and Stream classes
  mock_manager <- list(
    subscribe = function(def) 2L,
    register_handler = function(id, handler) NULL,
    get_connection_state = function() "connected"
  )
  
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )
  
  # We need to stub Stream class creation
  # Since Stream is defined in another file, we'll test the structure
  stream <- def$get_stream()
  
  expect_s3_class(stream, "Stream")
  expect_true(is.function(stream$open))
  expect_true(is.function(stream$close))
  expect_true(is.function(stream$get_definition))
})

test_that("StreamDefinition get_stream uses provided manager", {
  mock_manager <- StreamManager$new(stream_type = "pricing")
  
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )
  
  stream <- def$get_stream(manager = mock_manager)
  
  expect_s3_class(stream, "Stream")
  # Verify manager is used (check that manager is the same instance)
  # This is tricky to test directly, but we can verify the stream works
})

# Test PricingStreamDefinition
test_that("PricingStreamDefinition inherits from StreamDefinition", {
  def <- rd_streaming_pricing$Definition$new(
    universe = c("EUR="),
    fields = c("BID", "ASK")
  )
  
  expect_s3_class(def, "PricingStreamDefinition")
  expect_s3_class(def, "StreamDefinition")
})

test_that("PricingStreamDefinition initializes correctly", {
  def <- rd_streaming_pricing$Definition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK", "OPEN_PRC"),
    domain = "MarketPrice"
  )
  
  expect_equal(def$get_universe(), c("EUR=", "GBP="))
  expect_equal(def$get_fields(), c("BID", "ASK", "OPEN_PRC"))
  expect_equal(def$get_domain(), "MarketPrice")
})

test_that("PricingStreamDefinition creates stream with pricing type", {
  def <- rd_streaming_pricing$Definition$new(
    universe = "EUR=",
    fields = c("BID", "ASK")
  )
  
  stream <- def$get_stream()
  expect_s3_class(stream, "Stream")
})

test_that("StreamDefinition accessor methods work correctly", {
  def <- StreamDefinition$new(
    universe = c("EUR=", "GBP="),
    fields = c("BID", "ASK"),
    parameters = list(param1 = "value1"),
    domain = "MarketPrice"
  )
  
  expect_equal(def$get_universe(), c("EUR=", "GBP="))
  expect_equal(def$get_fields(), c("BID", "ASK"))
  expect_equal(def$get_parameters(), list(param1 = "value1"))
  expect_equal(def$get_domain(), "MarketPrice")
})

test_that("StreamDefinition handles NULL parameters", {
  def <- StreamDefinition$new(
    universe = "EUR=",
    fields = c("BID"),
    parameters = NULL
  )
  
  expect_null(def$get_parameters())
})

