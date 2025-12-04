library(testthat)
library(mockery)

# Unit tests for streaming utility functions

context("Streaming Utilities")

# Test get_streaming_url
test_that("get_streaming_url constructs correct URL with default port", {
  options(refinitiv_base_url = "http://localhost")
  options(streaming_port = 9000L)
  
  url <- get_streaming_url("pricing")
  expect_equal(url, "ws://localhost:9000/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
  options(streaming_port = NULL)
})

test_that("get_streaming_url uses provided port", {
  options(refinitiv_base_url = "http://localhost")
  
  url <- get_streaming_url("pricing", port = 8080L)
  expect_equal(url, "ws://localhost:8080/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
})

test_that("get_streaming_url falls back to eikon_port", {
  options(refinitiv_base_url = "http://localhost")
  options(streaming_port = NULL)
  options(eikon_port = 9060L)
  
  url <- get_streaming_url("pricing")
  expect_equal(url, "ws://localhost:9060/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
  options(eikon_port = NULL)
})

test_that("get_streaming_url falls back to rdp_port", {
  options(refinitiv_base_url = "http://localhost")
  options(streaming_port = NULL)
  options(eikon_port = NULL)
  options(rdp_port = 9060L)
  
  url <- get_streaming_url("pricing")
  expect_equal(url, "ws://localhost:9060/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
  options(rdp_port = NULL)
})

test_that("get_streaming_url uses default port when no options set", {
  options(refinitiv_base_url = "http://localhost")
  options(streaming_port = NULL)
  options(eikon_port = NULL)
  options(rdp_port = NULL)
  
  url <- get_streaming_url("pricing")
  expect_equal(url, "ws://localhost:9060/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
})

test_that("get_streaming_url converts http to ws and https to wss", {
  # Test http -> ws
  options(refinitiv_base_url = "http://api.example.com")
  options(streaming_port = 80L)
  
  url <- get_streaming_url("pricing")
  expect_equal(url, "ws://api.example.com:80/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Test https -> wss
  options(refinitiv_base_url = "https://api.example.com")
  options(streaming_port = 443L)
  
  url <- get_streaming_url("pricing")
  expect_equal(url, "wss://api.example.com:443/api/rdp/streaming/pricing/v1/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
  options(streaming_port = NULL)
})

test_that("get_streaming_url handles analytics stream type", {
  options(refinitiv_base_url = "http://localhost")
  options(streaming_port = 9000L)
  
  url <- get_streaming_url("analytics")
  expect_equal(url, "ws://localhost:9000/api/rdp/streaming/quantitative-analytics/beta1/financial-contracts/WebSocket")
  
  # Cleanup
  options(refinitiv_base_url = NULL)
  options(streaming_port = NULL)
})

test_that("get_streaming_url errors on unknown stream type", {
  expect_error(
    get_streaming_url("unknown"),
    "Unknown stream type: unknown"
  )
})

# Test get_streaming_protocol
test_that("get_streaming_protocol returns correct protocol for pricing", {
  protocol <- get_streaming_protocol("pricing")
  expect_equal(protocol, "tr_json2")
})

test_that("get_streaming_protocol returns correct protocol for analytics", {
  protocol <- get_streaming_protocol("analytics")
  expect_equal(protocol, "rdp_streaming")
})

test_that("get_streaming_protocol defaults to tr_json2 for unknown", {
  protocol <- get_streaming_protocol("unknown")
  expect_equal(protocol, "tr_json2")
})

# Test create_omm_login_request
test_that("create_omm_login_request creates valid JSON", {
  token <- "test_token_123"
  request <- create_omm_login_request(access_token = token)
  
  expect_type(request, "character")
  expect_true(grepl('"ID":1', request))
  expect_true(grepl('"Domain":"Login"', request))
  expect_true(grepl('"Authorization":"Bearer test_token_123"', request))
  expect_true(grepl('"AppKey":"DEFAULT_WORKSPACE_APP_KEY"', request))
})

test_that("create_omm_login_request uses custom app_key", {
  token <- "test_token"
  request <- create_omm_login_request(app_key = "CUSTOM_KEY", access_token = token)
  
  expect_true(grepl('"AppKey":"CUSTOM_KEY"', request))
})

# Test create_omm_stream_request
test_that("create_omm_stream_request creates valid request", {
  request <- create_omm_stream_request(
    domain = "MarketPrice",
    universe = "EUR=",
    fields = c("BID", "ASK"),
    streaming = TRUE,
    stream_id = 2L
  )
  
  expect_type(request, "character")
  expect_true(grepl('"ID":2', request))
  expect_true(grepl('"Domain":"MarketPrice"', request))
  expect_true(grepl('"Streaming":true', request))
  expect_true(grepl('"Name":"EUR="', request))
  expect_true(grepl('"BID"', request))
  expect_true(grepl('"ASK"', request))
})

test_that("create_omm_stream_request handles streaming = FALSE", {
  request <- create_omm_stream_request(
    domain = "MarketPrice",
    universe = "EUR=",
    fields = c("BID"),
    streaming = FALSE,
    stream_id = 3L
  )
  
  expect_true(grepl('"Streaming":false', request))
})

test_that("create_omm_stream_request warns on multiple instruments", {
  expect_warning(
    create_omm_stream_request(
      domain = "MarketPrice",
      universe = c("EUR=", "GBP="),
      fields = c("BID"),
      streaming = TRUE,
      stream_id = 4L
    ),
    "Multiple instruments not yet fully supported"
  )
})

test_that("create_omm_stream_request uses default stream_id", {
  request <- create_omm_stream_request(
    domain = "MarketPrice",
    universe = "EUR=",
    fields = c("BID"),
    streaming = TRUE,
    stream_id = NULL
  )
  
  expect_true(grepl('"ID":2', request))
})

# Test create_streaming_headers
test_that("create_streaming_headers creates correct headers", {
  token <- "test_token_456"
  headers <- create_streaming_headers(token)
  
  expect_type(headers, "list")
  expect_equal(headers$`User-Agent`, "R")
  expect_equal(headers$`x-tr-applicationid`, "DEFAULT_WORKSPACE_APP_KEY")
  expect_equal(headers$Authorization, "Bearer test_token_456")
})

# Test validate_streaming_params
test_that("validate_streaming_params accepts valid inputs", {
  expect_true(validate_streaming_params(c("EUR="), c("BID", "ASK")))
})

test_that("validate_streaming_params errors on NULL universe", {
  expect_error(
    validate_streaming_params(NULL, c("BID")),
    "universe must be a non-empty character vector"
  )
})

test_that("validate_streaming_params errors on empty universe", {
  expect_error(
    validate_streaming_params(character(0), c("BID")),
    "universe must be a non-empty character vector"
  )
})

test_that("validate_streaming_params errors on non-character universe", {
  expect_error(
    validate_streaming_params(123, c("BID")),
    "universe must be a character vector"
  )
})

test_that("validate_streaming_params errors on NULL fields", {
  expect_error(
    validate_streaming_params(c("EUR="), NULL),
    "fields must be a non-empty character vector"
  )
})

test_that("validate_streaming_params errors on empty fields", {
  expect_error(
    validate_streaming_params(c("EUR="), character(0)),
    "fields must be a non-empty character vector"
  )
})

test_that("validate_streaming_params errors on non-character fields", {
  expect_error(
    validate_streaming_params(c("EUR="), 123),
    "fields must be a character vector"
  )
})

# Test build_json_list_string
test_that("build_json_list_string creates valid JSON array", {
  result <- build_json_list_string(c("BID", "ASK", "OPEN_PRC"))
  expect_equal(result, '["BID","ASK","OPEN_PRC"]')
})

test_that("build_json_list_string handles single field", {
  result <- build_json_list_string("BID")
  expect_equal(result, '["BID"]')
})

test_that("build_json_list_string handles empty vector", {
  result <- build_json_list_string(character(0))
  expect_equal(result, '[""]')
})

# Test poll_until_connected
test_that("poll_until_connected returns TRUE when connected", {
  # Mock WebSocket object
  mock_ws <- list(
    readyState = function() 1L  # Connected
  )
  
  # Mock later package
  stub(poll_until_connected, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") return(TRUE)
    return(FALSE)
  })
  
  stub(poll_until_connected, "later::run_now", function(timeout) NULL)
  
  result <- poll_until_connected(mock_ws, timeout = 1)
  expect_true(result)
})

test_that("poll_until_connected errors when not connected", {
  # Mock WebSocket object that never connects
  mock_ws <- list(
    readyState = function() 0L  # Connecting
  )
  
  # Mock later package
  stub(poll_until_connected, "requireNamespace", function(pkg, quietly) {
    if (pkg == "later") return(TRUE)
    return(FALSE)
  })
  
  stub(poll_until_connected, "later::run_now", function(timeout) NULL)
  
  expect_error(
    poll_until_connected(mock_ws, timeout = 0.1),
    "Unable to establish websocket connection"
  )
})

test_that("poll_until_connected errors when later package not available", {
  mock_ws <- list(readyState = function() 0L)
  
  stub(poll_until_connected, "requireNamespace", function(pkg, quietly) FALSE)
  
  expect_error(
    poll_until_connected(mock_ws, timeout = 1),
    "Please install 'later' package"
  )
})

