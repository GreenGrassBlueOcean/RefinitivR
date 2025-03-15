library(testthat)
library(mockery)

# Unit tests for rd_handshake

# Test 1: When force is FALSE and an existing valid token is available,
# rd_handshake should return the existing token without performing a handshake.
test_that("rd_handshake returns existing valid token when force is FALSE", {
  # Set up existing token options.
  existing_token <- "existing_token"
  token_expiration <- as.numeric(Sys.time()) + 3600  # expires in 1 hour
  token_type <- "bearer"
  options(refinitiv_access_token = existing_token)
  options(refinitiv_token_expiration = token_expiration)
  options(refinitiv_token_type = token_type)

  # Stub external functions:
  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "rd_VerifyToken", function(token) TRUE)

  # Call rd_handshake with force = FALSE.
  result <- rd_handshake(debug = FALSE, force = FALSE)

  expect_equal(result$access_token, existing_token)
  expect_equal(result$token_type, token_type)
  # Allow a slight difference due to time elapsed.
  expect_true(result$expires_in > 3590 && result$expires_in <= 3600)
})

# Test 2: When force is TRUE (even if an existing token exists),
# rd_handshake should perform a handshake and return a new token.
test_that("rd_handshake fetches a new token when force is TRUE", {
  # Set up options with an existing token.
  options(refinitiv_access_token = "old_token")
  options(refinitiv_token_expiration = as.numeric(Sys.time()) + 3600)
  options(refinitiv_token_type = "bearer")
  # Set required options for handshake URL.
  options(refinitiv_base_url = "http://example.com")
  options(rdp_port = "1234")

  new_response <- list(access_token = "new_token", expires_in = 1800, token_type = "bearer")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  # Even if rd_VerifyToken exists, force is TRUE so handshake is performed.
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) new_response)

  result <- rd_handshake(debug = FALSE, force = TRUE)

  expect_equal(result$access_token, "new_token")
  expect_equal(result$expires_in, 1800)
  expect_equal(result$token_type, "bearer")
  # Verify that the new token is stored in options.
  expect_equal(getOption("refinitiv_access_token"), "new_token")
})

# Test 3: When force is FALSE but the existing token is invalid,
# rd_handshake should perform a handshake to fetch a new token.
test_that("rd_handshake fetches a new token when existing token is invalid", {
  # Set up options with an existing token.
  options(refinitiv_access_token = "old_token")
  options(refinitiv_token_expiration = as.numeric(Sys.time()) + 3600)
  options(refinitiv_token_type = "bearer")
  options(refinitiv_base_url = "http://example.com")
  options(rdp_port = "1234")

  new_response <- list(access_token = "new_token_invalid", expires_in = 1200, token_type = "bearer")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "rd_VerifyToken", function(token) FALSE)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) new_response)

  result <- rd_handshake(debug = FALSE, force = FALSE)

  expect_equal(result$access_token, "new_token_invalid")
  expect_equal(result$expires_in, 1200)
})

# Test 4: When the handshake response is missing required fields,
# rd_handshake should warn and return NULL.
test_that("rd_handshake returns NULL when handshake response is missing required fields", {
  # Clear existing token options to force handshake.
  options(refinitiv_access_token = NULL)
  options(refinitiv_token_expiration = NULL)
  options(refinitiv_token_type = NULL)
  options(refinitiv_base_url = "http://example.com")
  options(rdp_port = "1234")

  # Incomplete response (missing expires_in and token_type).
  incomplete_response <- list(access_token = "incomplete_token")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) incomplete_response)

  expect_warning(
    result <- rd_handshake(debug = FALSE, force = TRUE),
    "Handshake response is missing required fields."
  )
  expect_null(result)
})

# Test 5: When debug = TRUE, debug messages should be printed.
test_that("rd_handshake prints debug messages when debug is TRUE", {
  # Clear existing token options to force handshake.
  options(refinitiv_access_token = NULL)
  options(refinitiv_token_expiration = NULL)
  options(refinitiv_token_type = NULL)
  options(refinitiv_base_url = "http://example.com")
  options(rdp_port = "1234")

  new_response <- list(access_token = "debug_token", expires_in = 1800, token_type = "bearer")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) new_response)

  msgs <- capture_messages(rd_handshake(debug = TRUE, force = TRUE))
  expect_true(any(grepl("Sending handshake request to URL", msgs)))
  expect_true(any(grepl("New token acquired and stored successfully", msgs)))
})
