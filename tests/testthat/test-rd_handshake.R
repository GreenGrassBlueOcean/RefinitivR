library(testthat)
library(mockery)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# Unit tests for rd_handshake

# Test 1: When force is FALSE and an existing valid token is available,
# rd_handshake should return the existing token without performing a handshake.
test_that("rd_handshake returns existing valid token when force is FALSE", {
  local_refinitiv_state()

  existing_token <- "existing_token"
  token_expiration <- as.numeric(Sys.time()) + 3600 # expires in 1 hour
  token_type <- "bearer"
  refinitiv_vault_set("access_token", existing_token)
  refinitiv_vault_set("token_expiration", token_expiration)
  refinitiv_vault_set("token_type", token_type)

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "rd_VerifyToken", function(token) TRUE)

  result <- rd_handshake(debug = FALSE, force = FALSE)

  expect_equal(result$access_token, existing_token)
  expect_equal(result$token_type, token_type)
  # Allow a slight difference due to time elapsed.
  expect_true(result$expires_in > 3590 && result$expires_in <= 3600)
})

# Test 2: When force is TRUE (even if an existing token exists),
# rd_handshake should perform a handshake and return a new token.
test_that("rd_handshake fetches a new token when force is TRUE", {
  local_refinitiv_state()

  refinitiv_vault_set("access_token", "old_token")
  refinitiv_vault_set("token_expiration", as.numeric(Sys.time()) + 3600)
  refinitiv_vault_set("token_type", "bearer")
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

  new_response <- list(access_token = "new_token", expires_in = 1800, token_type = "bearer")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) new_response)

  result <- rd_handshake(debug = FALSE, force = TRUE)

  expect_equal(result$access_token, "new_token")
  expect_equal(result$expires_in, 1800)
  expect_equal(result$token_type, "bearer")
  # Verify that the new token is stored in the vault.
  expect_equal(refinitiv_vault_get("access_token"), "new_token")
})

# Test 3: When force is FALSE but the existing token is invalid,
# rd_handshake should perform a handshake to fetch a new token.
test_that("rd_handshake fetches a new token when existing token is invalid", {
  local_refinitiv_state()

  refinitiv_vault_set("access_token", "old_token")
  refinitiv_vault_set("token_expiration", as.numeric(Sys.time()) + 3600)
  refinitiv_vault_set("token_type", "bearer")
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

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
  local_refinitiv_state()

  refinitiv_vault_clear()
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

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
  local_refinitiv_state()

  refinitiv_vault_clear()
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

  new_response <- list(access_token = "debug_token", expires_in = 1800, token_type = "bearer")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) new_response)

  msgs <- capture_messages(rd_handshake(debug = TRUE, force = TRUE))
  expect_true(any(grepl("Sending handshake request to URL", msgs)))
  expect_true(any(grepl("New token acquired and stored successfully", msgs)))
})


# Q8: Handshake uses vault API key instead of hardcoded default
test_that("rd_handshake sends the vault API key in the payload", {
  local_refinitiv_state()

  refinitiv_vault_clear()
  refinitiv_vault_set("api_key", "MY_CUSTOM_KEY")
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

  captured_payload <- NULL
  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) {
    captured_payload <<- json
    list(access_token = "tok", expires_in = 1800, token_type = "bearer")
  })

  rd_handshake(debug = FALSE, force = TRUE)
  expect_equal(captured_payload$AppKey, "MY_CUSTOM_KEY")
})

test_that("rd_handshake falls back to DEFAULT_WORKSPACE_APP_KEY when vault is empty", {
  local_refinitiv_state()

  refinitiv_vault_clear()
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)
  withr::local_envvar(REFINITIV_APP_KEY = NA)

  captured_payload <- NULL
  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) {
    captured_payload <<- json
    list(access_token = "tok", expires_in = 1800, token_type = "bearer")
  })

  rd_handshake(debug = FALSE, force = TRUE)
  expect_equal(captured_payload$AppKey, "DEFAULT_WORKSPACE_APP_KEY")
})


restore_refinitiv_state(.saved_state, "test-rd_handshake")
