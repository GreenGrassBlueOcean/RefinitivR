# tests/testthat/test-bearer-coverage.R
# Additional tests for coverage of BearerTokenManagement.R

library(testthat)
library(mockery)

.saved_state <- save_refinitiv_state()


# ── rd_handshake debug messages for token reuse/expiry/force ───────────

test_that("rd_handshake debug message: 'Using existing valid token'", {
  local_refinitiv_state()

  refinitiv_vault_set("access_token", "valid_tok")
  refinitiv_vault_set("token_expiration", as.numeric(Sys.time()) + 3600)
  refinitiv_vault_set("token_type", "bearer")

  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "rd_VerifyToken", function(token) TRUE)

  msgs <- capture_messages(rd_handshake(debug = TRUE, force = FALSE))
  expect_true(any(grepl("Using existing valid token", msgs)))
})

test_that("rd_handshake debug message: 'Existing token is invalid or expired'", {
  local_refinitiv_state()

  refinitiv_vault_set("access_token", "expired_tok")
  refinitiv_vault_set("token_expiration", as.numeric(Sys.time()) + 3600)
  refinitiv_vault_set("token_type", "bearer")
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

  new_response <- list(access_token = "new_tok", expires_in = 1800, token_type = "bearer")
  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "rd_VerifyToken", function(token) FALSE)
  stub(rd_handshake, "send_json_request", function(...) new_response)

  msgs <- capture_messages(rd_handshake(debug = TRUE, force = FALSE))
  expect_true(any(grepl("Existing token is invalid or expired", msgs)))
})

test_that("rd_handshake debug message: 'Force is TRUE'", {
  local_refinitiv_state()

  refinitiv_vault_set("access_token", "existing_tok")
  refinitiv_vault_set("token_expiration", as.numeric(Sys.time()) + 3600)
  refinitiv_vault_set("token_type", "bearer")
  withr::local_options(refinitiv_base_url = "http://example.com", eikon_port = 1234L)

  new_response <- list(access_token = "forced_tok", expires_in = 1800, token_type = "bearer")
  stub(rd_handshake, "CheckTerminalType", function(verbose, force) NULL)
  stub(rd_handshake, "send_json_request", function(...) new_response)

  msgs <- capture_messages(rd_handshake(debug = TRUE, force = TRUE))
  expect_true(any(grepl("Force is TRUE", msgs)))
})


# ── rd_VerifyToken edge cases ──────────────────────────────────────────

# Helper from test-rd_VerifyToken.R
create_jwt <- function(payload) {
  header <- jsonlite::toJSON(list(alg = "HS256", typ = "JWT"), auto_unbox = TRUE)
  header_b64 <- base64enc::base64encode(charToRaw(header))
  header_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", header_b64)))
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  payload_b64 <- base64enc::base64encode(charToRaw(payload_json))
  payload_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", payload_b64)))
  paste(header_b64url, payload_b64url, "signature", sep = ".")
}

test_that("rd_VerifyToken returns FALSE when base64 decode fails", {
  # Construct a token where the payload is invalid base64 that triggers decode error
  token <- paste("eyJhbGciOiJIUzI1NiJ9", "!!!invalid!!!", "sig", sep = ".")
  expect_warning(
    result <- rd_VerifyToken(token),
    "Base64 decoding failed|JSON parsing failed"
  )
  expect_false(result)
})

test_that("rd_VerifyToken returns FALSE when rawToChar fails on NUL bytes", {
  # Create a payload that decodes to raw bytes containing embedded NUL
  # base64 for raw bytes \x00\x01\x02 = "AAEC"
  header <- "eyJhbGciOiJIUzI1NiJ9"
  # This base64 decodes to raw bytes with embedded NULs
  payload_with_nul <- base64enc::base64encode(as.raw(c(0x7b, 0x00, 0x7d)))
  payload_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", payload_with_nul)))
  token <- paste(header, payload_b64url, "sig", sep = ".")

  expect_warning(
    result <- rd_VerifyToken(token),
    "Failed to convert|JSON parsing failed|embedded nul"
  )
  expect_false(result)
})

test_that("rd_VerifyToken returns FALSE when JSON payload is not parseable", {
  header <- jsonlite::toJSON(list(alg = "HS256"), auto_unbox = TRUE)
  header_b64 <- base64enc::base64encode(charToRaw(header))
  header_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", header_b64)))

  # Valid base64, but not valid JSON
  bad_json <- "this is not json at all"
  payload_b64 <- base64enc::base64encode(charToRaw(bad_json))
  payload_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", payload_b64)))

  token <- paste(header_b64url, payload_b64url, "sig", sep = ".")
  expect_warning(
    result <- rd_VerifyToken(token),
    "JSON parsing failed"
  )
  expect_false(result)
})


restore_refinitiv_state(.saved_state, "test-bearer-coverage")
