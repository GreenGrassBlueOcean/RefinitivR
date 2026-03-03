test_that("Validtokens", {
  # dummy tests to allow helper function
  expect_equal(1, 1)
})

create_jwt <- function(payload) {
  # Encode header
  header <- jsonlite::toJSON(list(alg = "HS256", typ = "JWT"), auto_unbox = TRUE)
  header_b64 <- base64enc::base64encode(charToRaw(header))
  header_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", header_b64)))

  # Encode payload
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  payload_b64 <- base64enc::base64encode(charToRaw(payload_json))
  payload_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", payload_b64)))

  # Dummy signature (not verified)
  signature <- "signature"

  # Concatenate to form JWT
  jwt <- paste(header_b64url, payload_b64url, signature, sep = ".")
  return(jwt)
}

# Define the tests
testthat::test_that("Valid token is recognized as valid", {
  # No warnings expected for valid tokens
  future_time <- as.numeric(Sys.time()) + 3600 # 1 hour in the future
  payload <- list(exp = future_time)
  token <- create_jwt(payload)

  testthat::expect_no_warning({
    is_valid <- rd_VerifyToken(token)
    testthat::expect_true(is_valid)
  })
})

testthat::test_that("Expired token is recognized as invalid", {
  # Warning expected for expired tokens
  past_time <- as.numeric(Sys.time()) - 3600 # 1 hour in the past
  payload <- list(exp = past_time)
  token <- create_jwt(payload)

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "Token has expired"
  )
})

testthat::test_that("Token with invalid format (less than three parts) is invalid", {
  # Warning expected for invalid token format
  token <- "invalid.token"

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "Invalid token format\\. Expected three parts separated by '\\.'"
  )
})

testthat::test_that("Token with invalid format (more than three parts) is invalid", {
  # Warning expected for invalid token format
  token <- "part1.part2.part3.part4"

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "Invalid token format\\. Expected three parts separated by '\\.'"
  )
})

testthat::test_that("Token missing 'exp' claim is invalid", {
  # Warning expected for missing 'exp' claim
  payload <- list(sub = "1234567890", name = "John Doe")
  token <- create_jwt(payload)

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "'exp' field not found in token payload\\."
  )
})

testthat::test_that("Token with non-numeric 'exp' claim is invalid", {
  # Warning expected for non-numeric 'exp' claim
  payload <- list(exp = "not_a_timestamp")
  token <- create_jwt(payload)

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "'exp' field is not a valid numeric timestamp\\."
  )
})

testthat::test_that("Token with malformed Base64 payload is invalid", {
  # Warning expected for malformed Base64 payload
  header_b64url <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" # Valid header
  payload_b64url <- "!!!invalidbase64!!!"
  signature <- "signature"
  token <- paste(header_b64url, payload_b64url, signature, sep = ".")

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "Base64 decoding failed|JSON parsing failed"
  )
})

testthat::test_that("Token with malformed JSON payload is invalid", {
  # Warning expected for malformed JSON payload
  header <- jsonlite::toJSON(list(alg = "HS256", typ = "JWT"), auto_unbox = TRUE)
  header_b64 <- base64enc::base64encode(charToRaw(header))
  header_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", header_b64)))

  # Malformed JSON
  payload_json <- "{'exp': 1234567890" # Missing closing brace and incorrect quotes
  payload_b64 <- base64enc::base64encode(charToRaw(payload_json))
  payload_b64url <- gsub("=", "", gsub("\\+", "-", gsub("/", "_", payload_b64)))

  signature <- "signature"

  token <- paste(header_b64url, payload_b64url, signature, sep = ".")

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "JSON parsing failed"
  )
})

testthat::test_that("Empty token is invalid", {
  # Warning expected for empty token
  token <- ""

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "Token must be a non-empty character string\\."
  )
})

testthat::test_that("Non-string token is invalid", {
  # Warning expected for non-string tokens
  tokens <- list(NULL, 12345, list("part1", "part2", "part3"))

  for (token in tokens) {
    testthat::expect_warning(
      {
        is_valid <- rd_VerifyToken(token)
        testthat::expect_false(is_valid)
      },
      regexp = "Token must be a non-empty character string\\."
    )
  }
})


## Token buffer tests ----

testthat::test_that("Token expiring within buffer window is treated as expired", {
  # Token expires in 30 seconds, but default buffer is 60 — should be invalid
  near_expiry <- as.numeric(Sys.time()) + 30
  payload <- list(exp = near_expiry)
  token <- create_jwt(payload)

  testthat::expect_warning(
    {
      is_valid <- rd_VerifyToken(token)
      testthat::expect_false(is_valid)
    },
    regexp = "Token has expired"
  )
})

testthat::test_that("Token expiring well beyond buffer window is valid", {
  # Token expires in 120 seconds, buffer is 60 — should be valid
  safe_expiry <- as.numeric(Sys.time()) + 120
  payload <- list(exp = safe_expiry)
  token <- create_jwt(payload)

  testthat::expect_no_warning({
    is_valid <- rd_VerifyToken(token)
    testthat::expect_true(is_valid)
  })
})

testthat::test_that("Custom buffer via option is respected", {
  # Token expires in 15 seconds, custom buffer is 10 — should be valid
  near_expiry <- as.numeric(Sys.time()) + 15
  payload <- list(exp = near_expiry)
  token <- create_jwt(payload)

  withr::with_options(list(refinitiv_token_buffer_seconds = 10), {
    testthat::expect_no_warning({
      is_valid <- rd_VerifyToken(token)
      testthat::expect_true(is_valid)
    })
  })

  # Same token, but buffer of 20 — now invalid
  withr::with_options(list(refinitiv_token_buffer_seconds = 20), {
    testthat::expect_warning(
      {
        is_valid <- rd_VerifyToken(token)
        testthat::expect_false(is_valid)
      },
      regexp = "Token has expired"
    )
  })
})

dump_refinitiv_options("test-rd_VerifyToken")
