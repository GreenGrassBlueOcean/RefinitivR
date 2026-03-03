# tests/testthat/test-credential_vault.R
# Unit tests for the credential vault (C3 audit fix)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# ── Helpers ──────────────────────────────────────────────────────────
# Each test should start with a clean vault
clean_vault <- function() {
  refinitiv_vault_clear()
}

# ── Basic set / get ──────────────────────────────────────────────────

test_that("set and get round-trip works", {
  clean_vault()
  refinitiv_vault_set("access_token", "tok123")
  expect_equal(refinitiv_vault_get("access_token"), "tok123")
})

test_that("get returns NULL for missing key", {
  clean_vault()
  expect_null(refinitiv_vault_get("nonexistent"))
})

test_that("has returns TRUE for existing key, FALSE otherwise", {
  clean_vault()
  expect_false(refinitiv_vault_has("access_token"))
  refinitiv_vault_set("access_token", "tok")

  expect_true(refinitiv_vault_has("access_token"))
})

test_that("set overwrites existing value", {
  clean_vault()
  refinitiv_vault_set("access_token", "old")
  refinitiv_vault_set("access_token", "new")
  expect_equal(refinitiv_vault_get("access_token"), "new")
})

# ── Input validation ─────────────────────────────────────────────────

test_that("set rejects non-scalar key", {
  expect_error(refinitiv_vault_set(c("a", "b"), "val"), "single character")
  expect_error(refinitiv_vault_set(42, "val"), "single character")
})

test_that("get rejects non-scalar key", {
  expect_error(refinitiv_vault_get(c("a", "b")), "single character")
})

test_that("has rejects non-scalar key", {
  expect_error(refinitiv_vault_has(123), "single character")
})

# ── Keys listing ─────────────────────────────────────────────────────

test_that("keys returns stored key names", {
  clean_vault()
  refinitiv_vault_set("access_token", "t")
  refinitiv_vault_set("token_expiration", 999)
  keys <- refinitiv_vault_keys()
  expect_true("access_token" %in% keys)
  expect_true("token_expiration" %in% keys)
})

# ── Clear ────────────────────────────────────────────────────────────

test_that("clear(keys) removes only specified keys", {
  clean_vault()
  refinitiv_vault_set("access_token", "t")
  refinitiv_vault_set("token_expiration", 999)
  refinitiv_vault_clear(keys = "access_token")
  expect_false(refinitiv_vault_has("access_token"))
  expect_true(refinitiv_vault_has("token_expiration"))
})

test_that("clear() with no args wipes everything", {
  clean_vault()
  refinitiv_vault_set("access_token", "t")
  refinitiv_vault_set("token_expiration", 999)
  refinitiv_vault_clear()
  expect_equal(length(refinitiv_vault_keys()), 0L)
})

# ── API-key write-through ────────────────────────────────────────────

test_that("setting api_key writes through to options(.EikonApiKey)", {
  clean_vault()
  old <- getOption(".EikonApiKey")
  on.exit(options(.EikonApiKey = old), add = TRUE)

  refinitiv_vault_set("api_key", "my_secret_key")
  expect_equal(refinitiv_vault_get("api_key"), "my_secret_key")
  expect_equal(getOption(".EikonApiKey"), "my_secret_key")
})

test_that("clearing api_key also clears options(.EikonApiKey)", {
  clean_vault()
  old <- getOption(".EikonApiKey")
  on.exit(options(.EikonApiKey = old), add = TRUE)

  refinitiv_vault_set("api_key", "key123")
  refinitiv_vault_clear(keys = "api_key")
  expect_null(getOption(".EikonApiKey"))
})

test_that("clear-all also clears options(.EikonApiKey)", {
  clean_vault()
  old <- getOption(".EikonApiKey")
  on.exit(options(.EikonApiKey = old), add = TRUE)

  refinitiv_vault_set("api_key", "key456")
  refinitiv_vault_clear()
  expect_null(getOption(".EikonApiKey"))
})

# ── Legacy fallback ──────────────────────────────────────────────────

test_that("get('api_key') falls back to getOption('.EikonApiKey') when vault empty", {
  clean_vault()
  old <- getOption(".EikonApiKey")
  on.exit(options(.EikonApiKey = old), add = TRUE)

  # Set via options only (simulates user .Rprofile)
  options(.EikonApiKey = "legacy_key")
  expect_equal(refinitiv_vault_get("api_key"), "legacy_key")
})

test_that("no legacy fallback for non-api_key keys", {
  clean_vault()
  # access_token should NOT fall back to options
  withr::with_options(
    list(refinitiv_access_token = "should_not_find"),
    {
      expect_null(refinitiv_vault_get("access_token"))
    }
  )
})

# ── Print safety ─────────────────────────────────────────────────────

test_that("print.refinitiv_vault shows key names only, not values", {
  clean_vault()
  refinitiv_vault_set("access_token", "SUPER_SECRET_TOKEN")
  refinitiv_vault_set("api_key", "SUPER_SECRET_KEY")

  output <- capture.output(print(refinitiv_vault()))
  combined <- paste(output, collapse = "\n")

  # Should show key names

  expect_true(grepl("access_token", combined))
  expect_true(grepl("api_key", combined))

  # Should NOT show values
  expect_false(grepl("SUPER_SECRET_TOKEN", combined))
  expect_false(grepl("SUPER_SECRET_KEY", combined))
})

test_that("refinitiv_vault() shows empty when vault is clean", {
  clean_vault()
  output <- capture.output(print(refinitiv_vault()))
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("empty", combined))
})

# ── Bearer tokens not visible via options() ──────────────────────────

test_that("bearer token is not accessible via getOption after storing in vault", {
  clean_vault()
  refinitiv_vault_set("access_token", "secret_jwt_here")

  # The old option should be NULL (cleared by .onLoad or never set)
  expect_null(getOption("refinitiv_access_token"))
  # But vault has it
  expect_equal(refinitiv_vault_get("access_token"), "secret_jwt_here")
})

test_that("token_expiration is not accessible via getOption", {
  clean_vault()
  refinitiv_vault_set("token_expiration", 1234567890)
  expect_null(getOption("refinitiv_token_expiration"))
  expect_equal(refinitiv_vault_get("token_expiration"), 1234567890)
})

# ── Invisibility ─────────────────────────────────────────────────────

test_that("refinitiv_vault_set returns value invisibly", {
  clean_vault()
  out <- withVisible(refinitiv_vault_set("access_token", "t"))
  expect_false(out$visible)
  expect_equal(out$value, "t")
})

test_that("refinitiv_vault_clear returns TRUE invisibly", {
  clean_vault()
  out <- withVisible(refinitiv_vault_clear())
  expect_false(out$visible)
  expect_true(out$value)
})


restore_refinitiv_state(.saved_state, "test-credential_vault")
