# tests/testthat/test-zzz-onLoad.R
# Unit tests for .onLoad() in R/zzz.r
#
# covr cannot trace .onLoad() when it runs during package loading because
# the tracing instrumentation isn't yet in place.  We cover the code by
# calling .onLoad() directly with options/env-vars set up to exercise
# each branch.

test_that(".onLoad sets refinitiv_base_url from env var when option is NULL", {
  local_refinitiv_state()
  withr::local_options(refinitiv_base_url = NULL)
  withr::local_envvar(REFINITIV_BASE_URL = "http://my-remote-host")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("refinitiv_base_url"), "http://my-remote-host")
})

test_that(".onLoad uses default http://localhost when REFINITIV_BASE_URL is unset", {
  local_refinitiv_state()
  withr::local_options(refinitiv_base_url = NULL)
  withr::local_envvar(REFINITIV_BASE_URL = NA)
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("refinitiv_base_url"), "http://localhost")
})

test_that(".onLoad skips refinitiv_base_url when option already set", {
  local_refinitiv_state()
  withr::local_options(refinitiv_base_url = "http://already-set")
  withr::local_envvar(REFINITIV_BASE_URL = "http://should-not-override")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("refinitiv_base_url"), "http://already-set")
})

test_that(".onLoad sets eikon_port from env var when option is NULL", {
  local_refinitiv_state()
  withr::local_options(eikon_port = NULL)
  withr::local_envvar(REFINITIV_PORT = "9060")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("eikon_port"), 9060L)
})

test_that(".onLoad does not set eikon_port when REFINITIV_PORT is empty", {
  local_refinitiv_state()
  withr::local_options(eikon_port = NULL)
  withr::local_envvar(REFINITIV_PORT = NA)
  Refinitiv:::.onLoad(NULL, NULL)
  expect_null(getOption("eikon_port"))
})

test_that(".onLoad sets eikon_api and rdp_api when options are NULL", {
  local_refinitiv_state()
  withr::local_options(eikon_api = NULL, rdp_api = NULL)
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("eikon_api"), "/api/udf/")
  expect_equal(getOption("rdp_api"), "/api/rdp/")
})

test_that(".onLoad sets streaming_port from eikon_port when present", {
  local_refinitiv_state()
  withr::local_options(eikon_port = 9060L, streaming_port = NULL)
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("streaming_port"), 9060L)
})

test_that(".onLoad defaults streaming_port to 9000 when eikon_port is NULL", {
  local_refinitiv_state()
  withr::local_options(eikon_port = NULL, streaming_port = NULL)
  withr::local_envvar(REFINITIV_PORT = NA)
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(getOption("streaming_port"), 9000L)
})

test_that(".onLoad sets HistoricalPricingFields when option is NULL", {
  local_refinitiv_state()
  withr::local_options(HistoricalPricingFields = NULL)
  Refinitiv:::.onLoad(NULL, NULL)
  hpf <- getOption("HistoricalPricingFields")
  expect_true(is.character(hpf))
  expect_true("TRDPRC_1" %in% hpf)
  expect_true("ACVOL_UNS" %in% hpf)
  expect_length(hpf, 31)
})

test_that(".onLoad migrates legacy .EikonApiKey to vault", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  withr::local_options(.EikonApiKey = "legacy_test_key")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(refinitiv_vault_get("api_key"), "legacy_test_key")
})

test_that(".onLoad reads REFINITIV_APP_KEY env var into vault", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  withr::local_options(.EikonApiKey = NULL)
  withr::local_envvar(REFINITIV_APP_KEY = "envvar_key_123")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(refinitiv_vault_get("api_key"), "envvar_key_123")
})

test_that(".onLoad does not overwrite vault api_key with env var", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  refinitiv_vault_set("api_key", "already_in_vault")
  withr::local_options(.EikonApiKey = NULL)
  withr::local_envvar(REFINITIV_APP_KEY = "should_not_override")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_equal(refinitiv_vault_get("api_key"), "already_in_vault")
})

test_that(".onLoad clears stale token options", {
  local_refinitiv_state()
  withr::local_options(
    refinitiv_access_token = "old_token",
    refinitiv_token_expiration = 12345,
    refinitiv_token_type = "Bearer"
  )
  Refinitiv:::.onLoad(NULL, NULL)
  expect_null(getOption("refinitiv_access_token"))
  expect_null(getOption("refinitiv_token_expiration"))
  expect_null(getOption("refinitiv_token_type"))
})
