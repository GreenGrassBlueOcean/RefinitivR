library(testthat)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
local_refinitiv_state(teardown_env())

# ── Class and basic properties ──────────────────────────────────────

test_that("RefinitivJsonConnect returns a RefinitivConnection object", {
  setup_mock_json_env()
  conn <- suppressWarnings(RefinitivJsonConnect(Eikonapplication_id = "test_key"))

  expect_s3_class(conn, "RefinitivConnection")
  expect_s3_class(conn, "environment")
  expect_true(is.environment(conn))
})

# ── names() returns only API methods ────────────────────────────────

test_that("names() returns API methods without environment internals", {
  setup_mock_json_env()
  conn <- suppressWarnings(RefinitivJsonConnect(Eikonapplication_id = "test_key"))

  method_names <- names(conn)

  # Should include the known API methods
  expect_true("get_data" %in% method_names)
  expect_true("get_timeseries" %in% method_names)
  expect_true("search" %in% method_names)
  expect_true("set_app_key" %in% method_names)
  expect_true("get_app_key" %in% method_names)

  # Should NOT include environment/R6 internals
  expect_false(".__enclos_env__" %in% method_names)
  expect_false("clone" %in% method_names)
  expect_false("initialize" %in% method_names)
  expect_false(".__active__" %in% method_names)
})

# ── print() output ──────────────────────────────────────────────────

test_that("print() shows connection info without secrets", {
  setup_mock_json_env()
  conn <- suppressWarnings(RefinitivJsonConnect(Eikonapplication_id = "test_key"))

  output <- capture.output(print(conn))
  full <- paste(output, collapse = "\n")

  expect_true(grepl("RefinitivConnection", full))
  expect_true(grepl("Base URL", full))
  expect_true(grepl("Port", full))
  expect_true(grepl("API key", full))
  expect_true(grepl("Methods", full))

  # Secrets must not appear
  expect_false(grepl("test_key", full))
})

test_that("print() reports credential status correctly", {
  setup_mock_json_env()
  conn <- suppressWarnings(RefinitivJsonConnect(Eikonapplication_id = "test_key"))

  # API key was set by setup_mock_json_env

  output_with_key <- capture.output(print(conn))
  expect_true(any(grepl("\\(set\\)", output_with_key)))

  # Clear vault and check again
  refinitiv_vault_clear(keys = "api_key")
  output_no_key <- capture.output(print(conn))
  expect_true(any(grepl("\\(not set\\)", output_no_key)))

  # Restore
  refinitiv_vault_set("api_key", "test_key")
})

dump_refinitiv_options("test-RefinitivConnection")
