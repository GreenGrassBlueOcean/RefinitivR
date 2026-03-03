library(testthat)

.saved_state <- save_refinitiv_state()

test_that("RDConnect uses DEFAULT_WORKSPACE_APP_KEY when no key supplied", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  refinitiv_vault_clear(keys = "api_key")

  result <- RDConnect()
  expect_s3_class(result, "RefinitivConnection")
  expect_equal(refinitiv_vault_get("api_key"), "DEFAULT_WORKSPACE_APP_KEY")
})

test_that("RDConnect stores explicit key in vault", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  result <- RDConnect(application_id = "custom_key_123")
  expect_s3_class(result, "RefinitivConnection")
  expect_equal(refinitiv_vault_get("api_key"), "custom_key_123")
})

test_that("RDConnect warns when PythonModule is not JSON", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  expect_warning(
    RDConnect(application_id = "dummy_key", PythonModule = "RD"),
    "PythonModule parameter is deprecated"
  )
})

test_that("RDConnect still returns valid connection when PythonModule is RD", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  result <- suppressWarnings(
    RDConnect(application_id = "dummy_key", PythonModule = "RD")
  )
  expect_s3_class(result, "RefinitivConnection")
})

test_that("RDConnect updates the connection singleton", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  rd_conn <- RDConnect()
  expect_identical(rd_connection(), rd_conn)
})


restore_refinitiv_state(.saved_state, "test-RDConnect")
