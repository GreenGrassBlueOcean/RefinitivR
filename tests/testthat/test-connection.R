.saved_state <- save_refinitiv_state()

test_that("rd_connection() creates a RefinitivConnection on first call", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  # Clear any cached connection
  .connection_cache$conn <- NULL

  conn <- rd_connection()
  expect_s3_class(conn, "RefinitivConnection")
})

test_that("rd_connection() returns the same cached object on repeated calls", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  conn1 <- rd_connection()
  conn2 <- rd_connection()
  expect_identical(conn1, conn2)
})

test_that("rd_connection(reset = TRUE) creates a new object", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  conn1 <- rd_connection()
  conn2 <- rd_connection(reset = TRUE)
  expect_s3_class(conn2, "RefinitivConnection")
  # New object, not the same instance
  expect_false(identical(conn1, conn2))
})

test_that("EikonConnect() updates the singleton", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  conn_before <- rd_connection()
  eikon_conn <- EikonConnect()
  conn_after <- rd_connection()

  # After EikonConnect(), the singleton should be the EikonConnect result
  expect_identical(conn_after, eikon_conn)
  expect_false(identical(conn_before, conn_after))
})

test_that("RDConnect() updates the singleton", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  conn_before <- rd_connection()
  rd_conn <- RDConnect()
  conn_after <- rd_connection()

  expect_identical(conn_after, rd_conn)
  expect_false(identical(conn_before, conn_after))
})

test_that("rd_ClearCache() resets the connection singleton", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  conn1 <- rd_connection()
  expect_false(is.null(.connection_cache$conn))

  suppressMessages(rd_ClearCache())
  expect_null(.connection_cache$conn)

  # Next call creates a fresh connection
  conn2 <- rd_connection()
  expect_s3_class(conn2, "RefinitivConnection")
  expect_false(identical(conn1, conn2))
})


test_that("EikonConnect warns when PythonModule is not JSON", {
  local_refinitiv_state()
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  expect_warning(
    EikonConnect(PythonModule = "RD"),
    "PythonModule parameter is deprecated"
  )
})

test_that("EikonConnect stops when TestConnection is not logical", {
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  expect_error(
    EikonConnect(TestConnection = "yes"),
    "TestConnection should be TRUE or FALSE"
  )
})

test_that("EikonConnect with TestConnection = TRUE succeeds when get_data works", {
  local_refinitiv_state()
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  stub(EikonConnect, ".resolve_and_connect", function(application_id, UUID) {
    mock_conn <- RefinitivJsonConnect()
    mock_conn$get_data <- function(instruments, fields, raw_output) list(ok = TRUE)
    mock_conn
  })

  conn <- EikonConnect(TestConnection = TRUE)
  expect_s3_class(conn, "RefinitivConnection")
})

test_that("EikonConnect with TestConnection = TRUE stops when get_data fails", {
  local_refinitiv_state()
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))
  .connection_cache$conn <- NULL

  stub(EikonConnect, ".resolve_and_connect", function(application_id, UUID) {
    mock_conn <- RefinitivJsonConnect()
    mock_conn$get_data <- function(instruments, fields, raw_output) stop("network error")
    mock_conn
  })

  expect_error(
    EikonConnect(TestConnection = TRUE),
    "Connection test failed"
  )
})

test_that("RDConnect warns when PythonModule is not JSON", {
  local_refinitiv_state()
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  expect_warning(
    RDConnect(PythonModule = "RD"),
    "PythonModule parameter is deprecated"
  )
})

test_that(".resolve_and_connect uses UUID from options when UUID arg is NA", {
  local_refinitiv_state()
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON",
    .RefinitivUUID = "options-uuid"
  ))

  .resolve_and_connect()
  expect_equal(getOption(".RefinitivUUID"), "options-uuid")
})

test_that(".resolve_and_connect uses explicit application_id when provided", {
  local_refinitiv_state()
  withr::local_options(list(
    refinitiv_base_url = "http://lh",
    eikon_port = 9000L,
    .RefinitivAPI = "JSON",
    .RefinitivPyModuleName = "JSON"
  ))

  .resolve_and_connect(application_id = "my-custom-key")
  expect_equal(refinitiv_vault_get("api_key"), "my-custom-key")
})


restore_refinitiv_state(.saved_state, "test-connection")
