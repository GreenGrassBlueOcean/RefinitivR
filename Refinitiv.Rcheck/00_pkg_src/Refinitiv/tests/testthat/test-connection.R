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
