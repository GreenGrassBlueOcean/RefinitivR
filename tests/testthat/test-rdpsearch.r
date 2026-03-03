library(testthat)

.saved_state <- save_refinitiv_state()

# These tests require a live Eikon/Workspace terminal connection.
# Offline coverage for RDPsearch is provided by
# test-httptest2-integration.R using recorded fixtures.

test_that("RDPsearch returns results for basic query", {
  skip_if(!has_live_api(), "No live API available")
  RD <- RDConnect()

  result <- RDPsearch(RDP = RD, query = "AAPL.O")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("DocumentTitle" %in% names(result))
})

test_that("RDPsearch works with select parameter", {
  skip_if(!has_live_api(), "No live API available")
  RD <- RDConnect()

  result <- RDPsearch(
    RDP = RD, query = "AAPL.O",
    select = "DocumentTitle,RIC"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("RDPsearch works with People view", {
  skip_if(!has_live_api(), "No live API available")
  RD <- RDConnect()

  result <- RDPsearch(RDP = RD, query = "president", view = "People")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("RDPsearch works with complex filter", {
  skip_if(!has_live_api(), "No live API available")
  RD <- RDConnect()

  result <- RDPsearch(
    RDP = RD, view = "EquityQuotes",
    filter = "Eps gt 6.0 and IsPrimaryRIC eq true",
    top = 5, select = "DocumentTitle,RIC,Eps"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})


test_that("RDPsearch caches result and returns hit on second call", {
  call_count <- 0L
  mock_result <- data.table::data.table(DocumentTitle = "Apple Inc", RIC = "AAPL.O")
  dummy_conn <- list(
    search = function(...) {
      call_count <<- call_count + 1L
      mock_result
    }
  )

  rd_ClearCache()

  result1 <- RDPsearch(RDP = dummy_conn, query = "AAPL.O", cache = TRUE)
  result2 <- RDPsearch(RDP = dummy_conn, query = "AAPL.O", cache = TRUE)

  expect_equal(call_count, 1L)
  expect_s3_class(result1, "data.frame")
  expect_identical(result1, result2)

  rd_ClearCache()
})

test_that("RDPsearch warns and returns empty data.frame when search returns non-data.table", {
  dummy_conn <- list(
    search = function(...) NULL
  )

  expect_warning(
    result <- RDPsearch(RDP = dummy_conn, query = "AAPL.O"),
    "did not provide any result"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("RDPsearch coerces group_count and removes SpaceConvertor and cache from Arglist", {
  captured_args <- list()
  dummy_conn <- list(
    search = function(...) {
      captured_args <<- list(...)
      data.table::data.table(DocumentTitle = "Test")
    }
  )

  result <- RDPsearch(
    RDP = dummy_conn,
    query = "president",
    view = "People",
    group_by = "FirstName",
    group_count = 2,
    SpaceConvertor = "_",
    cache = FALSE
  )

  # group_count coerced to integer
  expect_type(captured_args$group_count, "integer")
  # SpaceConvertor and cache stripped from Arglist (not passed to search)
  expect_null(captured_args$SpaceConvertor)
  expect_null(captured_args$cache)
  expect_s3_class(result, "data.frame")
})


restore_refinitiv_state(.saved_state, "test-rdpsearch")
