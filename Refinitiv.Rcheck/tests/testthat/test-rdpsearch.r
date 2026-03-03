library(testthat)

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

  result <- RDPsearch(RDP = RD, query = "AAPL.O",
                      select = "DocumentTitle,RIC")

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

  result <- RDPsearch(RDP = RD, view = "EquityQuotes",
                      filter = "Eps gt 6.0 and IsPrimaryRIC eq true",
                      top = 5, select = "DocumentTitle,RIC,Eps")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

dump_refinitiv_options("test-rdpsearch")
