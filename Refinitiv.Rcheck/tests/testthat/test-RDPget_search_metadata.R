library(testthat)
library(mockery)

# RDPget_search_metadata now always uses JSON (calls RDP$get_search_metadata()).
# The refinitiv.data branch was removed in 0.2.0.

test_that("RDPget_search_metadata works with JSON connection", {
  dummy_df <- data.frame(
    Refinitiv_index = "DummyIndex",
    Type = "Equity",
    Searchable = TRUE,
    Sortable = FALSE,
    Navigable = TRUE,
    Groupable = FALSE,
    Exact = TRUE,
    Symbol = FALSE,
    stringsAsFactors = FALSE
  )

  dummy_conn <- list(
    get_search_metadata = function(searchView) dummy_df
  )

  result <- RDPget_search_metadata(RDP = dummy_conn, searchView = "EquityQuotes")
  expect_s3_class(result, "data.frame")
  expect_identical(result, dummy_df)
})

test_that("RDPget_search_metadata uses SearchAll as default view", {
  captured_view <- NULL
  dummy_conn <- list(
    get_search_metadata = function(searchView) {
      captured_view <<- searchView
      data.frame(x = 1)
    }
  )

  RDPget_search_metadata(RDP = dummy_conn)
  expect_equal(captured_view, "SearchAll")
})

test_that("RDPget_search_metadata passes through errors from connection", {
  dummy_conn <- list(
    get_search_metadata = function(searchView) {
      stop("SearchView not available")
    }
  )

  expect_error(
    RDPget_search_metadata(RDP = dummy_conn, searchView = "INVALID"),
    "SearchView not available"
  )
})

test_that("RDPget_search_metadata works with live API", {
  skip_if(!has_live_api(), "No live API available")
  RD <- RDConnect()

  result <- RDPget_search_metadata(RDP = RD, searchView = "EquityQuotes")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

dump_refinitiv_options("test-RDPget_search_metadata")
