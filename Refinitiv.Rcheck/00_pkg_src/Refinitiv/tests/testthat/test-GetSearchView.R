library(testthat)
library(mockery)

### Test 1: SearchView must not be NULL
test_that("GetSearchView stops when SearchView is NULL", {
  expect_error(
    GetSearchView(ConnectionObject = list(), SearchView = NULL),
    "parameter SearchView can not be null in GetSearchView"
  )
})

### Test 2: Known SearchView is returned as-is
test_that("GetSearchView returns SearchView when it is available", {
  stub(GetSearchView, "RDPShowAvailableSearchViews", function(...) {
    c("EquityQuotes", "SearchAll", "People")
  })

  result <- GetSearchView(ConnectionObject = list(), SearchView = "EquityQuotes")
  expect_equal(result, "EquityQuotes")
})

### Test 3: Legacy RD-style view names are translated via lookup
test_that("GetSearchView translates legacy RD-style view names", {
  stub(GetSearchView, "RDPShowAvailableSearchViews", function(...) {
    # Return empty so the view isn't found directly
    character(0)
  })

  # The lookup table maps RD-style names to JSON/RDP names.
  # Use a known mapping from SearchViewsLookup.
  lookup <- Refinitiv::SearchViewsLookup
  if (nrow(lookup) > 0) {
    rd_view <- lookup$SearchViews_RD[1]
    json_view <- lookup$SearchViews_JSON_RDP[1]

    result <- GetSearchView(ConnectionObject = list(), SearchView = rd_view)
    expect_equal(result, json_view)
  }
})

### Test 4: Unknown SearchView errors
test_that("GetSearchView errors for unknown SearchView", {
  stub(GetSearchView, "RDPShowAvailableSearchViews", function(...) {
    character(0)
  })

  expect_error(
    GetSearchView(ConnectionObject = list(), SearchView = "NONEXISTENT_VIEW"),
    "not available"
  )
})

dump_refinitiv_options("test-GetSearchView")
