library(testthat)
library(mockery)

### Test 1: SearchView must not be NULL
test_that("GetSearchView stops when SearchView is NULL", {
  dummy_meta <- list(name = "refinitiv.data")
  expect_error(
    GetSearchView(ConnectionObject = list(),
                  ConnectionMetaData = dummy_meta,
                  SearchView = NULL),
    "parameter SearchView can not be null in GetSearchView"
  )
})

### For the RD branch (ConnectionMetaData$name == "refinitiv.data")

# Test 2: When SearchView is available via RDPShowAvailableSearchViews (RD branch)
test_that("GetSearchView returns the view from ConnectionObject for RD branch", {
  # Stub RDPShowAvailableSearchViews for Platform "RD"
  stub(GetSearchView, "RDPShowAvailableSearchViews", function(Platform) {
    if(Platform == "RD") return(c("SEARCH_ALL", "VIEW2"))
    else return(character(0))
  })

  # Create a dummy ConnectionObject with a search Views list.
  dummy_conn <- list(content = list(search = list(Views = list(SEARCH_ALL = "dummy_view_rd"))))
  dummy_meta <- list(name = "refinitiv.data")

  result <- GetSearchView(ConnectionObject = dummy_conn,
                          ConnectionMetaData = dummy_meta,
                          SearchView = "SEARCH_ALL")
  expect_equal(result, "dummy_view_rd")
})



### For the JSON branch (ConnectionMetaData$name is "JSON" or "testing object")

# Test 4: When SearchView is available via RDPShowAvailableSearchViews (JSON branch)
test_that("GetSearchView returns SearchView for JSON branch when available", {
  stub(GetSearchView, "RDPShowAvailableSearchViews", function(Platform) {
    if(Platform == "JSON") return(c("SEARCH_ALL", "OTHER_VIEW"))
    else return(character(0))
  })
  dummy_meta <- list(name = "JSON")

  result <- GetSearchView(ConnectionObject = list(),
                          ConnectionMetaData = dummy_meta,
                          SearchView = "SEARCH_ALL")
  expect_equal(result, "SEARCH_ALL")
})


### Test 6: When ConnectionMetaData$name is neither "refinitiv.data" nor "JSON" (error branch)
test_that("GetSearchView errors for unsupported ConnectionMetaData", {
  dummy_meta <- list(name = "other")
  expect_error(
    GetSearchView(ConnectionObject = list(),
                  ConnectionMetaData = dummy_meta,
                  SearchView = "ANY"),
    "GetSearchView only available for RD or JSON but not for: other"
  )
})
