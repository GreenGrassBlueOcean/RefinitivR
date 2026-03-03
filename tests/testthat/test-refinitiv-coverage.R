# tests/testthat/test-refinitiv-coverage.R
# Additional tests for coverage of refinitiv.r

library(testthat)
library(mockery)

# ── install_eikon() is .Defunct ────────────────────────────────────────

test_that("install_eikon() raises .Defunct error", {
  expect_error(install_eikon(), "removed")
})


# ── PropertiesActiveRefinitivObject() ──────────────────────────────────

test_that("PropertiesActiveRefinitivObject returns correct list with verbose=TRUE", {
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    .RefinitivPyModuleType = "direct JSON connection through httr2 package"
  )
  expect_message(
    result <- PropertiesActiveRefinitivObject(verbose = TRUE),
    "JSON"
  )
  expect_type(result, "list")
  expect_equal(result$name, "JSON")
  expect_true(is.na(result$version))
})

test_that("PropertiesActiveRefinitivObject returns list silently with verbose=FALSE", {
  result <- PropertiesActiveRefinitivObject(verbose = FALSE)
  expect_type(result, "list")
  expect_equal(result$name, "JSON")
})

test_that("PropertiesActiveRefinitivObject errors on non-logical verbose", {
  expect_error(PropertiesActiveRefinitivObject(verbose = "yes"), "logical")
})


# ── InspectRequest() ──────────────────────────────────────────────────

test_that("InspectRequest returns NULL when verbose is FALSE", {
  expect_null(InspectRequest(data.frame(a = 1), "test", verbose = FALSE))
})

test_that("InspectRequest handles NA input", {
  expect_message(
    InspectRequest(NA, functionname = "testfn", verbose = TRUE),
    "testfn request returned NA"
  )
})

test_that("InspectRequest handles data with error field", {
  df_with_error <- list(
    error = list(
      list(code = 400, message = "Bad request"),
      list(code = 500, message = "Server error")
    )
  )
  expect_message(
    InspectRequest(df_with_error, functionname = "testfn", verbose = TRUE),
    "erors"
  )
})

test_that("InspectRequest handles data with totalRowsCount/totalColumnsCount", {
  df_counts <- list(totalRowsCount = 10, totalColumnsCount = 5)
  expect_message(
    InspectRequest(df_counts, functionname = "testfn", verbose = TRUE),
    "5 columns and 10 rows"
  )
})

test_that("InspectRequest handles empty input", {
  expect_message(
    InspectRequest(list(), functionname = "testfn", verbose = TRUE),
    "length 0"
  )
})

test_that("InspectRequest handles non-empty input", {
  expect_message(
    InspectRequest(list(a = 1, b = 2), functionname = "testfn", verbose = TRUE),
    "length 2"
  )
})


dump_refinitiv_options("test-refinitiv-coverage")
