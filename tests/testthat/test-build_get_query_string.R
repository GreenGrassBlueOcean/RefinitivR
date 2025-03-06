
test_that("build_get_query_string returns empty string for an empty list", {
  expect_equal(build_get_query_string(list()), "")
})

test_that("build_get_query_string returns correctly encoded query string", {
  params <- list(
    query    = "R:TSLA.O AND Language:EN",
    limit    = 5,
    dateFrom = "2023-01-01T00:00:00Z",
    extra    = NULL
  )
  qs <- build_get_query_string(params)
  expected <- "?query=R%3ATSLA.O%20AND%20Language%3AEN&limit=5&dateFrom=2023-01-01T00%3A00%3A00Z"
  expect_equal(qs, expected)
})

test_that("build_get_query_string errors if any element is unnamed", {
  params <- list("R:TSLA.O", limit = 5)
  expect_error(build_get_query_string(params), "All elements of the list must be named")
})
