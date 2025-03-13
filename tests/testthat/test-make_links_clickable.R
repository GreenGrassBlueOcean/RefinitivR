library(testthat)

test_that("make_links_clickable converts a single URL correctly", {
  input <- "Visit http://example.com for more info."
  expected <- "Visit <a href=\"http://example.com\" target=\"_blank\">http://example.com</a><br/> for more info."
  result <- make_links_clickable(input)
  expect_equal(result, expected)
})

test_that("make_links_clickable converts multiple URLs correctly", {
  input <- "Check http://example.com and https://another.com for details."
  expected <- "Check <a href=\"http://example.com\" target=\"_blank\">http://example.com</a><br/> and <a href=\"https://another.com\" target=\"_blank\">https://another.com</a><br/> for details."
  result <- make_links_clickable(input)
  expect_equal(result, expected)
})

test_that("make_links_clickable returns the original string when no URLs are present", {
  input <- "There is no URL here."
  result <- make_links_clickable(input)
  expect_equal(result, input)
})

test_that("make_links_clickable handles empty string input", {
  input <- ""
  result <- make_links_clickable(input)
  expect_equal(result, "")
})

test_that("make_links_clickable handles URL at beginning of string", {
  input <- "https://start.com is at the beginning."
  expected <- "<a href=\"https://start.com\" target=\"_blank\">https://start.com</a><br/> is at the beginning."
  result <- make_links_clickable(input)
  expect_equal(result, expected)
})

test_that("make_links_clickable handles URL adjacent to punctuation", {
  input <- "Visit http://example.com, it's great."
  # Note: the regex does not exclude commas, so the comma becomes part of the URL.
  expected <- "Visit <a href=\"http://example.com,\" target=\"_blank\">http://example.com,</a><br/> it's great."
  result <- make_links_clickable(input)
  expect_equal(result, expected)
})
