library(testthat)
library(mockery)
library(data.table)

# Create a dummy Eikon object with a stubbed get_news_story method.
dummy_Eikon <- list(
  get_news_story = function(story_id, debug, raw_output) {
    # Simulate different responses based on the story_id.
    if (story_id == "story1") {
      return(list(story = list(
        headlineHtml = "<h1>Headline 1</h1>",
        storyHtml = "<div>Story content 1</div>",
        storyInfoHtml = "<p>Info 1</p>"
      )))
    } else if (story_id == "story2") {
      return(list(webURL = "https://example.com/story2"))
    } else {
      stop("Story not found")
    }
  }
)

test_that("EikonGetNewsStory errors if story_id is NULL", {
  expect_error(
    EikonGetNewsStory(story_id = NULL, EikonObject = dummy_Eikon),
    "Parameter story_id has to be supplied and cannot be empty"
  )
})

test_that("EikonGetNewsStory returns raw output correctly", {
  # raw_output = TRUE should return the raw response list.
  result <- EikonGetNewsStory(story_id = "story1", EikonObject = dummy_Eikon, raw_output = TRUE)
  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_true("story" %in% names(result[[1]]))
})

test_that("EikonGetNewsStory processes output correctly for story with 'story' element", {
  # raw_output = FALSE: for a response that contains a "story" element,
  # the function should return x$story$storyHtml.
  result <- EikonGetNewsStory(story_id = "story1", EikonObject = dummy_Eikon, raw_output = FALSE)
  expect_type(result, "character")
  expect_equal(result, "<div>Story content 1</div>")
})

test_that("EikonGetNewsStory processes output correctly for story with 'webURL'", {
  # raw_output = FALSE: for a response that contains "webURL", return that value.
  result <- EikonGetNewsStory(story_id = "story2", EikonObject = dummy_Eikon, raw_output = FALSE)
  expect_type(result, "character")
  expect_equal(result, "https://example.com/story2")
})

test_that("EikonGetNewsStory can handle multiple story ids", {
  # When multiple story IDs are provided, the function should return a vector
  # with each element processed appropriately.
  result <- EikonGetNewsStory(
    story_id = c("story1", "story2"),
    EikonObject = dummy_Eikon,
    raw_output = FALSE
  )
  expect_type(result, "character")
  expect_equal(length(result), 2)
  expect_equal(result[1], "<div>Story content 1</div>")
  expect_equal(result[2], "https://example.com/story2")
})


library(testthat)
library(mockery)
library(data.table)

context("Testing EikonGetNewsStory renderHTML branch")

# Create a dummy Eikon object with a stubbed get_news_story method.
dummy_Eikon <- list(
  get_news_story = function(story_id, debug, raw_output) {
    if (story_id == "story1") {
      return(list(story = list(
        headlineHtml   = "<h1>Headline 1</h1>",
        storyHtml      = "<div>Story content 1</div>",
        storyInfoHtml  = "<p>Info 1</p>"
      )))
    } else {
      stop("Story not found")
    }
  }
)

# Test when RStudio viewer is available.
test_that("EikonGetNewsStory renders HTML using RStudio viewer when available", {
  # We'll capture the file path passed to rstudioapi::viewer.
  captured_viewer <- NULL

  # Stub rstudioapi::hasFun to return TRUE.
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) TRUE)
  # Stub rstudioapi::viewer to capture its argument.
  stub(EikonGetNewsStory, "rstudioapi::viewer", function(file) { captured_viewer <<- file })
  # Stub utils::browseURL to do nothing.
  stub(EikonGetNewsStory, "utils::browseURL", function(url) NULL)

  # Call the function with renderHTML = TRUE.
  result <- EikonGetNewsStory(EikonObject = dummy_Eikon, story_id = "story1",
                              raw_output = FALSE, debug = TRUE, renderHTML = TRUE)

  # Since raw_output is FALSE and renderHTML = TRUE, the function returns the processed news content.
  # We expect result to be equal to the storyHtml extracted.
  expect_type(result, "character")
  expect_equal(result, "<div>Story content 1</div>")

  # Check that rstudioapi::viewer was called with a non-empty file path.
  expect_true(is.character(captured_viewer))
  expect_true(nchar(captured_viewer) > 0)

  # Optionally, you can check that the captured path ends with "index.html"
  expect_true(grepl("index\\.html$", captured_viewer))
})

# Test when RStudio viewer is not available (fallback to default browser).
test_that("EikonGetNewsStory renders HTML using default browser when RStudio viewer not available", {
  captured_url <- NULL

  # Stub rstudioapi::hasFun to return FALSE.
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) FALSE)
  # Stub utils::browseURL to capture its argument.
  stub(EikonGetNewsStory, "utils::browseURL", function(url) { captured_url <<- url })

  # Call the function with renderHTML = TRUE.
  result <- EikonGetNewsStory(EikonObject = dummy_Eikon, story_id = "story1",
                              raw_output = FALSE, debug = TRUE, renderHTML = TRUE)

  expect_type(result, "character")
  expect_equal(result, "<div>Story content 1</div>")

  expect_true(is.character(captured_url))
  expect_true(nchar(captured_url) > 0)
  expect_true(grepl("index\\.html$", captured_url))
})

