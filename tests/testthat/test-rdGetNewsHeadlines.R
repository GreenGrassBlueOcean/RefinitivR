library(testthat)
library(Refinitiv)  # adjust package name if needed

# Create a dummy RDObject with a get_news_story() method
dummy_RD <- list(
  get_news_story = function(story_id, raw_output, debug) {
    # Return a dummy story that has a plain URL in the storyHtml element.
    # For testing purposes, we simulate two story items.
    list(
      story = list(
        storyHtml = paste0("This is a test story. Visit http://example.com for more info.")
      )
    )
  }
)
class(dummy_RD) <- "dummyRD"

# Define a dummy retry function that simply evaluates its argument
retry <- function(expr, max) {
  force(expr)
}

# Define a dummy %||% operator if not defined:
`%||%` <- function(a, b) if (!is.null(a)) a else b

test_that("rd_get_news_story returns clickable links when renderHTML is TRUE", {
  # Call the function with renderHTML = TRUE using the dummy RDObject.
  result <- rd_get_news_story(
    RDObject   = dummy_RD,
    story_id   = "dummy_id",
    raw_output = FALSE,
    debug      = FALSE,
    renderHTML = TRUE
  )

  # Check that the result is a non-empty character string
  expect_type(result, "character")
  expect_true(nzchar(result))

  # Check that the result contains an anchor tag for the URL "http://example.com"
  expect_match(result, "<a href=\"http://example.com\" target=\"_blank\">http://example.com</a>")

  # Optionally, check that a <br/> is appended after the link
  expect_match(result, "<a href=\"http://example.com\" target=\"_blank\">http://example.com</a><br/>")
})

test_that("rd_get_news_story returns raw output when renderHTML is FALSE", {
  result <- rd_get_news_story(
    RDObject   = dummy_RD,
    story_id   = "dummy_id",
    raw_output = FALSE,
    debug      = FALSE,
    renderHTML = FALSE
  )

  # In non-render mode, the function returns a character vector (one element per story)
  expect_type(result, "character")
  expect_true(length(result) == 1)
  # And the result should NOT contain clickable link markup
  expect_false(grepl("<a href=", result))
})
