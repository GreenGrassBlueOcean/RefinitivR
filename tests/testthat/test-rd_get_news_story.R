library(testthat)
library(mockery)

# A dummy function that behaves differently based on story_id.
# This allows us to exercise all the branches in rd_get_news_story.
fake_rd_get_news_story <- function(story_id, raw_output, debug) {
  switch(story_id,
         "legacy_story" = list(story = list(storyHtml = "<p>Legacy HTML content</p>")),
         "web_url"      = list(webURL = "https://example.com/story"),
         "inline_xml"   = list(newsItem = list(contentSet = list(
           inlineXML = list(`$` = "<p>Inline XML</p>")
         ))),
         "inline_data"  = list(newsItem = list(contentSet = list(
           inlineData = list(`$` = "<p>Inline Data</p>")
         ))),
         "empty_newsItem" = list(newsItem = list(contentSet = list())),
         # Return a minimal list that doesn't contain expected keys
         "not_a_list" = list(unrecognizedField = TRUE)
  )
}

###############################################################################
# Test 1: Error when story_id is NULL
###############################################################################
test_that("rd_get_news_story errors if story_id is NULL", {
  expect_error(
    rd_get_news_story(RDObject = list(), story_id = NULL),
    "rd_get_news_story: must supply 'story_id'"
  )
})

###############################################################################
# Test 2: Error when retries exceed allowed limit
###############################################################################
test_that("rd_get_news_story errors after multiple retries when retrieval fails", {
  # Create a dummy function that always fails.
  always_fail_rd_get_news_story <- function(story_id, raw_output, debug) {
    stop("Simulated retrieval failure")
  }
  RDObj_fail <- list(rd_get_news_story = always_fail_rd_get_news_story)

  # Override retry() so that it always returns a try-error.
  with_mocked_bindings({
    expect_error(
      rd_get_news_story(RDObject = RDObj_fail, story_id = "any_story", debug = FALSE),
      "rd_get_news_story: retrieving data failed after multiple retries."
    )
  }, retry = function(expr, max) try(expr, silent = TRUE))
})

###############################################################################
# Test 3: Raw output returns the raw list
###############################################################################
dummy_raw_rd_get_news_story <- function(story_id, raw_output, debug) {
  # Return a known list element so we can verify raw output.
  list(raw = paste("raw", story_id))
}

test_that("rd_get_news_story returns raw output when raw_output is TRUE", {
  RDObj_raw <- list(rd_get_news_story = dummy_raw_rd_get_news_story)
  raw_out <- rd_get_news_story(RDObject = RDObj_raw,
                               story_id = "test_story",
                               raw_output = TRUE,
                               debug = FALSE)
  expect_true(is.list(raw_out))
  expect_equal(raw_out[[1]]$raw, "raw test_story")
})

###############################################################################
# Test 4: Legacy branch returns correct HTML without %||%
###############################################################################
dummy_legacy_rd_get_news_story <- function(story_id, raw_output, debug) {
  # Simulate a legacy response with storyHtml.
  list(story = list(storyHtml = paste("HTML for", story_id)))
}

test_that("rd_get_news_story returns legacy story HTML correctly", {
  RDObj_legacy <- list(rd_get_news_story = dummy_legacy_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj_legacy,
                              story_id = "dummy_story",
                              raw_output = FALSE,
                              debug = FALSE,
                              renderHTML = FALSE)
  expect_type(result, "character")
  expect_equal(result, "HTML for dummy_story")
})

###############################################################################
# Test 5: Other branches in the vapply
###############################################################################
test_that("rd_get_news_story handles 'webURL' branch correctly", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "web_url", debug = FALSE)
  expect_equal(result, "https://example.com/story")
})

test_that("rd_get_news_story handles newsItem inlineXML branch correctly", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "inline_xml", debug = FALSE)
  expect_equal(result, "<p>Inline XML</p>")
})

test_that("rd_get_news_story handles newsItem inlineData branch correctly", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "inline_data", debug = FALSE)
  expect_equal(result, "<p>Inline Data</p>")
})

test_that("rd_get_news_story returns empty string for empty newsItem", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "empty_newsItem", debug = FALSE)
  expect_equal(result, "")
})

test_that("rd_get_news_story returns empty string when no recognized fields", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "not_a_list", debug = FALSE)
  expect_equal(result, "")
})

###############################################################################
# Test 6: Handling multiple story IDs
###############################################################################
test_that("rd_get_news_story handles multiple story IDs at once", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  stories <- rd_get_news_story(RDObject = RDObj,
                               story_id = c("web_url", "inline_data"),
                               debug = FALSE,
                               renderHTML = FALSE)
  expect_length(stories, 2)
  expect_equal(stories[1], "https://example.com/story")
  expect_equal(stories[2], "<p>Inline Data</p>")
})

###############################################################################
# Test 7: Debug messages are printed when debug = TRUE
###############################################################################
test_that("rd_get_news_story prints debug messages when debug = TRUE", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  expect_message(
    rd_get_news_story(RDObject = RDObj,
                      story_id = "inline_xml",
                      debug = TRUE,
                      renderHTML = FALSE),
    "Download Status:"
  )
})

###############################################################################
# Test 8: rd_get_news_story handles renderHTML = TRUE
###############################################################################

test_that("rd_get_news_story handles renderHTML = TRUE branch", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  mock_viewer <- mock()

  with_mocked_bindings(
    code = {
      result <- rd_get_news_story(
        RDObject   = RDObj,
        story_id   = c("inline_xml", "legacy_story"),
        debug      = TRUE,
        renderHTML = TRUE
      )
      # For "inline_xml", fake_rd_get_news_story returns "<p>Inline XML</p>"
      # For "legacy_story", it returns "<p>Legacy HTML content</p>"
      # They will be joined by "<hr/>" (via paste0) and then passed through make_links_clickable.
      # Since there are no URL substrings in the expected text, the output should be identical.
      expected <- paste0(c("<p>Inline XML</p>", "<p>Legacy HTML content</p>"), collapse = "<hr/>")

      expect_true(grepl("<p>Inline XML</p>", result))
      expect_true(grepl("<p>Legacy HTML content</p>", result))
      expect_true(grepl("<hr/>", result))

      # Check that the viewer was called (since rstudioapi::hasFun returns TRUE)
      expect_called(mock_viewer, 1)
    },
    .package = "rstudioapi",
    hasFun = function(name) TRUE,
    viewer = mock_viewer
  )
})
