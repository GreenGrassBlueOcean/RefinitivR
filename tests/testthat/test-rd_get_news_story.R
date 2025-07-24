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
  retry_count <- 0
  max_retries <- 5

  # Override retry() to simulate multiple failures
  with_mocked_bindings({
    expect_error(
      rd_get_news_story(RDObject = RDObj_fail, story_id = "any_story", debug = FALSE),
      "rd_get_news_story: retrieval failed after maximum retries for ID: any_story"
    )
  }, retry = function(expr, max) {
    retry_count <<- retry_count + 1
    if (retry_count <= max) try(expr, silent = TRUE) else stop("Max retries exceeded")
  })
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
  expect_named(raw_out, "test_story")
  expect_equal(raw_out[["test_story"]]$raw, "raw test_story")
})

###############################################################################
# Test 4: Legacy branch returns correct HTML
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
  expect_type(result, "list")
  expect_named(result, "dummy_story")
  expect_equal(result[["dummy_story"]]$html, "HTML for dummy_story")
  expect_equal(result[["dummy_story"]]$inline, "")
})

###############################################################################
# Test 5: Other branches return correct content
###############################################################################
test_that("rd_get_news_story handles 'webURL' branch correctly", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "web_url", debug = FALSE)
  expect_type(result, "list")
  expect_named(result, "web_url")
  expect_equal(result[["web_url"]]$inline, "https://example.com/story")
  expect_equal(result[["web_url"]]$html, "")
})

test_that("rd_get_news_story handles newsItem inlineXML branch correctly", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "inline_xml", debug = FALSE)
  expect_type(result, "list")
  expect_named(result, "inline_xml")
  expect_equal(result[["inline_xml"]]$html, "<p>Inline XML</p>")
  expect_equal(result[["inline_xml"]]$inline, "")
})

test_that("rd_get_news_story handles newsItem inlineData branch correctly", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "inline_data", debug = FALSE)
  expect_type(result, "list")
  expect_named(result, "inline_data")
  expect_equal(result[["inline_data"]]$inline, "<p>Inline Data</p>")
  expect_equal(result[["inline_data"]]$html, "")
})

test_that("rd_get_news_story returns empty sub-list for empty newsItem", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "empty_newsItem", debug = FALSE)
  expect_type(result, "list")
  expect_named(result, "empty_newsItem")
  expect_equal(result[["empty_newsItem"]]$inline, "")
  expect_equal(result[["empty_newsItem"]]$html, "")
})

test_that("rd_get_news_story returns empty sub-list when no recognized fields", {
  RDObj <- list(rd_get_news_story = fake_rd_get_news_story)
  result <- rd_get_news_story(RDObject = RDObj, story_id = "not_a_list", debug = FALSE)
  expect_type(result, "list")
  expect_named(result, "not_a_list")
  expect_equal(result[["not_a_list"]]$inline, "")
  expect_equal(result[["not_a_list"]]$html, "")
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
  expect_type(stories, "list")
  expect_length(stories, 2)
  expect_named(stories, c("web_url", "inline_data"))
  expect_equal(stories[["web_url"]]$inline, "https://example.com/story")
  expect_equal(stories[["inline_data"]]$inline, "<p>Inline Data</p>")
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
    "Successfully fetched story for ID: inline_xml"
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

###############################################################################
# Test 9: Headline fallback when no content
###############################################################################
test_that("rd_get_news_story falls back to headline when no content", {
  RDObj <- list(rd_get_news_story = function(story_id, raw_output, debug) {
    list(newsItem = list(itemMeta = list(title = list(list(`$` = "Fallback Headline")))))
  })
  result <- rd_get_news_story(RDObject = RDObj, story_id = "fallback_story", debug = FALSE)
  expect_type(result, "list")
  expect_named(result, "fallback_story")
  expect_equal(result[["fallback_story"]]$html, "<h3>Fallback Headline</h3><p>(No full story available)</p>")
  expect_equal(result[["fallback_story"]]$inline, "")
})
