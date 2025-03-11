# Dummy connection object for testing
dummy_RD <- new.env()


dummy_RD$rd_get_news_story <- function(story_id, raw_output = FALSE, debug = FALSE) {
  # Return a valid response for "plain_url" and "dummy_id"
  if (story_id %in% c("plain_url", "dummy_id")) {
    return(list(webURL = "https://example.com/story"))
  } else if (story_id == "html_story") {
    return(list(story = list(storyHtml = "<p>This is a story with <strong>HTML</strong></p>")))
  } else {
    # Simulate an error or no data
    return(NA)
  }
}

class(dummy_RD) <- "dummyRDObject"


test_that("rd_get_news_story returns error with missing story_id", {
  expect_error(rd_get_news_story(RDObject = dummy_RD, story_id = NULL),
               "must supply 'story_id'")
})

test_that("rd_get_news_story raw_output returns a list", {
  result <- rd_get_news_story(RDObject = dummy_RD, story_id = c("plain_url", "html_story"),
                              raw_output = TRUE, debug = FALSE)
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("rd_get_news_story processed output returns character vector", {
  result <- rd_get_news_story(RDObject = dummy_RD, story_id = c("plain_url", "html_story"),
                              raw_output = FALSE, debug = FALSE, renderHTML = FALSE)
  expect_type(result, "character")
  expect_equal(result[1], "https://example.com/story")
  expect_equal(result[2], "<p>This is a story with <strong>HTML</strong></p>")
})

test_that("rd_get_news_story renderHTML wraps plain URLs with anchor tags", {
  html_output <- rd_get_news_story(RDObject = dummy_RD, story_id = "plain_url",
                                   raw_output = FALSE, debug = FALSE, renderHTML = TRUE)
  expect_match(html_output, "<a href=\"https://example.com/story\"")
})
