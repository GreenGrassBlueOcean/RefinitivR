library(testthat)
library(mockery)

###############################################################################
# 1) Fake Connection Object and Fake Functions
###############################################################################

fakeRD <- list()
class(fakeRD) <- "fakeRD"

fake_rd_get_news_headlines <- function(RDObject, query, limit = 5, ...) {
  if (query == "R:TSLA.O") {
    # Return a "headlines" sub-list
    return(list(
      data = list(
        headlines = list(
          list(
            storyId = "story_tsla",
            title   = list(list(`$` = "Headline for TSLA"))
          )
        )
      )
    ))
  } else if (query == "R:MSFT.O") {
    # Return a "direct data" structure
    return(list(
      data = list(
        list(
          storyId = "story_msft",
          title   = list(list(`$` = "Direct headline for MSFT"))
        )
      )
    ))
  } else {
    # Return an empty result
    return(list(data = list()))
  }
}

fake_rd_get_news_story <- function(RDObject,
                                   story_id,
                                   raw_output  = FALSE,
                                   debug       = FALSE,
                                   renderHTML  = FALSE) {
  # If multiple story IDs, return a list (raw_output=TRUE) or a character vector.
  if (length(story_id) > 1) {
    if (raw_output) {
      return(lapply(story_id, function(sid) fake_rd_get_news_story(RDObject, sid, TRUE)))
    } else {
      return(sapply(story_id, function(sid) {
        single <- fake_rd_get_news_story(RDObject, sid, FALSE)
        if (is.character(single)) single else ""
      }))
    }
  }
  # Single ID logic
  if (story_id == "dummy_id") {
    return("http://example.com")
  } else if (story_id == "plain_url") {
    return("https://example.com/story")
  } else if (story_id == "html_story") {
    return("<p>This is a story with <strong>HTML</strong></p>")
  } else {
    return("")
  }
}

###############################################################################
# 2) Tests for rd_get_news_story
###############################################################################

test_that("rd_get_news_story returns correct story HTML", {
  # Create a dummy RDObject with the fake_rd_get_news_story method.
  dummy_RDObject <- list(rd_get_news_story = fake_rd_get_news_story)

  # Call rd_get_news_story using the dummy object.
  result <- rd_get_news_story(
    RDObject = dummy_RDObject,
    story_id = "dummy_id",
    raw_output = FALSE,
    debug = FALSE,
    renderHTML = FALSE
  )

  # Expect the result to be a character string with the expected content.
  expect_type(result, "character")
  expect_equal(result, "http://example.com")
})

###############################################################################
# 3) Tests for rd_get_top_news using mockery::stub()
###############################################################################

test_that("rd_get_top_news returns a data.frame with expected fields", {
  # Dummy send_json_request to simulate responses based on the EndPoint.
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    # When called on the top-news endpoint.
    if (EndPoint == "data/news/v1/top-news") {
      return(list(
        data = list(
          list(
            name = "Main",
            pages = list(
              list(
                name = "Front Page",
                po = "PO1",
                revisionId = "R1",
                revisionDate = "2025-03-12T00:00:00Z",
                topNewsId = "TN1"
              )
            )
          )
        )
      ))
    }
    # When called for a specific top news detail (look for TN1 in EndPoint).
    if (grepl("data/news/v1/top-news/", EndPoint)) {
      return(list(
        data = list(
          storyId = "S1",
          text = "Top Headline",
          snippet = "Top Summary"
        )
      ))
    }
  }

  # Use mockery::stub to override send_json_request inside rd_get_top_news.
  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  # Call rd_get_top_news with the dummy responses.
  result <- rd_get_top_news(raw_output = FALSE, debug = FALSE)

  # Define the expected columns as per documentation.
  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")

  # Check that the output is a data.frame.
  expect_s3_class(result, "data.frame")

  # Check that all expected columns exist.
  expect_true(all(expected_cols %in% names(result)))

  # Verify that the dummy values are present.
  expect_equal(result$group, "Main")
  expect_equal(result$page_name, "Front Page")
  expect_equal(result$po, "PO1")
  expect_equal(result$revisionId, "R1")
  expect_equal(result$topNewsId, "TN1")
  expect_equal(result$storyId, "S1")
  expect_equal(result$title, "Top Headline")
  expect_equal(result$snippet, "Top Summary")
})
