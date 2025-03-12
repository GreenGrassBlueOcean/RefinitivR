test_that("rd_get_news_story returns correct story HTML", {
  # Dummy function to simulate a successful story retrieval.
  dummy_rd_get_news_story <- function(story_id, raw_output, debug) {
    # Simulate a legacy response with storyHtml.
    list(story = list(storyHtml = paste("HTML for", story_id)))
  }

  # Create a dummy RDObject with the dummy method.
  dummy_RDObject <- list(rd_get_news_story = dummy_rd_get_news_story)

  # Call rd_get_news_story using the dummy object.
  result <- rd_get_news_story(
    RDObject = dummy_RDObject,
    story_id = "dummy_story",
    raw_output = FALSE,
    debug = FALSE,
    renderHTML = FALSE
  )

  # Check that the result is a character string with the expected content.
  expect_type(result, "character")
  expect_equal(result, "HTML for dummy_story")
})

test_that("rd_get_news_story returns correct story HTML", {
  # Dummy function to simulate a successful story retrieval.
  dummy_rd_get_news_story <- function(story_id, raw_output, debug) {
    # Simulate a legacy response with storyHtml.
    list(story = list(storyHtml = paste("HTML for", story_id)))
  }

  # Create a dummy RDObject with the dummy method.
  dummy_RDObject <- list(rd_get_news_story = dummy_rd_get_news_story)

  # Call rd_get_news_story using the dummy object.
  result <- rd_get_news_story(
    RDObject = dummy_RDObject,
    story_id = "dummy_story",
    raw_output = FALSE,
    debug = FALSE,
    renderHTML = FALSE
  )

  # Check that the result is a character string with the expected content.
  expect_type(result, "character")
  expect_equal(result, "HTML for dummy_story")
})

library(mockery)

test_that("rd_get_top_news returns a data.frame with expected fields", {
  # Dummy send_json_request to simulate responses based on EndPoint.
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

  # Stub send_json_request within rd_get_top_news to use our dummy function.
  # This replaces calls to send_json_request inside rd_get_top_news.
  mockery::stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

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
