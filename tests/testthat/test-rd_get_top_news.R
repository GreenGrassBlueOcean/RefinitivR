###############################################################################
# 3) Tests for rd_get_top_news using mockery::stub()
###############################################################################

library(testthat)
library(mockery)

test_that("rd_get_top_news returns a data.frame with expected fields for a single story", {
  # Dummy send_json_request to simulate responses.
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
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
    # For top news details endpoint (single story scenario)
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

  # Override send_json_request in rd_get_top_news.
  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  result <- rd_get_top_news(raw_output = FALSE, debug = FALSE)

  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")

  expect_s3_class(result, "data.frame")
  expect_true(all(expected_cols %in% names(result)))

  # Should return one row because details response is a single story.
  expect_equal(nrow(result), 1)

  expect_equal(result$group, "Main")
  expect_equal(result$page_name, "Front Page")
  expect_equal(result$po, "PO1")
  expect_equal(result$revisionId, "R1")
  expect_equal(result$revisionDate, "2025-03-12T00:00:00Z")
  expect_equal(result$topNewsId, "TN1")
  expect_equal(result$storyId, "S1")
  expect_equal(result$title, "Top Headline")
  expect_equal(result$snippet, "Top Summary")
})

test_that("rd_get_top_news expands multiple stories into separate rows", {
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
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
    # For top news details endpoint (multiple stories scenario)
    if (grepl("data/news/v1/top-news/", EndPoint)) {
      return(list(
        data = list(
          list(storyId = "S1", text = "Headline 1", snippet = "Summary 1"),
          list(storyId = "S2", text = "Headline 2", snippet = "Summary 2")
        )
      ))
    }
  }

  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  result <- rd_get_top_news(raw_output = FALSE, debug = FALSE)

  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")

  expect_s3_class(result, "data.frame")
  expect_true(all(expected_cols %in% names(result)))

  # Should return two rows because details response contains two stories.
  expect_equal(nrow(result), 2)

  # Verify first story.
  expect_equal(result$group[1], "Main")
  expect_equal(result$page_name[1], "Front Page")
  expect_equal(result$po[1], "PO1")
  expect_equal(result$revisionId[1], "R1")
  expect_equal(result$revisionDate[1], "2025-03-12T00:00:00Z")
  expect_equal(result$topNewsId[1], "TN1")
  expect_equal(result$storyId[1], "S1")
  expect_equal(result$title[1], "Headline 1")
  expect_equal(result$snippet[1], "Summary 1")

  # Verify second story.
  expect_equal(result$group[2], "Main")
  expect_equal(result$page_name[2], "Front Page")
  expect_equal(result$po[2], "PO1")
  expect_equal(result$revisionId[2], "R1")
  expect_equal(result$revisionDate[2], "2025-03-12T00:00:00Z")
  expect_equal(result$topNewsId[2], "TN1")
  expect_equal(result$storyId[2], "S2")
  expect_equal(result$title[2], "Headline 2")
  expect_equal(result$snippet[2], "Summary 2")
})
