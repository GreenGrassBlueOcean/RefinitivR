###############################################################################
# 3) Updated Tests for rd_get_top_news with Vector Filtering Support
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

  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  result <- rd_get_top_news(raw_output = FALSE, debug = FALSE)

  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")

  expect_s3_class(result, "data.frame")
  expect_true(all(expected_cols %in% names(result)))
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
  expect_equal(nrow(result), 2)

  # Verify first story.
  expect_equal(result$storyId[1], "S1")
  expect_equal(result$title[1], "Headline 1")
  expect_equal(result$snippet[1], "Summary 1")

  # Verify second story.
  expect_equal(result$storyId[2], "S2")
  expect_equal(result$title[2], "Headline 2")
  expect_equal(result$snippet[2], "Summary 2")
})

test_that("rd_get_top_news filters by group/page regex correctly with vector inputs", {
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    # Provide multiple groups and pages
    if (EndPoint == "data/news/v1/top-news") {
      return(list(
        data = list(
          list(
            name = "Main",
            pages = list(
              list(name = "Front Page",   topNewsId = "TN1", po = "PO1", revisionId = "R1", revisionDate = "2025-03-12T00:00:00Z"),
              list(name = "Second Page",  topNewsId = "TN2", po = "PO2", revisionId = "R2", revisionDate = "2025-03-12T00:00:00Z")
            )
          ),
          list(
            name = "Sports & Lifestyle",
            pages = list(
              list(name = "Sport", topNewsId = "TN3", po = "PO3", revisionId = "R3", revisionDate = "2025-03-12T00:00:00Z")
            )
          )
        )
      ))
    }
    # For each topNewsId, return a single story.
    if (grepl("TN1", EndPoint)) {
      return(list(data = list(storyId = "S1", text = "Headline 1", snippet = "Summary 1")))
    }
    if (grepl("TN2", EndPoint)) {
      return(list(data = list(storyId = "S2", text = "Headline 2", snippet = "Summary 2")))
    }
    if (grepl("TN3", EndPoint)) {
      return(list(data = list(storyId = "S3", text = "Headline 3", snippet = "Summary 3")))
    }
  }
  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  # Filter: group = "Main" and page = "Front Page"
  res <- rd_get_top_news(group = "^Main$", page = "^Front Page$", debug = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(res$group, "Main")
  expect_equal(res$page_name, "Front Page")
})

test_that("rd_get_top_news vector filtering for page works correctly", {
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    if (EndPoint == "data/news/v1/top-news") {
      # Simulate multiple groups with various pages.
      return(list(
        data = list(
          list(
            name = "Main",
            pages = list(
              list(name = "Central Banks & Global Economy", po = "PO1", revisionId = "R1", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN1"),
              list(name = "Other Page", po = "PO2", revisionId = "R2", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN6")
            )
          ),
          list(
            name = "Breakingviews",
            pages = list(
              list(name = "Reuters Breakingviews", po = "PO3", revisionId = "R3", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN2")
            )
          ),
          list(
            name = "Companies",
            pages = list(
              list(name = "U.S. Companies", po = "PO4", revisionId = "R4", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN3"),
              list(name = "European Companies", po = "PO5", revisionId = "R5", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN4"),
              list(name = "Asian Companies", po = "PO6", revisionId = "R6", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN5")
            )
          ),
          list(
            name = "Sports & Lifestyle",
            pages = list(
              list(name = "Sport", po = "PO7", revisionId = "R7", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN7")
            )
          )
        )
      ))
    }
    # Return details for each topNewsId
    if (grepl("TN1", EndPoint)) {
      return(list(data = list(storyId = "S1", text = "Headline 1", snippet = "Summary 1")))
    }
    if (grepl("TN2", EndPoint)) {
      return(list(data = list(storyId = "S2", text = "Headline 2", snippet = "Summary 2")))
    }
    if (grepl("TN3", EndPoint)) {
      return(list(data = list(storyId = "S3", text = "Headline 3", snippet = "Summary 3")))
    }
    if (grepl("TN4", EndPoint)) {
      return(list(data = list(storyId = "S4", text = "Headline 4", snippet = "Summary 4")))
    }
    if (grepl("TN5", EndPoint)) {
      return(list(data = list(storyId = "S5", text = "Headline 5", snippet = "Summary 5")))
    }
    if (grepl("TN6", EndPoint)) {
      return(list(data = list(storyId = "S6", text = "Headline 6", snippet = "Summary 6")))
    }
    if (grepl("TN7", EndPoint)) {
      return(list(data = list(storyId = "S7", text = "Headline 7", snippet = "Summary 7")))
    }
  }
  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  # Use a vector of pages for filtering.
  res <- rd_get_top_news(page = c(
    "Central Banks & Global Economy",
    "Reuters Breakingviews",
    "U\\.S\\. Companies",
    "European Companies",
    "Asian Companies"
  ), debug = FALSE)

  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")
  expect_s3_class(res, "data.frame")
  expect_true(all(expected_cols %in% names(res)))
  # We expect only rows corresponding to TN1, TN2, TN3, TN4, and TN5.
  expect_true(all(res$topNewsId %in% c("TN1", "TN2", "TN3", "TN4", "TN5")))
  expect_equal(res$storyId, c("S1", "S2", "S3", "S4", "S5"))
})

test_that("rd_get_top_news does NOT re-download the same topNewsId multiple times", {
  # Track how many times details for a given topNewsId are downloaded.
  call_counts <- new.env(parent = emptyenv())
  call_counts$TN1 <- 0

  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    if (EndPoint == "data/news/v1/top-news") {
      # Two pages refer to the same topNewsId ("TN1")
      return(list(
        data = list(
          list(
            name = "Main",
            pages = list(
              list(name = "Front Page",   topNewsId = "TN1", po = "PO1", revisionId = "R1", revisionDate = "2025-03-12T00:00:00Z"),
              list(name = "Alternate Page", topNewsId = "TN1", po = "PO4", revisionId = "R4", revisionDate = "2025-03-12T00:00:00Z")
            )
          )
        )
      ))
    }
    if (grepl("data/news/v1/top-news/TN1", EndPoint)) {
      call_counts$TN1 <- call_counts$TN1 + 1
      return(list(
        data = list(
          list(storyId = "S1", text = "Headline A", snippet = "Summary A")
        )
      ))
    }
  }

  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  res <- rd_get_top_news(debug = FALSE)
  # Two rows (because two pages reference the same TN1) but the detail call is made only once.
  expect_equal(nrow(res), 2)
  expect_equal(call_counts$TN1, 1,
               info = "Details for TN1 should only be downloaded once even if referenced by multiple pages.")
})
