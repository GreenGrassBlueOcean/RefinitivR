###############################################################################
# Load the function definition (adjust the path as needed)
###############################################################################
library(mockery)

test_that("rd_get_top_news returns a data.frame with expected fields for a single story", {
  # Dummy response with one group "Main" and one page "Front Page".
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
  expect_equal(result$storyId[1], "S1")
  expect_equal(result$title[1], "Headline 1")
  expect_equal(result$snippet[1], "Summary 1")
  expect_equal(result$storyId[2], "S2")
  expect_equal(result$title[2], "Headline 2")
  expect_equal(result$snippet[2], "Summary 2")
})

test_that("rd_get_top_news returns union of group and page filters", {
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    # Simulate three groups and pages.
    if (EndPoint == "data/news/v1/top-news") {
      return(list(
        data = list(
          # Group "Main" with page "Central Banks & Global Economy"
          list(
            name = "Main",
            pages = list(
              list(name = "Central Banks & Global Economy", po = "PO1",
                   revisionId = "R1", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN1")
            )
          ),
          # Group "Companies" with page "U.S. Companies"
          list(
            name = "Companies",
            pages = list(
              list(name = "U.S. Companies", po = "PO2",
                   revisionId = "R2", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN2")
            )
          ),
          # Group "Breakingviews" with page "Reuters Breakingviews"
          list(
            name = "Breakingviews",
            pages = list(
              list(name = "Reuters Breakingviews", po = "PO3",
                   revisionId = "R3", revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN3")
            )
          )
        )
      ))
    }
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

  # Use union filtering: group = c("Companies", "Breakingviews") OR page = c("Central Banks & Global Economy")
  res <- rd_get_top_news(group = c("Companies", "Breakingviews"),
                         page = c("Central Banks & Global Economy"),
                         debug = FALSE)
  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")
  expect_s3_class(res, "data.frame")
  expect_true(all(expected_cols %in% names(res)))
  # Expect union: TN1 (from Main, matches page filter), TN2 (from Companies), and TN3 (from Breakingviews)
  expect_true(all(res$topNewsId %in% c("TN1", "TN2", "TN3")))
  expect_equal(sort(res$storyId), sort(c("S1", "S2", "S3")))
  expect_equal(nrow(res), 3)
})

test_that("rd_get_top_news filters by group and page regex correctly with vector inputs", {
  # This test simulates a case where a single page matches both filters.
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    # Return one group "Main" with one page "Front Page".
    if (EndPoint == "data/news/v1/top-news") {
      return(list(
        data = list(
          list(
            name = "Main",
            pages = list(
              list(name = "Front Page", topNewsId = "TN1", po = "PO1",
                   revisionId = "R1", revisionDate = "2025-03-14T13:37:16.000Z")
            )
          )
        )
      ))
    }
    if (grepl("TN1", EndPoint)) {
      return(list(data = list(storyId = "S1", text = "Headline 1", snippet = "Summary 1")))
    }
  }
  stub(rd_get_top_news, "send_json_request", dummy_send_json_request)

  # Both filters match the same row.
  res <- rd_get_top_news(group = "Main", page = "Front Page", debug = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(res$group, "Main")
  expect_equal(res$page_name, "Front Page")
})

test_that("rd_get_top_news does NOT re-download the same topNewsId multiple times", {
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
              list(name = "Front Page", topNewsId = "TN1", po = "PO1",
                   revisionId = "R1", revisionDate = "2025-03-12T00:00:00Z"),
              list(name = "Alternate Page", topNewsId = "TN1", po = "PO4",
                   revisionId = "R4", revisionDate = "2025-03-12T00:00:00Z")
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
  expect_equal(nrow(res), 2)
  expect_equal(call_counts$TN1, 1,
               info = "Details for TN1 should only be downloaded once even if referenced by multiple pages.")
})
