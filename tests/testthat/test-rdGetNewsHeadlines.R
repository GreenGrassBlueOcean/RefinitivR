library(testthat)
library(data.table)

# Dummy function to simulate a response for rd_get_news_headlines
fake_rd_get_news_headlines <- function(RDObject, query, limit, sort, relevancy,
                                       cursor, dateFrom, dateTo, raw_output, debug) {
  # Simulate a response containing one headline JSON object
  return(list(data = list(
    list(
      storyId = "dummy_story",
      newsItem = list(
        `_version` = 1L,
        contentMeta = list(
          creator = list(list(`_qcode` = "NS:RTRS", `_role` = "source")),
          infoSource = list(list(`_qcode` = "NS:RTRS", `_role` = "source")),
          language = list(list(`_tag` = "en")),
          subject = list(list(`_qcode` = "G:1"), list(`_qcode` = "M:1QD")),
          urgency = list(`$` = 3L)
        ),
        itemMeta = list(
          firstCreated = list(`$` = "2025-03-12T15:55:31.127Z"),
          versionCreated = list(`$` = "2025-03-12T15:55:31.127Z"),
          title = list(list(`$` = "Dummy headline"))
        )
      )
    )
  )))
}

# Create a dummy RDObject with the fake method
dummyRD <- list(rd_get_news_headlines = fake_rd_get_news_headlines)

test_that("rd_get_news_headlines returns a flattened data.frame", {
  result <- rd_get_news_headlines(RDObject = dummyRD, query = "MSFT.O",
                                  limit = 2, raw_output = FALSE, debug = FALSE)

  # Expected columns as per documentation:
  expected_cols <- c("storyId", "version", "urgency", "firstCreated", "versionCreated",
                     "title", "creator", "infoSource", "language", "subject", "query")

  expect_true(is.data.frame(result))
  expect_true(all(expected_cols %in% names(result)))

  # Verify that flattened fields match our dummy data:
  expect_equal(result$storyId[1], "dummy_story")
  expect_equal(result$title[1], "Dummy headline")
  expect_equal(result$creator[1], "NS:RTRS")
  expect_equal(result$infoSource[1], "NS:RTRS")
  expect_equal(result$language[1], "en")
  expect_equal(result$subject[1], "G:1,M:1QD")
  expect_equal(result$query[1], "MSFT.O")
})

test_that("rd_get_news_headlines returns raw output when raw_output is TRUE", {
  raw_result <- rd_get_news_headlines(RDObject = dummyRD, query = "MSFT.O",
                                      limit = 2, raw_output = TRUE, debug = FALSE)

  expect_true(is.list(raw_result))
  # Check that the first element in the raw output contains a 'data' field.
  expect_true("data" %in% names(raw_result[[1]]))
})

test_that("rd_get_news_headlines errors when limit exceeds 100", {
  expect_error(
    rd_get_news_headlines(query = "MSFT.O", limit = 101),
    "rd_get_news_headlines: 'limit' cannot exceed 100."
  )
})

###############################################################################
# 3) Updated Tests for rd_get_top_news with Union Logic for Group and Page Filters
###############################################################################

library(testthat)
library(mockery)

test_that("rd_get_top_news returns a data.frame with expected fields for a single story", {
  # Dummy send_json_request to simulate a simple response.
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    if (EndPoint == "data/news/v1/top-news") {
      # Simulate a single group "Main" with one page "Front Page"
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
    # For details endpoint, simulate a single story.
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
      # Simulate a single group "Main" with one page "Front Page"
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
    # For details endpoint, simulate multiple stories.
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

  # Check details for each story.
  expect_equal(result$storyId[1], "S1")
  expect_equal(result$title[1], "Headline 1")
  expect_equal(result$snippet[1], "Summary 1")
  expect_equal(result$storyId[2], "S2")
  expect_equal(result$title[2], "Headline 2")
  expect_equal(result$snippet[2], "Summary 2")
})

test_that("rd_get_top_news returns union of group and page filters", {
  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    # Simulate multiple groups and pages.
    if (EndPoint == "data/news/v1/top-news") {
      return(list(
        data = list(
          # Group "Main" with page "Central Banks & Global Economy"
          list(
            name = "Main",
            pages = list(
              list(name = "Central Banks & Global Economy", po = "PO1", revisionId = "R1",
                   revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN1")
            )
          ),
          # Group "Companies" with page "U.S. Companies"
          list(
            name = "Companies",
            pages = list(
              list(name = "U.S. Companies", po = "PO2", revisionId = "R2",
                   revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN2")
            )
          ),
          # Group "Breakingviews" with page "Reuters Breakingviews"
          list(
            name = "Breakingviews",
            pages = list(
              list(name = "Reuters Breakingviews", po = "PO3", revisionId = "R3",
                   revisionDate = "2025-03-14T13:37:16.000Z", topNewsId = "TN3")
            )
          )
        )
      ))
    }
    # For details endpoints, return one story per topNewsId.
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

  # Using union logic: group = c("Companies", "Breakingviews") OR page = c("Central Banks & Global Economy")
  res <- rd_get_top_news(group = c("Companies", "Breakingviews"),
                         page = c("Central Banks & Global Economy"),
                         debug = FALSE)

  expected_cols <- c("group", "page_name", "po", "revisionId", "revisionDate",
                     "topNewsId", "storyId", "title", "snippet")
  expect_s3_class(res, "data.frame")
  expect_true(all(expected_cols %in% names(res)))
  # Expect union: TN1 (from Main, matches page filter), TN2 (from Companies), and TN3 (from Breakingviews).
  expect_true(all(res$topNewsId %in% c("TN1", "TN2", "TN3")))
  expect_equal(sort(res$storyId), sort(c("S1", "S2", "S3")))
  # Also check that we have three rows.
  expect_equal(nrow(res), 3)
})

test_that("rd_get_top_news does NOT re-download the same topNewsId multiple times", {
  # Track download calls.
  call_counts <- new.env(parent = emptyenv())
  call_counts$TN1 <- 0

  dummy_send_json_request <- function(..., service, request_type, EndPoint, debug) {
    if (EndPoint == "data/news/v1/top-news") {
      # Two pages refer to the same topNewsId ("TN1").
      return(list(
        data = list(
          list(
            name = "Main",
            pages = list(
              list(name = "Front Page",   topNewsId = "TN1", po = "PO1", revisionId = "R1",
                   revisionDate = "2025-03-12T00:00:00Z"),
              list(name = "Alternate Page", topNewsId = "TN1", po = "PO4", revisionId = "R4",
                   revisionDate = "2025-03-12T00:00:00Z")
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
  # Two rows (because two pages reference the same TN1) but detail call should be made only once.
  expect_equal(nrow(res), 2)
  expect_equal(call_counts$TN1, 1,
               info = "Details for TN1 should only be downloaded once even if referenced by multiple pages.")
})
