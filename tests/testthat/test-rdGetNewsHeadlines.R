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
