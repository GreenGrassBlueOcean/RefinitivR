# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

test_that("EikonGetNewsHeadlines works with live API", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  CheckHeadlines <- EikonGetNewsHeadlines(
    EikonObject = Eikon,
    query = "R:MSFT.O", count = 2
  )

  expect_s3_class(CheckHeadlines, "data.frame")
  expect_true("storyId" %in% names(CheckHeadlines))
  expect_true("text" %in% names(CheckHeadlines))
  expect_true(nrow(CheckHeadlines) > 0)
})


library(testthat)
library(mockery)
library(data.table)

context("Testing EikonGetNewsHeadlines")

# Create a dummy headlines response.
# Note: "headlines" is now a list of one data.table.
dummy_headlines <- list(
  headlines = list(
    data.table(
      Index = "2024-01-01T00:00:00",
      version_created = "2024-01-01",
      text = "Test headline",
      story_id = "story123",
      source_code = "SRC1"
    )
  )
)

# Create a dummy EikonObject with a get_news_headlines method that returns dummy_headlines.
dummy_Eikon <- list(
  get_news_headlines = function(query, count, repository, date_from, date_to, raw_output, debug) {
    # Always return the same dummy_headlines for testing purposes.
    dummy_headlines
  }
)

test_that("EikonGetNewsHeadlines returns raw output correctly", {
  # When raw_output is TRUE, the function should return a list of raw responses.
  result <- EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon, query = "R:MSFT.O",
    count = 2, raw_output = TRUE, debug = FALSE
  )
  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_equal(result[[1]], dummy_headlines)
})

test_that("EikonGetNewsHeadlines processes raw output and returns a data.frame", {
  # When raw_output is FALSE, the function should process the raw headlines.
  result <- EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon, query = "R:MSFT.O",
    count = 2, raw_output = FALSE, debug = FALSE
  )
  expect_true(is.data.frame(result))

  # The function assembles the results via data.table::rbindlist with an id column "query".
  # Expected columns: "query" plus the columns from dummy_headlines$headlines[[1]].
  expected_cols <- c("query", names(dummy_headlines$headlines[[1]]))
  expect_equal(sort(names(result)), sort(expected_cols))

  # The query column should equal the provided query.
  expect_equal(result$query, "R:MSFT.O")
})

test_that("EikonGetNewsHeadlines handles multiple query values", {
  # Create a dummy EikonObject that returns different results based on the query.
  dummy_Eikon_multi <- list(
    get_news_headlines = function(query, count, repository, date_from, date_to, raw_output, debug) {
      if (query == "R:MSFT.O") {
        list(headlines = list(
          data.table(
            Index = "2024-01-01T00:00:00",
            version_created = "2024-01-01",
            text = "MSFT headline",
            story_id = "story_msft",
            source_code = "SRC1"
          )
        ))
      } else if (query == "R:AAPL.O") {
        list(headlines = list(
          data.table(
            Index = "2024-01-02T00:00:00",
            version_created = "2024-01-02",
            text = "AAPL headline",
            story_id = "story_aapl",
            source_code = "SRC2"
          )
        ))
      } else {
        stop("Unknown query")
      }
    }
  )
  queries <- c("R:MSFT.O", "R:AAPL.O")
  result <- EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon_multi, query = queries,
    count = 2, raw_output = FALSE, debug = FALSE
  )
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$query), sort(queries))
})

test_that("EikonGetNewsHeadlines stops after exceeding retry limit", {
  # Simulate failures: get_news_headlines always errors.
  failing_Eikon <- list(
    get_news_headlines = function(query, count, repository, date_from, date_to, raw_output, debug) {
      stop("simulated API failure")
    }
  )
  expect_error(
    suppressWarnings(suppressMessages(
      EikonGetNewsHeadlines(
        EikonObject = failing_Eikon, query = "R:MSFT.O",
        count = 2, raw_output = TRUE, debug = FALSE
      )
    )),
    "EikonGetNewsHeadlines downloading data failed"
  )
})


restore_refinitiv_state(.saved_state, "test-EikonGetNewsHeadlines")
