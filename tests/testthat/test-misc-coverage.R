# tests/testthat/test-misc-coverage.R
# Coverage tests for HistoricalPricing.r, rdpsearch.r, EikonNews.R, and
# rd_OutputProcesser edge cases.

library(testthat)
library(mockery)
library(data.table)


# ═══════════════════════════════════════════════════════════════════════
# rd_OutputProcesser: date column edge cases
# ═══════════════════════════════════════════════════════════════════════

test_that("rd_OutputProcesser converts numeric epoch date columns to POSIXct", {
  response <- list(
    headers = list(
      list(name = "Instrument", type = "string", decimalChar = NA),
      list(name = "Trade Date", type = "number", decimalChar = NA),
      list(name = "Value", type = "number", decimalChar = NA)
    ),
    data = list(
      list("AAPL.O", 1672531200000, 100),
      list("AAPL.O", 1672617600000, 101)
    )
  )
  result <- rd_OutputProcesser(response)
  expect_s3_class(result, "data.table")
  # "Trade Date" matches the date pattern, numeric type → POSIXct conversion
  expect_s3_class(result[["Trade Date"]], "POSIXct")
})

test_that("rd_OutputProcesser converts string date columns with as.Date", {
  response <- list(
    headers = list(
      list(name = "Instrument", type = "string", decimalChar = NA),
      list(name = "Report Date", type = "string", decimalChar = NA),
      list(name = "Value", type = "number", decimalChar = NA)
    ),
    data = list(
      list("AAPL.O", "2023-01-01", 100),
      list("AAPL.O", "2023-01-02", 101)
    )
  )
  result <- rd_OutputProcesser(response)
  expect_s3_class(result, "data.table")
  # "Report Date" matches the date pattern, character type → Date conversion
  expect_s3_class(result[["Report Date"]], "Date")
})

test_that("rd_OutputProcesser returns empty data.table for missing data key", {
  response <- list(error = list(code = 400, message = "Bad request"))
  expect_warning(
    result <- rd_OutputProcesser(response),
    "errors"
  )
  expect_equal(nrow(result), 0)
})

test_that("rd_OutputProcesser returns empty data.table for empty data list", {
  response <- list(
    headers = list(list(name = "Instrument")),
    data = list(),
    universe = "AAPL.O"
  )
  expect_message(
    result <- rd_OutputProcesser(response),
    "empty list"
  )
  expect_equal(nrow(result), 0)
})

test_that("rd_OutputProcesser applies SpaceConvertor when non-NULL", {
  response <- list(
    headers = list(
      list(name = "Instrument", type = "string", decimalChar = NA),
      list(name = "Company Name", type = "string", decimalChar = NA)
    ),
    data = list(
      list("AAPL.O", "Apple Inc")
    )
  )
  result <- rd_OutputProcesser(response, SpaceConvertor = ".")
  expect_true("Company.Name" %in% names(result))
})

test_that("rd_OutputProcesser uses title header when use_field_names_in_headers=FALSE", {
  response <- list(
    headers = list(
      list(name = "TR.CLOSE", title = "Close Price", type = "number"),
      list(name = "TR.OPEN", title = "Open Price", type = "number")
    ),
    data = list(
      list(100, 99)
    )
  )
  result <- rd_OutputProcesser(response, use_field_names_in_headers = FALSE)
  expect_true("Close Price" %in% names(result))
})


# ═══════════════════════════════════════════════════════════════════════
# rd_GetHistoricalPricing: input validation
# ═══════════════════════════════════════════════════════════════════════

test_that("rd_GetHistoricalPricing warns and returns empty df for null universe", {
  expect_warning(
    result <- rd_GetHistoricalPricing(universe = NULL, cache = FALSE),
    "no rics"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("rd_GetHistoricalPricing errors on invalid interval", {
  expect_error(
    rd_GetHistoricalPricing(
      universe = "AAPL.O", interval = "INVALID",
      cache = FALSE
    ),
    "Interval is INVALID"
  )
})


# ═══════════════════════════════════════════════════════════════════════
# EikonGetNewsStory: processing branches
# ═══════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory errors when story_id is NULL", {
  expect_error(
    EikonGetNewsStory(story_id = NULL),
    "story_id has to be supplied"
  )
})

test_that("EikonGetNewsStory processes story format correctly", {
  mock_fn <- function(story_id, debug, raw_output) {
    list(story = list(
      storyHtml = "<p>Story HTML</p>",
      headlineHtml = "<h1>Headline</h1>",
      storyInfoHtml = "<p>Info</p>"
    ))
  }
  RDObj <- list(
    get_news_story = mock_fn,
    set_app_key = function(app_key) NULL
  )
  result <- EikonGetNewsStory(
    EikonObject = RDObj, story_id = "story1",
    raw_output = FALSE, debug = FALSE,
    renderHTML = FALSE, cache = FALSE
  )
  expect_true(is.character(result))
  expect_true(any(grepl("Story HTML", result)))
})

test_that("EikonGetNewsStory processes webURL format correctly", {
  mock_fn <- function(story_id, debug, raw_output) {
    list(webURL = "https://example.com/news/123")
  }
  RDObj <- list(
    get_news_story = mock_fn,
    set_app_key = function(app_key) NULL
  )
  result <- EikonGetNewsStory(
    EikonObject = RDObj, story_id = "web1",
    raw_output = FALSE, debug = FALSE,
    renderHTML = FALSE, cache = FALSE
  )
  expect_true(any(grepl("example.com", result)))
})

test_that("EikonGetNewsStory returns raw output correctly", {
  mock_fn <- function(story_id, debug, raw_output) {
    list(story = list(storyHtml = "<p>Raw data</p>"))
  }
  RDObj <- list(
    get_news_story = mock_fn,
    set_app_key = function(app_key) NULL
  )
  result <- EikonGetNewsStory(
    EikonObject = RDObj, story_id = "raw1",
    raw_output = TRUE, debug = FALSE,
    cache = FALSE
  )
  expect_type(result, "list")
})

test_that("EikonGetNewsStory renderHTML writes temp file and opens viewer", {
  mock_fn <- function(story_id, debug, raw_output) {
    list(story = list(
      storyHtml = "<p>HTML for render</p>",
      headlineHtml = "<h1>Headline</h1>",
      storyInfoHtml = "<p>Info</p>"
    ))
  }
  RDObj <- list(
    get_news_story = mock_fn,
    set_app_key = function(app_key) NULL
  )
  mock_viewer <- mock()

  with_mocked_bindings(
    code = {
      result <- EikonGetNewsStory(
        EikonObject = RDObj, story_id = "render1",
        raw_output = FALSE, debug = FALSE,
        renderHTML = TRUE, cache = FALSE
      )
      # The function returns the processed lines (not the renderHTML output)
      expect_true(is.character(result))
      expect_called(mock_viewer, 1)
    },
    .package = "rstudioapi",
    hasFun = function(name) TRUE,
    viewer = mock_viewer
  )
})

test_that("EikonGetNewsStory cache hit works", {
  call_count <- 0L
  mock_fn <- function(story_id, debug, raw_output) {
    call_count <<- call_count + 1L
    list(story = list(storyHtml = "<p>Cached story</p>"))
  }
  RDObj <- list(
    get_news_story = mock_fn,
    set_app_key = function(app_key) NULL
  )
  rd_ClearCache()

  r1 <- EikonGetNewsStory(
    EikonObject = RDObj, story_id = "cache_s",
    raw_output = FALSE, debug = FALSE,
    renderHTML = FALSE, cache = 300
  )
  cnt1 <- call_count
  r2 <- EikonGetNewsStory(
    EikonObject = RDObj, story_id = "cache_s",
    raw_output = FALSE, debug = FALSE,
    renderHTML = FALSE, cache = 300
  )
  expect_equal(call_count, cnt1)
  rd_ClearCache()
})


# ═══════════════════════════════════════════════════════════════════════
# EikonGetNewsHeadlines: edge cases
# ═══════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsHeadlines handles null query", {
  mock_fn <- function(query, count, repository, date_from, date_to,
                      raw_output, debug, productName, attributionCode) {
    list(headlines = list(
      list(storyId = "s1", firstCreated = "2025-01-01", text = "Test HL")
    ))
  }
  RDObj <- list(
    get_news_headlines = mock_fn,
    set_app_key = function(app_key) NULL
  )
  # Should not error when query is NULL
  result <- EikonGetNewsHeadlines(
    EikonObject = RDObj, query = NULL,
    count = 10, debug = FALSE, cache = FALSE
  )
  expect_true(!is.null(result))
})

test_that("EikonGetNewsHeadlines cache hit works", {
  call_count <- 0L
  mock_fn <- function(query, count, repository, date_from, date_to,
                      raw_output, debug, productName, attributionCode) {
    call_count <<- call_count + 1L
    list(headlines = list(
      list(storyId = "s1", firstCreated = "2025-01-01", text = "HL")
    ))
  }
  RDObj <- list(
    get_news_headlines = mock_fn,
    set_app_key = function(app_key) NULL
  )
  rd_ClearCache()

  r1 <- EikonGetNewsHeadlines(
    EikonObject = RDObj, query = "cache_test",
    count = 5, debug = FALSE, cache = 300
  )
  cnt1 <- call_count
  r2 <- EikonGetNewsHeadlines(
    EikonObject = RDObj, query = "cache_test",
    count = 5, debug = FALSE, cache = 300
  )
  expect_equal(call_count, cnt1)
  rd_ClearCache()
})


# ═══════════════════════════════════════════════════════════════════════
# flatten_headline_item: edge cases
# ═══════════════════════════════════════════════════════════════════════

test_that("flatten_headline_item returns NULL for non-list input", {
  expect_null(Refinitiv:::flatten_headline_item("not a list"))
})

test_that("flatten_headline_item returns NULL when newsItem is missing", {
  expect_null(Refinitiv:::flatten_headline_item(list(other = "data")))
})

test_that("flatten_headline_item extracts qcodes as comma-separated strings", {
  h <- list(
    storyId = "test_story",
    newsItem = list(
      `_version` = 2L,
      contentMeta = list(
        urgency = list(`$` = 3L),
        creator = list(list(`_qcode` = "NS:RTRS"), list(`_qcode` = "NS:AP")),
        infoSource = list(list(`_qcode` = "NS:RTRS")),
        language = list(list(`_tag` = "en"), list(`_tag` = "fr")),
        subject = list(list(`_qcode` = "G:1"), list(`_qcode` = "M:1QD"))
      ),
      itemMeta = list(
        firstCreated = list(`$` = "2025-03-12T15:55:31Z"),
        versionCreated = list(`$` = "2025-03-12T15:55:31Z"),
        title = list(list(`$` = "Test Headline"))
      )
    )
  )
  result <- Refinitiv:::flatten_headline_item(h)
  expect_equal(result$storyId, "test_story")
  expect_equal(result$version, 2L)
  expect_equal(result$creator, "NS:RTRS,NS:AP")
  expect_equal(result$language, "en,fr")
  expect_equal(result$subject, "G:1,M:1QD")
  expect_equal(result$title, "Test Headline")
})

test_that("flatten_headline_item handles missing optional fields gracefully", {
  h <- list(
    newsItem = list(
      contentMeta = list(),
      itemMeta = list()
    )
  )
  result <- Refinitiv:::flatten_headline_item(h)
  expect_true(is.na(result$storyId))
  expect_true(is.na(result$version))
  expect_true(is.na(result$urgency))
  expect_true(is.na(result$title))
  expect_true(is.na(result$creator))
})


dump_refinitiv_options("test-misc-coverage")
