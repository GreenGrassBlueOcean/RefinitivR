# tests/testthat/test-EikonNews-coverage.R
# Additional tests to improve coverage of R/EikonNews.R
# Targets uncovered branches: cache hit/store, NULL query, renderHTML failures,
# and the "else" fallback in EikonGetNewsStory Returnlines processing.

library(testthat)
library(mockery)
library(data.table)

.saved_state <- save_refinitiv_state()


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsHeadlines — cache paths
# ═══════════════════════════════════════════════════════════════════════════════

dummy_hl_response <- list(
  headlines = list(
    data.table(
      Index = "2024-06-01T00:00:00",
      version_created = "2024-06-01",
      text = "Cache test headline",
      story_id = "cache_hl_001",
      source_code = "TST"
    )
  )
)

dummy_Eikon_hl <- list(
  get_news_headlines = function(query, count, repository, date_from, date_to,
                                raw_output, debug) {
    dummy_hl_response
  }
)


test_that("EikonGetNewsHeadlines: cache store + hit path works", {
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  # First call stores in cache
  r1 <- EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon_hl, query = "R:TEST.O",
    count = 1, raw_output = FALSE, debug = FALSE, cache = 300
  )
  expect_s3_class(r1, "data.frame")

  # Second call should hit cache (mock never called again)
  call_count <- 0L
  counting_Eikon <- list(
    get_news_headlines = function(query, count, repository, date_from, date_to,
                                  raw_output, debug) {
      call_count <<- call_count + 1L
      dummy_hl_response
    }
  )
  r2 <- EikonGetNewsHeadlines(
    EikonObject = counting_Eikon, query = "R:TEST.O",
    count = 1, raw_output = FALSE, debug = FALSE, cache = 300
  )
  expect_equal(call_count, 0L)
  expect_equal(r1, r2)
  rd_ClearCache()
})


test_that("EikonGetNewsHeadlines: cache hit emits debug message", {
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  # Populate cache
  EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon_hl, query = "R:DBG.O",
    count = 1, raw_output = FALSE, debug = FALSE, cache = 300
  )

  # Second call with debug = TRUE should emit cache hit message
  expect_message(
    EikonGetNewsHeadlines(
      EikonObject = dummy_Eikon_hl, query = "R:DBG.O",
      count = 1, raw_output = FALSE, debug = TRUE, cache = 300
    ),
    "Cache hit"
  )
  rd_ClearCache()
})


test_that("EikonGetNewsHeadlines: cache store for raw_output = TRUE", {
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  r1 <- EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon_hl, query = "R:RAW.O",
    count = 1, raw_output = TRUE, debug = FALSE, cache = 60
  )
  expect_type(r1, "list")

  call_count <- 0L
  counting_Eikon <- list(
    get_news_headlines = function(query, count, repository, date_from, date_to,
                                  raw_output, debug) {
      call_count <<- call_count + 1L
      dummy_hl_response
    }
  )
  r2 <- EikonGetNewsHeadlines(
    EikonObject = counting_Eikon, query = "R:RAW.O",
    count = 1, raw_output = TRUE, debug = FALSE, cache = 60
  )
  expect_equal(call_count, 0L)
  rd_ClearCache()
})


test_that("EikonGetNewsHeadlines: query = NULL is converted to empty string", {
  # When query is NULL, it should be set to "" internally and produce a result
  result <- EikonGetNewsHeadlines(
    EikonObject = dummy_Eikon_hl, query = NULL,
    count = 1, raw_output = FALSE, debug = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_equal(result$query, "")
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — cache paths
# ═══════════════════════════════════════════════════════════════════════════════

dummy_Eikon_story <- list(
  get_news_story = function(story_id, debug, raw_output) {
    list(story = list(
      headlineHtml  = "<h1>Headline</h1>",
      storyHtml     = "<div>Story</div>",
      storyInfoHtml = "<p>Info</p>"
    ))
  }
)


test_that("EikonGetNewsStory: cache store + hit path works", {
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  r1 <- EikonGetNewsStory(
    EikonObject = dummy_Eikon_story, story_id = "cache_story_1",
    raw_output = FALSE, debug = FALSE, cache = 300
  )
  expect_type(r1, "character")

  call_count <- 0L
  counting_Eikon <- list(
    get_news_story = function(story_id, debug, raw_output) {
      call_count <<- call_count + 1L
      list(story = list(storyHtml = "<div>Story</div>"))
    }
  )
  r2 <- EikonGetNewsStory(
    EikonObject = counting_Eikon, story_id = "cache_story_1",
    raw_output = FALSE, debug = FALSE, cache = 300
  )
  expect_equal(call_count, 0L)
  expect_equal(r1, r2)
  rd_ClearCache()
})


test_that("EikonGetNewsStory: cache hit emits debug message", {
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  EikonGetNewsStory(
    EikonObject = dummy_Eikon_story, story_id = "dbg_story_1",
    raw_output = FALSE, debug = FALSE, cache = 300
  )

  expect_message(
    EikonGetNewsStory(
      EikonObject = dummy_Eikon_story, story_id = "dbg_story_1",
      raw_output = FALSE, debug = TRUE, cache = 300
    ),
    "Cache hit"
  )
  rd_ClearCache()
})


test_that("EikonGetNewsStory: cache store for raw_output = TRUE", {
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  r1 <- EikonGetNewsStory(
    EikonObject = dummy_Eikon_story, story_id = "raw_cache_1",
    raw_output = TRUE, debug = FALSE, cache = 120
  )
  expect_type(r1, "list")

  call_count <- 0L
  counting_Eikon <- list(
    get_news_story = function(story_id, debug, raw_output) {
      call_count <<- call_count + 1L
      list(story = list(storyHtml = "<div>Story</div>"))
    }
  )
  r2 <- EikonGetNewsStory(
    EikonObject = counting_Eikon, story_id = "raw_cache_1",
    raw_output = TRUE, debug = FALSE, cache = 120
  )
  expect_equal(call_count, 0L)
  rd_ClearCache()
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — else (fallback) branch in Returnlines processing
# ═══════════════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory: fallback branch returns raw content as-is", {
  # When the response has neither "story" nor "webURL" keys,
  # the else branch should return(x) directly.
  fallback_Eikon <- list(
    get_news_story = function(story_id, debug, raw_output) {
      "Plain text content with no story/webURL wrapper"
    }
  )

  result <- EikonGetNewsStory(
    EikonObject = fallback_Eikon, story_id = "fallback_1",
    raw_output = FALSE, debug = FALSE
  )
  expect_type(result, "character")
  expect_equal(result, "Plain text content with no story/webURL wrapper")
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — renderHTML: dir.create failure
# ═══════════════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory: renderHTML errors when dir.create fails", {
  stub(EikonGetNewsStory, "dir.create", function(...) FALSE)
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) TRUE)
  stub(EikonGetNewsStory, "rstudioapi::viewer", function(file) NULL)

  expect_error(
    EikonGetNewsStory(
      EikonObject = dummy_Eikon_story, story_id = "dir_fail_1",
      raw_output = FALSE, debug = FALSE, renderHTML = TRUE
    ),
    "Failed to create temporary directory"
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — renderHTML: writeLines failure
# ═══════════════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory: renderHTML errors when writeLines fails", {
  stub(EikonGetNewsStory, "writeLines", function(...) stop("disk full"))
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) TRUE)
  stub(EikonGetNewsStory, "rstudioapi::viewer", function(file) NULL)

  expect_error(
    EikonGetNewsStory(
      EikonObject = dummy_Eikon_story, story_id = "write_fail_1",
      raw_output = FALSE, debug = FALSE, renderHTML = TRUE
    ),
    "Failed to write to HTML file"
  )
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — renderHTML with webURL response
# ═══════════════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory: renderHTML handles webURL in Newslines processing", {
  web_Eikon <- list(
    get_news_story = function(story_id, debug, raw_output) {
      list(webURL = "https://example.com/news/123")
    }
  )

  captured_viewer <- NULL
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) TRUE)
  stub(EikonGetNewsStory, "rstudioapi::viewer", function(file) {
    captured_viewer <<- file
  })

  result <- EikonGetNewsStory(
    EikonObject = web_Eikon, story_id = "weburl_render_1",
    raw_output = FALSE, debug = FALSE, renderHTML = TRUE
  )
  expect_type(result, "character")
  # The webURL path in the Newslines lapply returns x$webURL
  expect_true(grepl("example\\.com", result))
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — renderHTML: else branch in Newslines processing
# ═══════════════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory: renderHTML else branch returns raw in Newslines", {
  other_Eikon <- list(
    get_news_story = function(story_id, debug, raw_output) {
      "Some raw text"
    }
  )

  captured_viewer <- NULL
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) TRUE)
  stub(EikonGetNewsStory, "rstudioapi::viewer", function(file) {
    captured_viewer <<- file
  })

  result <- EikonGetNewsStory(
    EikonObject = other_Eikon, story_id = "other_render_1",
    raw_output = FALSE, debug = FALSE, renderHTML = TRUE
  )
  expect_type(result, "character")
})


# ═══════════════════════════════════════════════════════════════════════════════
# EikonGetNewsStory — renderHTML debug messages
# ═══════════════════════════════════════════════════════════════════════════════

test_that("EikonGetNewsStory: renderHTML with debug emits all debug messages", {
  stub(EikonGetNewsStory, "rstudioapi::hasFun", function(fun) TRUE)
  stub(EikonGetNewsStory, "rstudioapi::viewer", function(file) NULL)

  msgs <- capture_messages(
    EikonGetNewsStory(
      EikonObject = dummy_Eikon_story, story_id = "debug_render_1",
      raw_output = FALSE, debug = TRUE, renderHTML = TRUE
    )
  )
  # Should emit multiple debug messages about temp dir, HTML path, writing, etc.
  all_msgs <- paste(msgs, collapse = " ")
  expect_true(grepl("Normalized temporary directory", all_msgs))
  expect_true(grepl("Creating temporary directory", all_msgs))
  expect_true(grepl("HTML file path", all_msgs))
  expect_true(grepl("Writing news content", all_msgs))
  expect_true(grepl("Successfully wrote to HTML file", all_msgs))
  expect_true(grepl("Opening HTML file in RStudio viewer", all_msgs))
})


restore_refinitiv_state(.saved_state, "test-EikonNews-coverage")
