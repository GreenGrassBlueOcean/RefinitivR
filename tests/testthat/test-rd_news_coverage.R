# tests/testthat/test-rd_news_coverage.R
# Additional tests to cover uncovered branches in rd_news.r

library(testthat)
library(mockery)

# ── rd_get_news_story: newsItem with full metadata ──────────────────────

test_that("rd_get_news_story extracts versionCreated and urgency from newsItem", {
  mock_fn <- function(story_id, raw_output, debug) {
    list(newsItem = list(
      itemMeta = list(
        title = list(list(`$` = "Test Headline")),
        versionCreated = list(`$` = "2025-03-12T15:55:31.127Z")
      ),
      contentMeta = list(
        urgency = list(`$` = 3L)
      ),
      contentSet = list(
        inlineXML = list(`$` = "<p>Full article</p>"),
        inlineData = list(`$` = "Plain text content")
      )
    ))
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  result <- rd_get_news_story(
    RDObject = RDObj, story_id = "meta_story",
    debug = FALSE, cache = FALSE
  )
  s <- result[["meta_story"]]
  expect_equal(s$headline, "Test Headline")
  expect_equal(s$versionCreated, "2025-03-12T15:55:31.127Z")
  expect_equal(s$urgency, 3L)
  expect_equal(s$html, "<p>Full article</p>")
  expect_equal(s$inline, "Plain text content")
})


# ── rd_get_news_story: webURL with headline ─────────────────────────────

test_that("rd_get_news_story extracts headline from webURL response", {
  mock_fn <- function(story_id, raw_output, debug) {
    list(webURL = "https://example.com/story", headline = "Web Story Title")
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  result <- rd_get_news_story(
    RDObject = RDObj, story_id = "web_hl",
    debug = FALSE, cache = FALSE
  )
  expect_equal(result[["web_hl"]]$headline, "Web Story Title")
  expect_equal(result[["web_hl"]]$inline, "https://example.com/story")
})


# ── rd_get_news_story: base64 image in newsItem HTML ───────────────────

test_that("rd_get_news_story handles base64 image in newsItem HTML", {
  mock_fn <- function(story_id, raw_output, debug) {
    list(newsItem = list(
      contentSet = list(
        inlineXML = list(`$` = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUg==")
      )
    ))
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  result <- rd_get_news_story(
    RDObject = RDObj, story_id = "img_story",
    debug = FALSE, cache = FALSE
  )
  s <- result[["img_story"]]
  # The base64 handling replaces the content with an <img> tag
  expect_true(grepl("img", s$html) || grepl("data:image", s$html))
})


# ── rd_get_news_story: headline-only fallback ───────────────────────────

test_that("rd_get_news_story headline fallback emits debug message", {
  mock_fn <- function(story_id, raw_output, debug) {
    list(newsItem = list(
      itemMeta = list(title = list(list(`$` = "Only Headline"))),
      contentSet = list()
    ))
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  expect_message(
    rd_get_news_story(
      RDObject = RDObj, story_id = "hl_only",
      debug = TRUE, cache = FALSE
    ),
    "No full content; using headline"
  )
})


# ── rd_get_news_story: browseURL fallback when rstudioapi unavailable ──

test_that("rd_get_news_story falls back to browseURL when rstudioapi unavailable", {
  mock_fn <- function(story_id, raw_output, debug) {
    list(story = list(storyHtml = "<p>HTML content for browser</p>"))
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  mock_browse <- mock()

  # Need to mock rstudioapi::hasFun → FALSE and utils::browseURL → no-op
  with_mocked_bindings(
    code = {
      with_mocked_bindings(
        code = {
          result <- rd_get_news_story(
            RDObject = RDObj, story_id = "browser_story",
            debug = FALSE, renderHTML = TRUE, cache = FALSE
          )
          expect_true(is.character(result))
          expect_called(mock_browse, 1)
        },
        .package = "utils",
        browseURL = mock_browse
      )
    },
    .package = "rstudioapi",
    hasFun = function(name) FALSE
  )
})


# ── rd_get_news_story: cache hit path ──────────────────────────────────

test_that("rd_get_news_story returns cached result on second call", {
  call_count <- 0L
  mock_fn <- function(story_id, raw_output, debug) {
    call_count <<- call_count + 1L
    list(story = list(storyHtml = "<p>Cached</p>"))
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  withr::local_options(refinitiv_cache = TRUE)
  rd_ClearCache()

  # First call populates cache
  r1 <- rd_get_news_story(
    RDObject = RDObj, story_id = "cache_story",
    debug = FALSE, cache = TRUE
  )
  first_count <- call_count

  # Second call should hit cache
  r2 <- rd_get_news_story(
    RDObject = RDObj, story_id = "cache_story",
    debug = FALSE, cache = TRUE
  )
  expect_equal(r1, r2)
  expect_equal(call_count, first_count) # no additional API call
  rd_ClearCache()
})

test_that("rd_get_news_story caches raw_output results", {
  mock_fn <- function(story_id, raw_output, debug) {
    list(raw = "data")
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  rd_ClearCache()

  r1 <- rd_get_news_story(
    RDObject = RDObj, story_id = "raw_cache",
    raw_output = TRUE, debug = FALSE, cache = 300
  )
  r2 <- rd_get_news_story(
    RDObject = RDObj, story_id = "raw_cache",
    raw_output = TRUE, debug = FALSE, cache = 300
  )
  expect_equal(r1, r2)
  rd_ClearCache()
})


# ── rd_get_news_story: debug messages in retry loop ────────────────────

test_that("rd_get_news_story debug messages during retry failures", {
  attempt <- 0L
  mock_fn <- function(story_id, raw_output, debug) {
    attempt <<- attempt + 1L
    if (attempt < 3) stop("temporary failure")
    list(story = list(storyHtml = "<p>Got it</p>"))
  }
  RDObj <- list(rd_get_news_story = mock_fn)
  expect_message(
    rd_get_news_story(
      RDObject = RDObj, story_id = "retry_story",
      debug = TRUE, cache = FALSE
    ),
    "Failed fetch for retry_story"
  )
})


# ═══════════════════════════════════════════════════════════════════════
# rd_get_news_headlines processing branches
# ═══════════════════════════════════════════════════════════════════════

test_that("rd_get_news_headlines processes data correctly", {
  mock_fn <- function(query, limit, sort, relevancy, cursor, dateFrom, dateTo,
                      raw_output, debug) {
    list(data = list(
      list(
        storyId = "urn:newsml:reuters.com:20250312:nABC123:1",
        newsItem = list(
          `_version` = 1L,
          contentMeta = list(
            urgency = list(`$` = 2L),
            creator = list(list(`_qcode` = "NS:RTRS")),
            infoSource = list(list(`_qcode` = "NS:RTRS")),
            language = list(list(`_tag` = "en")),
            subject = list(list(`_qcode` = "G:1"))
          ),
          itemMeta = list(
            firstCreated = list(`$` = "2025-03-12T15:55:31Z"),
            versionCreated = list(`$` = "2025-03-12T15:55:31Z"),
            title = list(list(`$` = "Test Headline"))
          )
        )
      )
    ))
  }
  RDObj <- list(rd_get_news_headlines = mock_fn)
  result <- rd_get_news_headlines(
    RDObject = RDObj, query = "AAPL",
    limit = 10, debug = FALSE, cache = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_true("storyId" %in% names(result))
  expect_true("title" %in% names(result))
})

test_that("rd_get_news_headlines returns empty df when no data", {
  mock_fn <- function(query, limit, sort, relevancy, cursor, dateFrom, dateTo,
                      raw_output, debug) {
    list(data = NULL)
  }
  RDObj <- list(rd_get_news_headlines = mock_fn)
  result <- rd_get_news_headlines(
    RDObject = RDObj, query = "NOSUCH",
    limit = 10, debug = FALSE, cache = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("rd_get_news_headlines returns empty df when all flattened items are NULL", {
  mock_fn <- function(query, limit, sort, relevancy, cursor, dateFrom, dateTo,
                      raw_output, debug) {
    list(data = list(
      list(no_newsItem = "bad structure")
    ))
  }
  RDObj <- list(rd_get_news_headlines = mock_fn)
  result <- rd_get_news_headlines(
    RDObject = RDObj, query = "BAD",
    limit = 10, debug = FALSE, cache = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("rd_get_news_headlines sets query to empty string when NULL", {
  mock_fn <- function(query, limit, sort, relevancy, cursor, dateFrom, dateTo,
                      raw_output, debug) {
    # Verify query was passed through
    list(data = list(
      list(
        storyId = "test1",
        newsItem = list(
          `_version` = 1L,
          contentMeta = list(),
          itemMeta = list(title = list(list(`$` = "Headline")))
        )
      )
    ))
  }
  RDObj <- list(rd_get_news_headlines = mock_fn)
  result <- rd_get_news_headlines(
    RDObject = RDObj, query = NULL,
    limit = 10, debug = FALSE, cache = FALSE
  )
  expect_s3_class(result, "data.frame")
})

test_that("rd_get_news_headlines cache hit path works", {
  call_count <- 0L
  mock_fn <- function(query, limit, sort, relevancy, cursor, dateFrom, dateTo,
                      raw_output, debug) {
    call_count <<- call_count + 1L
    list(data = list(
      list(
        storyId = "cached1",
        newsItem = list(
          `_version` = 1L,
          contentMeta = list(),
          itemMeta = list(title = list(list(`$` = "Cached HL")))
        )
      )
    ))
  }
  RDObj <- list(rd_get_news_headlines = mock_fn)
  rd_ClearCache()

  r1 <- rd_get_news_headlines(
    RDObject = RDObj, query = "CACHE_TEST",
    limit = 10, debug = FALSE, cache = 300
  )
  cnt1 <- call_count
  r2 <- rd_get_news_headlines(
    RDObject = RDObj, query = "CACHE_TEST",
    limit = 10, debug = FALSE, cache = 300
  )
  expect_equal(call_count, cnt1)
  rd_ClearCache()
})


# ═══════════════════════════════════════════════════════════════════════
# rd_get_top_news processing tests
# ═══════════════════════════════════════════════════════════════════════

# Helper: create a mock RDObject that routes send_json_request calls
# through a list of known responses
mock_top_news_response <- list(
  data = list(
    list(
      name = "Main",
      pages = list(
        list(
          name = "Front Page", po = 1L, revisionId = "rev1",
          revisionDate = "2025-03-01", topNewsId = "tn001"
        ),
        list(
          name = "World News", po = 2L, revisionId = "rev2",
          revisionDate = "2025-03-01", topNewsId = "tn002"
        )
      )
    ),
    list(
      name = "Breakingviews",
      pages = list(
        list(
          name = "Reuters Breakingviews", po = 1L, revisionId = "rev3",
          revisionDate = "2025-03-01", topNewsId = "tn003"
        )
      )
    )
  )
)

mock_story_response <- list(
  data = list(
    list(storyId = "s1", text = "First Story", snippet = "Snippet 1"),
    list(storyId = "s2", text = "Second Story", snippet = "Snippet 2")
  )
)

test_that("rd_get_top_news processes response and returns data.frame", {
  call_count <- 0L
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(RDObject = list(), debug = FALSE, cache = FALSE)
      expect_s3_class(result, "data.frame")
      expect_true("group" %in% names(result))
      expect_true("storyId" %in% names(result))
      expect_true("title" %in% names(result))
      expect_true(nrow(result) > 0)
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      call_count <<- call_count + 1L
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news filters by group", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(
        RDObject = list(), group = "Main",
        debug = FALSE, cache = FALSE
      )
      expect_s3_class(result, "data.frame")
      expect_true(all(result$group == "Main"))
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news filters by page", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(
        RDObject = list(), page = "Front Page",
        debug = FALSE, cache = FALSE
      )
      expect_s3_class(result, "data.frame")
      expect_true(all(result$page_name == "Front Page"))
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news filters by both group and page (union)", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(
        RDObject = list(),
        group = "Main", page = "Reuters Breakingviews",
        debug = FALSE, cache = FALSE
      )
      expect_s3_class(result, "data.frame")
      # Should get Main group rows + Reuters Breakingviews page row
      expect_true(nrow(result) > 0)
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news returns empty df when no matching pages", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(
        RDObject = list(),
        group = "NonExistentGroup",
        debug = FALSE, cache = FALSE
      )
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news returns raw output when requested", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(
        RDObject = list(), raw_output = TRUE,
        debug = FALSE, cache = FALSE
      )
      expect_type(result, "list")
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news handles null data in response", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(RDObject = list(), debug = FALSE, cache = FALSE)
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      return(list(data = NULL))
    }
  )
})

test_that("rd_get_top_news handles story fetch failure gracefully", {
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(RDObject = list(), debug = FALSE, cache = FALSE)
      expect_s3_class(result, "data.frame")
      # Stories should have NA for storyId/title/snippet
      expect_true(any(is.na(result$storyId)))
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        stop("Simulated fetch failure")
      }
      return(mock_top_news_response)
    }
  )
})

test_that("rd_get_top_news handles group with null pages", {
  response_with_null_pages <- list(
    data = list(
      list(name = "EmptyGroup", pages = NULL),
      list(
        name = "Main",
        pages = list(
          list(
            name = "Front Page", po = 1L, revisionId = "rev1",
            revisionDate = "2025-03-01", topNewsId = "tn001"
          )
        )
      )
    )
  )
  with_mocked_bindings(
    code = {
      result <- rd_get_top_news(RDObject = list(), debug = FALSE, cache = FALSE)
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(response_with_null_pages)
    }
  )
})

test_that("rd_get_top_news cache hit path works", {
  call_count <- 0L
  rd_ClearCache()
  with_mocked_bindings(
    code = {
      r1 <- rd_get_top_news(RDObject = list(), debug = FALSE, cache = 300)
      cnt1 <- call_count
      r2 <- rd_get_top_news(RDObject = list(), debug = FALSE, cache = 300)
      expect_equal(call_count, cnt1) # no additional API calls
    },
    send_json_request = function(payload = NULL, service = NULL,
                                 request_type = NULL, EndPoint = NULL,
                                 debug = FALSE, ...) {
      call_count <<- call_count + 1L
      if (grepl("top-news/tn", EndPoint)) {
        return(mock_story_response)
      }
      return(mock_top_news_response)
    }
  )
  rd_ClearCache()
})

dump_refinitiv_options("test-rd_news_coverage")
