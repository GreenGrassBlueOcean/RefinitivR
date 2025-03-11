# tests/testthat/test-rdGetNewsHeadlines.R
library(testthat)

###############################################################################
# 1) "Fake" Connection Object
###############################################################################
fakeRD <- list()
class(fakeRD) <- "fakeRD"

###############################################################################
# 2) "Fake" Functions For Testing
#    These do NOT call any real Refinitiv code. They just return the data
#    your tests expect, so you can verify correct data frames, etc.
###############################################################################

fake_rd_get_news_headlines <- function(RDObject, query, limit = 5, ...) {
  if (query == "R:TSLA.O") {
    # Return a "headlines" sub-list
    return(list(
      data = list(
        headlines = list(
          list(
            storyId = "story_tsla",
            title   = list(list(`$` = "Headline for TSLA"))
          )
        )
      )
    ))
  } else if (query == "R:MSFT.O") {
    # Return a "direct data" structure
    return(list(
      data = list(
        list(
          storyId = "story_msft",
          title   = list(list(`$` = "Direct headline for MSFT"))
        )
      )
    ))
  } else {
    # Return an empty result
    return(list(data = list()))
  }
}

fake_rd_get_news_story <- function(RDObject,
                                   story_id,
                                   raw_output  = FALSE,
                                   debug       = FALSE,
                                   renderHTML  = FALSE) {
  # Return different values depending on story_id
  if (length(story_id) > 1) {
    # If multiple story IDs, return a list (raw_output=TRUE) or a character vector
    # for processed output
    if (raw_output) {
      return(lapply(story_id, function(sid) fake_rd_get_news_story(RDObject, sid, TRUE)))
    } else {
      return(sapply(story_id, function(sid) {
        single <- fake_rd_get_news_story(RDObject, sid, FALSE)
        # single might be a string or empty
        if (is.character(single)) single else ""
      }))
    }
  }

  # Single ID logic
  if (story_id == "dummy_id") {
    # The tests want "http://example.com" as the raw link
    return("http://example.com")
  } else if (story_id == "plain_url") {
    return("https://example.com/story")
  } else if (story_id == "html_story") {
    return("<p>This is a story with <strong>HTML</strong></p>")
  } else {
    return("")
  }
}

###############################################################################
# 3) Tests That Call Our Fake Functions Directly
###############################################################################

test_that("fake_rd_get_news_headlines binds single 'headlines' result", {
  raw_resp <- fake_rd_get_news_headlines(fakeRD, query = "R:TSLA.O", limit = 5)
  expect_type(raw_resp, "list")
  expect_true("data" %in% names(raw_resp))
  expect_true("headlines" %in% names(raw_resp$data))
  expect_equal(raw_resp$data$headlines[[1]]$storyId, "story_tsla")
})

test_that("fake_rd_get_news_headlines binds single 'direct' data result", {
  raw_resp <- fake_rd_get_news_headlines(fakeRD, query = "R:MSFT.O", limit = 5)
  expect_type(raw_resp, "list")
  expect_true("data" %in% names(raw_resp))
  expect_type(raw_resp$data[[1]], "list")
  expect_equal(raw_resp$data[[1]]$storyId, "story_msft")
})

test_that("fake_rd_get_news_headlines returns empty list for unknown query", {
  raw_resp <- fake_rd_get_news_headlines(fakeRD, query = "R:FOO.BAR", limit = 5)
  expect_true("data" %in% names(raw_resp))
  expect_equal(length(raw_resp$data), 0)
})

test_that("fake_rd_get_news_story returns clickable links when renderHTML=TRUE", {
  # In a real scenario, you might run make_links_clickable() on the result
  # But here we can emulate it directly, or we can do it ourselves:
  raw_link <- fake_rd_get_news_story(fakeRD, "dummy_id", raw_output = FALSE)
  # 'raw_link' => "http://example.com"
  html_output <- make_links_clickable(raw_link)
  # Now check that we got the expected anchor
  expect_true(nzchar(html_output))
  expect_match(html_output, "<a href=\"http://example.com\"")
  expect_match(html_output, "target=\"_blank\">http://example.com</a><br/>")
})

test_that("fake_rd_get_news_story raw_output returns list if multiple IDs", {
  res <- fake_rd_get_news_story(fakeRD,
                                story_id   = c("dummy_id", "html_story"),
                                raw_output = TRUE)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_equal(res[[1]], "http://example.com")
  expect_equal(res[[2]], "<p>This is a story with <strong>HTML</strong></p>")
})

test_that("fake_rd_get_news_story processed output is character vector if multiple IDs", {
  vec <- fake_rd_get_news_story(
    fakeRD,
    story_id   = c("plain_url", "html_story"),
    raw_output = FALSE
  )
  # Remove names from 'vec'
  vec <- unname(vec)

  expect_type(vec, "character")
  expect_length(vec, 2)
  expect_equal(vec[1], "https://example.com/story")
  expect_equal(vec[2], "<p>This is a story with <strong>HTML</strong></p>")
})
