# A helper function to simulate a response where headlines are in a "headlines" sub‐list.
simulate_response_with_headlines <- function(story_ids) {
  headlines <- lapply(story_ids, function(sid) {
    list(
      storyId = sid,
      title = list(list(`$` = paste("Headline for", sid)))
    )
  })
  return(list(data = list(headlines = headlines)))
}

# A helper function to simulate a response where headlines are directly in the data list.
simulate_response_direct <- function(story_ids) {
  headlines <- lapply(story_ids, function(sid) {
    list(
      storyId = sid,
      title = list(list(`$` = paste("Direct headline for", sid)))
    )
  })
  return(list(data = headlines))
}

test_that("rd_get_news_headlines binds a single query result with data$headlines correctly", {
  fakeRD <- list()
  fakeRD$rd_get_news_headlines <- function(...) {
    simulate_response_with_headlines("story1")
  }
  class(fakeRD) <- "fakeRD"

  result <- rd_get_news_headlines(RDObject = fakeRD, query = "R:TSLA.O", limit = 5)

  expect_true("storyId" %in% names(result))
  expect_equal(result$storyId, "story1")
  expect_equal(result$query, "R:TSLA.O")
})

test_that("rd_get_news_headlines binds a single query result with direct data correctly", {
  fakeRD <- list()
  fakeRD$rd_get_news_headlines <- function(...) {
    simulate_response_direct("story_direct")
  }
  class(fakeRD) <- "fakeRD"

  result <- rd_get_news_headlines(RDObject = fakeRD, query = "R:MSFT.O", limit = 5)

  expect_true("storyId" %in% names(result))
  expect_equal(result$storyId, "story_direct")
  expect_equal(result$query, "R:MSFT.O")
})

test_that("rd_get_news_headlines binds multiple query results correctly", {
  fakeRD <- list()
  fakeRD$rd_get_news_headlines <- function(query, ...) {
    # Return a simulated response depending on the query value.
    if (query == "R:TSLA.O") {
      simulate_response_with_headlines("story_tsla")
    } else {
      simulate_response_direct("story_msft")
    }
  }
  class(fakeRD) <- "fakeRD"

  queries <- c("R:TSLA.O", "R:MSFT.O")
  result <- rd_get_news_headlines(RDObject = fakeRD, query = queries, limit = 5)

  expect_true("query" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$query, queries)
})

test_that("rd_get_news_headlines returns an empty data frame when responses are empty", {
  fakeRD <- list()
  fakeRD$rd_get_news_headlines <- function(...) {
    list(data = list())
  }
  class(fakeRD) <- "fakeRD"

  result <- rd_get_news_headlines(RDObject = fakeRD, query = "R:TSLA.O", limit = 5)
  expect_equal(nrow(result), 0)
})
