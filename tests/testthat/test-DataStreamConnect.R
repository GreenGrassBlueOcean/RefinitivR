library(testthat)
library(httr2)
library(mockery)

# Unit tests for DataStream connection and credentials validation

test_that("DataStream connection stops without password", {
  # Mocking getOption to return NULL to simulate missing credentials
  stub(getOption, "Datastream.Username", NULL)
  stub(getOption, "Datastream.Password", NULL)

  expect_error(DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = "real password"),
               "Please supply the DataStream username.")

  expect_error(DataStreamConnect(DatastreamUserName = "real username", DatastreamPassword = NA),
               "Please supply the DataStream password.")
})


test_that("TestDataStreamCredentials fails when it should", {
  # Mocking for testing without actual API call
  stub(TestDataStreamCredentials, "httr2::request", function(...) {
    stop("Please supply the DataStream username.")
  })

  expect_error(TestDataStreamCredentials(), "Please supply the DataStream username.")
  expect_error(TestDataStreamCredentials(DatastreamUsername = "someuser"), "Please supply the DataStream password.")
})


test_that("TestDataStreamCredentials satisfies test cases", {
  # Mocking httr2::req_perform to return a mock response
  stub(httr2::req_perform, "httr2::req_perform", function(...) {
    httr2::response(status_code = 401, body = jsonlite::toJSON(list(TokenValue = NULL)))
  })

  expect_warning(expect_false(TestDataStreamCredentials(DatastreamUsername = "wrong username", DatastreamPassword = "wrong Password")))
})


