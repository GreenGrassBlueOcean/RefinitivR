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


# Existing tests for missing credentials and TestDataStreamCredentials remain unchanged
test_that("DataStream connection stops without password", {
  # Mocking getOption to return NULL to simulate missing credentials
  stub(getOption, "Datastream.Username", NULL)
  stub(getOption, "Datastream.Password", NULL)

  expect_error(
    DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = "real password"),
    "Please supply the DataStream username."
  )

  expect_error(
    DataStreamConnect(DatastreamUserName = "real username", DatastreamPassword = NA),
    "Please supply the DataStream password."
  )
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

  expect_warning(
    expect_false(TestDataStreamCredentials(
      DatastreamUsername = "wrong username",
      DatastreamPassword = "wrong Password"
    ))
  )
})

# --- New Test Covering the "Main Operation" Lines in DataStreamConnect ---

test_that("DataStreamConnect sets options and returns a stubbed dsws object", {
  # Stub the new method of DatastreamDSWS2R::dsws to return "mock_dsws_object"
  stub(DataStreamConnect, "DatastreamDSWS2R::dsws$new", function(...) "mock_dsws_object")

  # Call DataStreamConnect with valid credentials.
  result <- DataStreamConnect(DatastreamUserName = "test_user", DatastreamPassword = "test_password")

  # Check that the returned object is the stubbed value.
  expect_equal(result, "mock_dsws_object")

  # Verify that the options have been set correctly.
  expect_equal(getOption("Datastream.Username"), "test_user")
  expect_equal(getOption("Datastream.Password"), "test_password")
})



test_that("TestDataStreamCredentials returns FALSE if response lacks valid token", {
  # Create a dummy response with status 200 but missing TokenValue.
  dummy_response <- structure(
    list(
      status_code = 200,
      body = charToRaw('{"SomeField": "value"}')
    ),
    class = "httr2_response"
  )

  # Stub req_perform, resp_status, and resp_body_string.
  stub(TestDataStreamCredentials, "httr2::req_perform", function(...) dummy_response)
  stub(TestDataStreamCredentials, "httr2::resp_status", function(response) response$status_code)
  stub(TestDataStreamCredentials, "httr2::resp_body_string", function(response) rawToChar(response$body))

  expect_warning(
    result <- TestDataStreamCredentials(DatastreamUsername = "user", DatastreamPassword = "pass"),
    "Failed to retrieve a valid token. Credentials may be incorrect."
  )
  expect_false(result)
})

test_that("TestDataStreamCredentials returns TRUE if a valid token is received", {
  # Create a dummy response with status 200 and a valid TokenValue.
  dummy_response <- structure(
    list(
      status_code = 200,
      body = charToRaw('{"TokenValue": "abc123"}')
    ),
    class = "httr2_response"
  )

  # Stub req_perform, resp_status, and resp_body_string.
  stub(TestDataStreamCredentials, "httr2::req_perform", function(...) dummy_response)
  stub(TestDataStreamCredentials, "httr2::resp_status", function(response) response$status_code)
  stub(TestDataStreamCredentials, "httr2::resp_body_string", function(response) rawToChar(response$body))

  msgs <- capture_messages(
    result <- TestDataStreamCredentials(DatastreamUsername = "user", DatastreamPassword = "pass")
  )

  expect_true(result)
  expect_true(any(grepl("DataStream credentials are valid", msgs)))
})
