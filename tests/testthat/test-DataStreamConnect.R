library(testthat)
library(httr2)
library(mockery)

test_that("DataStream connection stops without password", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  withr::local_envvar(DatastreamUsername = NA, DatastreamPassword = NA)
  withr::local_options(Datastream.Username = NULL, Datastream.Password = NULL)

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
  # No options need to be saved/restored here unless they are modified;
  # we only stub the httr2::request to simulate a missing username.
  stub(TestDataStreamCredentials, "httr2::request", function(...) {
    stop("Please supply the DataStream username.")
  })

  expect_error(TestDataStreamCredentials(), "Please supply the DataStream username.")
  expect_error(
    TestDataStreamCredentials(DatastreamUsername = "someuser"),
    "Please supply the DataStream password."
  )
})

test_that("TestDataStreamCredentials satisfies test cases", {
  # Stub httr2::req_perform to return a mock response simulating an invalid token
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

test_that("DataStreamConnect stores credentials in vault, clears options, and returns a stubbed dsws object", {
  local_refinitiv_state()
  refinitiv_vault_clear()

  captured_args <- NULL
  # Stub the new() method of DatastreamDSWS2R::dsws to return a mock object.
  stub(DataStreamConnect, "DatastreamDSWS2R::dsws$new", function(...) {
    captured_args <<- list(...)
    "mock_dsws_object"
  })

  # Call DataStreamConnect with valid credentials.
  result <- DataStreamConnect(
    DatastreamUserName = "test_user",
    DatastreamPassword = "test_password"
  )

  expect_equal(result, "mock_dsws_object")
  expect_equal(captured_args$username, "test_user")
  expect_equal(captured_args$password, "test_password")
  expect_equal(refinitiv_vault_get("datastream_username"), "test_user")
  expect_equal(refinitiv_vault_get("datastream_password"), "test_password")
  expect_null(getOption("Datastream.Username"))
  expect_null(getOption("Datastream.Password"))
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

  # Stub req_perform, resp_status, and resp_body_string to simulate the response.
  stub(TestDataStreamCredentials, "httr2::req_perform", function(...) dummy_response)
  stub(TestDataStreamCredentials, "httr2::resp_status", function(response) response$status_code)
  stub(TestDataStreamCredentials, "httr2::resp_body_string", function(response) rawToChar(response$body))

  expect_warning(
    result <- TestDataStreamCredentials(DatastreamUsername = "user", DatastreamPassword = "pass"),
    "Failed to retrieve a valid token. Credentials may be incorrect."
  )
  expect_false(result)
})

test_that("TestDataStreamCredentials returns FALSE on non-200 HTTP status", {
  dummy_response <- structure(
    list(status_code = 401),
    class = "httr2_response"
  )

  stub(TestDataStreamCredentials, "httr2::req_perform", function(...) dummy_response)
  stub(TestDataStreamCredentials, "httr2::resp_status", function(response) response$status_code)

  expect_warning(
    result <- TestDataStreamCredentials(DatastreamUsername = "user", DatastreamPassword = "pass"),
    "HTTP status code: 401"
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

  # Stub req_perform, resp_status, and resp_body_string to simulate the response.
  stub(TestDataStreamCredentials, "httr2::req_perform", function(...) dummy_response)
  stub(TestDataStreamCredentials, "httr2::resp_status", function(response) response$status_code)
  stub(TestDataStreamCredentials, "httr2::resp_body_string", function(response) rawToChar(response$body))

  msgs <- capture_messages(
    result <- TestDataStreamCredentials(DatastreamUsername = "user", DatastreamPassword = "pass")
  )

  expect_true(result)
  expect_true(any(grepl("DataStream credentials are valid", msgs)))
})

dump_refinitiv_options("test-DataStreamConnect")
