# Snapshot + auto-restore all Refinitiv options and vault at end of file
local_refinitiv_state(teardown_env())

test_that("json_builder works", {

  directions <- 'DataGrid_StandardAsync'
  payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
        'fields' = lapply(list("TR.RICCode"), function(x) list("name" = x))
      )))

  TestOutcome <- json_builder(directions, payload)
  CorrectOutcome <- list(Entity = list(E = "DataGrid_StandardAsync"
                                      , W = list(requests = list(list(instruments = list("TSLA.O")
                                                                      , fields = list(list(name = "TR.RICCode")))))))

  expect_equal(TestOutcome, CorrectOutcome)
})

test_that("jsonDataGridConstructor works with simple request", {
  payload <- list(universe = "AAPL.O", fields = c("BID", "ASK"))
  result <- jsonDataGridConstructor(payload)

  # Output must be valid JSON with the correct structure
  parsed <- jsonlite::fromJSON(result, simplifyVector = TRUE)
  expect_equal(parsed$universe, "AAPL.O")
  expect_equal(parsed$fields, c("BID", "ASK"))
  expect_null(parsed$parameters)
})

test_that("jsonDataGridConstructor works with complex request", {
  payload <- list(universe = "AAPL.O"
                , fields = c("TR.Revenue", "TR.GrossProfit")
                , parameters = list("Curn" = "USD", "SDate" = "2020-10-27"
                                    , "EDate" = "2020-12-01"))
  result <- jsonDataGridConstructor(payload)

  parsed <- jsonlite::fromJSON(result, simplifyVector = TRUE)
  expect_equal(parsed$universe, "AAPL.O")
  expect_equal(parsed$fields, c("TR.Revenue", "TR.GrossProfit"))
  expect_equal(parsed$parameters$Curn,  "USD")
  expect_equal(parsed$parameters$SDate, "2020-10-27")
  expect_equal(parsed$parameters$EDate, "2020-12-01")
})

test_that("jsonDataGridConstructor works with parameters as empty list", {
  payload <- list(universe = "AAPL.O"
                , fields = c("TR.Revenue", "TR.GrossProfit")
                , parameters = list())
  result <- jsonDataGridConstructor(payload)

  parsed <- jsonlite::fromJSON(result, simplifyVector = TRUE)
  expect_equal(parsed$universe, "AAPL.O")
  expect_equal(parsed$fields, c("TR.Revenue", "TR.GrossProfit"))
  # Empty parameters must be dropped from the payload entirely
  expect_null(parsed$parameters)
})

test_that("jsonDataGridConstructor correctly escapes special characters in RIC names", {
  # Hand-rolled paste() would break with quotes; jsonlite handles this safely
  payload <- list(universe = 'RIC"with"quotes', fields = c("BID"))
  result <- jsonDataGridConstructor(payload)

  parsed <- jsonlite::fromJSON(result, simplifyVector = TRUE)
  expect_equal(parsed$universe, 'RIC"with"quotes')
})



test_that("RefinitivJsonConnect uses DEFAULT_WORKSPACE_APP_KEY when no key supplied", {
  originalOptionValue = getOption(".EikonApiKey")
  on.exit(options(.EikonApiKey = originalOptionValue), add = TRUE)

  options(.EikonApiKey = NULL)
  refinitiv_vault_clear(keys = "api_key")

  conn <- suppressWarnings(RefinitivJsonConnect())
  expect_equal(refinitiv_vault_get("api_key"), "DEFAULT_WORKSPACE_APP_KEY")
})


test_that("RefinitivJsonConnect does work with application id", {
  local_refinitiv_state()

  EikonJson <- suppressWarnings(RefinitivJsonConnect(Eikonapplication_id = "testing_key"))

  expected_methods <- c(
    "create_custom_instrument", "get_app_key", "get_app_port", "get_data",
    "get_data_rdp", "get_esg", "get_estimates", "get_historical_pricing",
    "get_interday_custominstrument_pricing",
    "get_intraday_custominstrument_pricing", "get_news_headlines", "get_news_story",
    "get_ownership", "get_rdp_streaming_url", "get_search_metadata",
    "get_symbology", "get_timeseries",
    "manage_custom_instrument", "rd_get_news_headlines", "rd_get_news_story",
    "search", "search_custom_instrument", "set_app_key", "set_app_port"
  )
  expect_equal(sort(names(EikonJson)), expected_methods)

  expect_equal(EikonJson$get_app_key(), "testing_key")
})

test_that("Construct_url does work correctly", {
  withr::local_options(
    refinitiv_base_url = "http://localhost",
    eikon_port = 9000L,
    eikon_api = "/api/v1/data",
    rdp_api = "/api/rdp/"
  )

  expect_equal( Construct_url(service = "eikon")
              ,  "http://localhost:9000/api/v1/data"
              )

  expect_equal( Construct_url(service = "rdp", EndPoint = "discovery/search/v1/")
                , "http://localhost:9000/api/rdp/discovery/search/v1/"
  )

  expect_error( Construct_url(service = "wrongservice", EndPoint = "discovery/search/v1/")
                , "wrong service selected in function Construct_url, only rdp, udf or eikon allowed but wrongservice is chosen"
  )
})

test_that("send_json_request can make a GET and POST request", {
  withr::local_options(.EikonApiKey = "testing_key")

  # Uses httptest2 fixtures recorded from fakerapi.it and jsonplaceholder.typicode.com.
  # Fixtures live in tests/testthat/json-ext/.
  with_mock_dir("json-ext", {

    test_GET <- send_json_request(json = list(), request_type = "GET",
                                  url = "https://fakerapi.it/api/v1/companies?_seed=12456")

    expect_equal(lapply(test_GET, class),
                 list(status = "character", code = "integer", locale = "character",
                      seed = "character", total = "integer", data = "list"))

    directions <- 'DataGrid_StandardAsync'
    payload <- list('requests' = list(list(
      'instruments' = list("TSLA.O"),
      'fields' = lapply(list("TR.RICCode"), function(x) list("name" = x))
    )))
    json <- json_builder(directions, payload)

    test_POST <- send_json_request(json = json, request_type = "POST",
                                   url = "https://jsonplaceholder.typicode.com/todos/1/posts")

    expect_equal(test_POST, list(Entity = list(E = "DataGrid_StandardAsync",
      W = list(requests = list(list(instruments = list("TSLA.O"),
                                    fields = list(list(name = "TR.RICCode")))))),
      todoId = "1", id = 101L))

  })
})


## Test build_request includes req_retry policy ----

test_that("build_request inside send_json_request includes req_retry policy", {
  withr::local_options(
    refinitiv_base_url = "http://localhost",
    eikon_port = 9000L,
    eikon_api = "/api/udf/"
  )

  # Capture the request object by stubbing req_perform to return it
  captured_req <- NULL
  mockery::stub(send_json_request, "httr2::req_perform", function(req) {
    captured_req <<- req
    # Return a minimal response that will break out of the polling loop
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"result": "ok"}')
    )
  })

  suppressMessages(
    send_json_request(json = list(test = TRUE), request_type = "POST",
                      apikey = "test_key")
  )

  expect_false(is.null(captured_req))
  # req_retry sets retry policies on the request object
  expect_equal(captured_req$policies$retry_max_tries, 3L)
  expect_true(is.function(captured_req$policies$retry_is_transient))

  # Verify is_transient matches 429 and 503, rejects others
  mock_429 <- httr2::response(status_code = 429L)
  mock_503 <- httr2::response(status_code = 503L)
  mock_200 <- httr2::response(status_code = 200L)
  mock_500 <- httr2::response(status_code = 500L)

  is_transient <- captured_req$policies$retry_is_transient
  expect_true(is_transient(mock_429))
  expect_true(is_transient(mock_503))
  expect_false(is_transient(mock_200))
  # 500 is NOT transient at HTTP level — handled by LSEG JSON polling loop
  expect_false(is_transient(mock_500))
})

test_that("build_request respects refinitiv_rate_limit_max_wait option", {
  withr::local_options(
    refinitiv_base_url = "http://localhost",
    eikon_port = 9000L,
    eikon_api = "/api/udf/",
    refinitiv_rate_limit_max_wait = 120
  )

  captured_req <- NULL
  mockery::stub(send_json_request, "httr2::req_perform", function(req) {
    captured_req <<- req
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"result": "ok"}')
    )
  })

  suppressMessages(
    send_json_request(json = list(test = TRUE), request_type = "POST",
                      apikey = "test_key")
  )

  expect_equal(captured_req$policies$retry_max_wait, 120)
})

test_that("send_json_request warns when polling loop exhausts retries", {
  withr::local_options(
    refinitiv_base_url = "http://localhost",
    eikon_port = 9000L,
    eikon_api = "/api/udf/"
  )

  # Always return a retriable error code (2504)
  mockery::stub(send_json_request, "httr2::req_perform", function(req) {
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"ErrorCode": 2504, "ErrorMessage": "Backend timeout"}')
    )
  })
  mockery::stub(send_json_request, "Sys.sleep", function(...) invisible(NULL))

  expect_warning(
    suppressMessages(
      result <- send_json_request(json = list(test = TRUE), request_type = "POST",
                                  apikey = "test_key")
    ),
    "polling loop exhausted"
  )
  expect_identical(result, NA)
})

dump_refinitiv_options("test-JSON")
