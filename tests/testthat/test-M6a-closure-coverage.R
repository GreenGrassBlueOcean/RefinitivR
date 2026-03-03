# Tests for M6a closure coverage (#9–#14)
#
# Uses closure environment injection to test RefinitivJsonConnect() closure
# methods without httptest2 fixtures. Mocks are injected directly into the
# closure's enclosing environment, which is robust under covr instrumentation.
# See test-M6a-coverage-quickwins.R for the pattern rationale.

library(testthat)
library(mockery)
library(data.table)

.saved_state <- save_refinitiv_state()


# ── Helper: create a connection with mocked send_json_request ──

make_mock_conn <- function(mock_fn, .env = parent.frame()) {
  withr::local_options(
    eikon_port = 9000L,
    refinitiv_base_url = "http://lh",
    .envir = .env
  )
  conn <- RefinitivJsonConnect(Eikonapplication_id = "test_key")
  closure_env <- environment(conn$get_data_rdp)
  closure_env$send_json_request <- mock_fn
  conn
}


# ── #9: get_data_rdp closure (lines 391-410) ──

test_that("get_data_rdp sends correct payload and endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$payload <<- json
    captured$service <<- service
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(headers = list(list(name = "Close")), data = list(list(150.5)))
  })

  result <- conn$get_data_rdp(
    universe = c("AAPL.O", "GOOG.O"),
    fields = c("TR.CLOSE", "TR.OPEN"),
    parameters = list(SDate = "2022-10-05", EDate = "2022-11-05"),
    output = "Col,T|Va,Row,In,date|",
    debug = FALSE,
    raw_output = FALSE
  )

  expect_equal(captured$endpoint, "data/datagrid/beta1/")
  expect_equal(captured$service, "rdp")
  expect_equal(captured$method, "POST")
  # JsonListBuilder wraps single values in list(); multi-element vectors stay as vectors
  expect_equal(captured$payload$universe, c("AAPL.O", "GOOG.O"))
  expect_equal(captured$payload$fields, c("TR.CLOSE", "TR.OPEN"))
  expect_equal(captured$payload$parameters$SDate, "2022-10-05")
  expect_equal(captured$payload$output, "Col,T|Va,Row,In,date|")
})

test_that("get_data_rdp drops empty parameters", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, ...) {
    captured$payload <<- json
    list(headers = list(list(name = "Close")), data = list(list(150.5)))
  })

  conn$get_data_rdp(
    universe = "AAPL.O", fields = "TR.CLOSE",
    parameters = list(), output = NULL,
    debug = FALSE, raw_output = FALSE
  )

  # Empty parameters and NULL output should be dropped
  expect_null(captured$payload$parameters)
  expect_null(captured$payload$output)
})


# ── #10: Legacy get_news_story (lines 800-816) ──

test_that("get_news_story builds correct UDF payload with Entity wrapper", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$json <<- json
    captured$service <<- service
    captured$method <<- request_type
    captured$endpoint <<- EndPoint
    list(storyHtml = "<p>Test story content</p>")
  })

  result <- conn$get_news_story(
    story_id = "urn:newsml:reuters.com:20240101:nL1N3AB123",
    raw_output = FALSE,
    debug = FALSE
  )

  # Legacy news uses UDF (Entity wrapper), not RDP
  expect_equal(captured$service, "udf")
  expect_equal(captured$method, "POST")
  expect_null(captured$endpoint) # UDF endpoints don't use EndPoint param
  # Entity wrapper should contain News_Story direction
  expect_equal(captured$json$Entity$E, "News_Story")
  # Payload should contain storyId
  expect_equal(
    captured$json$Entity$W$storyId,
    "urn:newsml:reuters.com:20240101:nL1N3AB123"
  )
  expect_equal(result$storyHtml, "<p>Test story content</p>")
})


# ── #11: get_rdp_streaming_url (lines 595-605) ──

test_that("get_rdp_streaming_url sends GET to correct endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    captured$service <<- service
    list(services = list(list(location = list("us-east-1a"), port = 443L)))
  })

  result <- conn$get_rdp_streaming_url(debug = FALSE)

  expect_equal(captured$endpoint, "streaming/pricing/v1/")
  expect_equal(captured$method, "GET")
  expect_equal(captured$service, "rdp")
})


# ── #12: create_custom_instrument + search_custom_instrument (lines 623-661) ──

test_that("create_custom_instrument sends POST with instrument payload", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$payload <<- json
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(instrumentId = "CI_12345", symbol = "S)lseg_test.UUID-123")
  })

  result <- conn$create_custom_instrument(
    symbol         = "MyBasket",
    type           = "basket",
    currency       = "USD",
    instrumentName = "Test Basket",
    exchangeName   = "NYSE",
    description    = "A test basket instrument",
    debug          = FALSE
  )

  expect_equal(captured$endpoint, "data/custom-instruments/v1/instruments")
  expect_equal(captured$method, "POST")
  # NULL fields should be stripped
  expect_equal(captured$payload$symbol, "MyBasket")
  expect_equal(captured$payload$type, "basket")
  expect_equal(captured$payload$currency, "USD")
  expect_null(captured$payload$formula) # was NULL, should be dropped
  expect_null(captured$payload$basket)
  expect_equal(result$instrumentId, "CI_12345")
})

test_that("search_custom_instrument sends GET to search endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(instruments = list(list(symbol = "S)test.UUID")))
  })

  result <- conn$search_custom_instrument(debug = FALSE)

  expect_equal(captured$endpoint, "data/custom-instruments/v1/search")
  expect_equal(captured$method, "GET")
})


# ── #13: manage_custom_instrument GET/UPDATE/DELETE (lines 679-722) ──

test_that("manage_custom_instrument errors when both symbol and Id supplied", {
  local_refinitiv_state()
  conn <- make_mock_conn(function(...) list())

  expect_error(
    conn$manage_custom_instrument(symbol = "SYM", Id = "ID123", operation = "GET"),
    "supply either symbol or Id"
  )
})

test_that("manage_custom_instrument errors on invalid operation", {
  local_refinitiv_state()
  conn <- make_mock_conn(function(...) list())

  expect_error(
    conn$manage_custom_instrument(symbol = "SYM", operation = c("GET", "DELETE")),
    "parameter operation should be length 1"
  )

  expect_error(
    conn$manage_custom_instrument(symbol = "SYM", operation = "PATCH"),
    "parameter operation should be length 1"
  )
})

test_that("manage_custom_instrument GET by symbol builds correct endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(symbol = "MyInstrument", type = "formula")
  })

  result <- conn$manage_custom_instrument(
    symbol = "MyInstrument", operation = "GET", debug = FALSE
  )

  expect_equal(captured$endpoint, "data/custom-instruments/v1/instruments/MyInstrument")
  expect_equal(captured$method, "GET")
})

test_that("manage_custom_instrument GET by Id builds correct endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(symbol = "MyInstrument", type = "formula")
  })

  result <- conn$manage_custom_instrument(
    Id = "CI_99999", operation = "GET", debug = FALSE
  )

  expect_equal(captured$endpoint, "data/custom-instruments/v1/instruments/CI_99999")
})

test_that("manage_custom_instrument DELETE sends DELETE request", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$method <<- request_type
    captured$endpoint <<- EndPoint
    list()
  })

  conn$manage_custom_instrument(
    symbol = "OldInstrument", operation = "DELETE", debug = FALSE
  )

  expect_equal(captured$method, "DELETE")
  expect_equal(captured$endpoint, "data/custom-instruments/v1/instruments/OldInstrument")
})

test_that("manage_custom_instrument UPDATE does GET-then-PUT merge", {
  local_refinitiv_state()

  call_count <- 0L
  captured_calls <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    call_count <<- call_count + 1L
    captured_calls[[call_count]] <<- list(
      payload = json, method = request_type, endpoint = EndPoint
    )
    if (call_count == 1L) {
      # GET response — existing instrument data
      list(
        symbol = "MyInstrument", type = "formula",
        formula = "AAPL.O*2", currency = "USD",
        instrumentName = "Original Name", description = "Original desc"
      )
    } else {
      # PUT response
      list(symbol = "MyInstrument", instrumentName = "Updated Name")
    }
  })

  result <- suppressMessages(
    conn$manage_custom_instrument(
      symbol         = "MyInstrument",
      operation      = "UPDATE",
      instrumentName = "Updated Name",
      description    = "New description",
      debug          = FALSE
    )
  )

  # First call should be GET (to fetch existing data)
  expect_equal(captured_calls[[1]]$method, "GET")
  expect_equal(
    captured_calls[[1]]$endpoint,
    "data/custom-instruments/v1/instruments/MyInstrument"
  )

  # Second call should be PUT with merged payload
  expect_equal(captured_calls[[2]]$method, "PUT")
  # Updated fields should be present
  expect_equal(captured_calls[[2]]$payload$instrumentName, "Updated Name")
  expect_equal(captured_calls[[2]]$payload$description, "New description")
  # Unchanged fields from GET should be preserved
  expect_equal(captured_calls[[2]]$payload$formula, "AAPL.O*2")
  expect_equal(captured_calls[[2]]$payload$currency, "USD")
})


# ── #14: get_intraday/interday_custominstrument_pricing (lines 528-590) ──

test_that("get_intraday_custominstrument_pricing builds correct endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(
      universe = "S)test.UUID-123",
      headers = list(list(title = "DATE_TIME"), list(title = "BID")),
      data = list(list("2024-01-02T10:00:00", 100))
    )
  })

  conn$get_intraday_custominstrument_pricing(
    EikonObject = conn,
    universe    = "S)test.UUID-123",
    interval    = "PT5M",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = c("exchangeCorrection"),
    count       = 100L,
    fields      = c("BID", "ASK"),
    sessions    = c("normal", "post")
  )

  expect_true(grepl("data/custom-instruments/v1/intraday-summaries/", captured$endpoint))
  expect_true(grepl("S)test.UUID-123", captured$endpoint))
  expect_true(grepl("interval=PT5M", captured$endpoint))
  expect_true(grepl("adjustments=exchangeCorrection", captured$endpoint))
  expect_true(grepl("fields=BID,ASK", captured$endpoint))
  expect_true(grepl("sessions=normal,post", captured$endpoint))
  expect_equal(captured$method, "GET")
})

test_that("get_interday_custominstrument_pricing builds correct endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$method <<- request_type
    list(
      universe = "S)test.UUID-123",
      headers = list(list(title = "Date"), list(title = "CLOSE")),
      data = list(list("2024-01-02", 150.5))
    )
  })

  conn$get_interday_custominstrument_pricing(
    EikonObject = conn,
    universe    = "S)test.UUID-123",
    interval    = "P1D",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = NULL,
    count       = 50L,
    fields      = NULL,
    sessions    = NULL
  )

  expect_true(grepl("data/custom-instruments/v1/interday-summaries/", captured$endpoint))
  expect_true(grepl("S)test.UUID-123", captured$endpoint))
  expect_true(grepl("interval=P1D", captured$endpoint))
  # NULL adjustments/fields/sessions should not appear
  expect_false(grepl("adjustments", captured$endpoint))
  expect_false(grepl("fields=", captured$endpoint))
  expect_false(grepl("sessions=", captured$endpoint))
  expect_equal(captured$method, "GET")
})


restore_refinitiv_state(.saved_state, "test-M6a-closure-coverage")
