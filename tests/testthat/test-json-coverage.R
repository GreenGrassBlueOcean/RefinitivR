# tests/testthat/test-json-coverage.R
# Additional tests to improve coverage of R/json.r
# Targets: async ticket body rewrite (line 236), closure method optional params
# (lines 528/533, 566/572/577, 614/620/625), and get_news_headlines UDF (778-798).

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


# ═══════════════════════════════════════════════════════════════════════════════
# Line 236: ConstructTicketJsonBody inside send_json_request async polling
# ═══════════════════════════════════════════════════════════════════════════════

test_that("send_json_request rewrites json body via ConstructTicketJsonBody on async ticket", {
  local_refinitiv_state()

  call_count <- 0L

  mockery::stub(send_json_request, "httr2::req_perform", function(req) {
    call_count <<- call_count + 1L
    if (call_count == 1L) {
      body <- '{"estimatedDuration": 500, "ticket": "TKT_42"}'
    } else {
      body <- '{"data": "final_result"}'
    }
    resp <- httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(body)
    )
    # Attach the original request so query$request$body is non-NULL
    resp$request <- list(
      body = list(
        data = list(Entity = list(
          E = "DataGrid_StandardAsync",
          W = list(requests = list(list(instruments = "AAPL.O")))
        ))
      )
    )
    resp
  })
  mockery::stub(send_json_request, "Sys.sleep", function(...) invisible(NULL))

  result <- suppressMessages(
    send_json_request(
      json = list(Entity = list(
        E = "DataGrid_StandardAsync",
        W = list(requests = list(list(instruments = "AAPL.O")))
      )),
      request_type = "POST",
      url = "http://lh:9000/api/udf/",
      apikey = "k"
    )
  )

  expect_equal(result$data, "final_result")
  expect_equal(call_count, 2L)
})


# ═══════════════════════════════════════════════════════════════════════════════
# Lines 528, 533: get_historical_pricing with non-NULL fields & sessions
# ═══════════════════════════════════════════════════════════════════════════════

test_that("get_historical_pricing includes fields and sessions in endpoint", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    list(
      universe = "VOD.L",
      headers = list(list(title = "Date"), list(title = "HIGH"), list(title = "LOW")),
      data = list(list("2024-01-02", 110, 95))
    )
  })

  conn$get_historical_pricing(
    EikonObject = conn,
    universe    = "VOD.L",
    interval    = "P1D",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = NULL,
    count       = 20L,
    fields      = c("HIGH", "LOW"),
    sessions    = c("normal", "pre")
  )

  expect_true(grepl("fields=HIGH,LOW", captured$endpoint))
  expect_true(grepl("sessions=normal,pre", captured$endpoint))
})


# ═══════════════════════════════════════════════════════════════════════════════
# Lines 566, 572, 577: get_intraday_custominstrument_pricing non-NULL optional params
# ═══════════════════════════════════════════════════════════════════════════════

test_that("get_intraday_custominstrument_pricing includes adjustments/fields/sessions", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    list(
      universe = "S)test.UUID-1",
      headers = list(list(title = "DATE_TIME"), list(title = "BID")),
      data = list(list("2024-01-02T10:00:00", 100))
    )
  })

  conn$get_intraday_custominstrument_pricing(
    EikonObject = conn,
    universe    = "S)test.UUID-1",
    interval    = "PT1H",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = c("exchangeCorrection", "CCH"),
    count       = 50L,
    fields      = c("BID", "ASK", "MID"),
    sessions    = c("normal")
  )

  expect_true(grepl("adjustments=exchangeCorrection,CCH", captured$endpoint))
  expect_true(grepl("fields=BID,ASK,MID", captured$endpoint))
  expect_true(grepl("sessions=normal", captured$endpoint))
})


# ═══════════════════════════════════════════════════════════════════════════════
# Lines 614, 620, 625: get_interday_custominstrument_pricing non-NULL optional params
# ═══════════════════════════════════════════════════════════════════════════════

test_that("get_interday_custominstrument_pricing includes adjustments/fields/sessions", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    list(
      universe = "S)test.UUID-2",
      headers = list(list(title = "Date"), list(title = "CLOSE")),
      data = list(list("2024-01-02", 150.5))
    )
  })

  conn$get_interday_custominstrument_pricing(
    EikonObject = conn,
    universe    = "S)test.UUID-2",
    interval    = "P1D",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = c("exchangeCorrection"),
    count       = 100L,
    fields      = c("CLOSE", "OPEN"),
    sessions    = c("normal", "post")
  )

  expect_true(grepl("adjustments=exchangeCorrection", captured$endpoint))
  expect_true(grepl("fields=CLOSE,OPEN", captured$endpoint))
  expect_true(grepl("sessions=normal,post", captured$endpoint))
})


# ═══════════════════════════════════════════════════════════════════════════════
# NULL optional params: adjustments, fields, sessions stripped from endpoint
# ═══════════════════════════════════════════════════════════════════════════════

test_that("get_historical_pricing excludes NULL adjustments/fields/sessions", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    list(
      universe = "VOD.L",
      headers = list(list(title = "Date"), list(title = "CLOSE")),
      data = list(list("2024-01-02", 100))
    )
  })

  conn$get_historical_pricing(
    EikonObject = conn,
    universe    = "VOD.L",
    interval    = "P1D",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = NULL,
    count       = 20L,
    fields      = NULL,
    sessions    = NULL
  )

  expect_false(grepl("adjustments", captured$endpoint))
  expect_false(grepl("fields", captured$endpoint))
  expect_false(grepl("sessions", captured$endpoint))
})

test_that("get_intraday_custominstrument_pricing excludes NULL optional params", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    list(
      universe = "S)test.UUID-1",
      headers = list(list(title = "DATE_TIME"), list(title = "BID")),
      data = list(list("2024-01-02T10:00:00", 100))
    )
  })

  conn$get_intraday_custominstrument_pricing(
    EikonObject = conn,
    universe    = "S)test.UUID-1",
    interval    = "PT1H",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = NULL,
    count       = 50L,
    fields      = NULL,
    sessions    = NULL
  )

  expect_false(grepl("adjustments", captured$endpoint))
  expect_false(grepl("fields", captured$endpoint))
  expect_false(grepl("sessions", captured$endpoint))
})

test_that("get_interday_custominstrument_pricing excludes NULL optional params", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    list(
      universe = "S)test.UUID-2",
      headers = list(list(title = "Date"), list(title = "CLOSE")),
      data = list(list("2024-01-02", 150.5))
    )
  })

  conn$get_interday_custominstrument_pricing(
    EikonObject = conn,
    universe    = "S)test.UUID-2",
    interval    = "P1D",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = NULL,
    count       = 100L,
    fields      = NULL,
    sessions    = NULL
  )

  expect_false(grepl("adjustments", captured$endpoint))
  expect_false(grepl("fields", captured$endpoint))
  expect_false(grepl("sessions", captured$endpoint))
})


# ═══════════════════════════════════════════════════════════════════════════════
# Lines 778-798: get_news_headlines UDF closure method
# ═══════════════════════════════════════════════════════════════════════════════

test_that("get_news_headlines builds correct UDF payload", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$json <<- json
    captured$service <<- service
    captured$method <<- request_type
    list(headlines = list(
      data.table(
        storyId = "urn:newsml:test:20240101:nABC123:1",
        text = "Test headline",
        sourceCode = "RTRS"
      )
    ))
  })

  result <- conn$get_news_headlines(
    query      = "R:MSFT.O",
    count      = 5L,
    repository = "NewsWire",
    date_from  = "2024-01-01T00:00:00",
    date_to    = "2024-01-31T00:00:00",
    raw_output = TRUE,
    debug      = FALSE
  )

  # Legacy UDF service
  expect_equal(captured$service, "udf")
  expect_equal(captured$method, "POST")
  # json_builder wraps in Entity
  expect_equal(captured$json$Entity$E, "News_Headlines")
  # Payload fields
  expect_equal(captured$json$Entity$W$query, "R:MSFT.O")
  expect_equal(captured$json$Entity$W$number, "5")
  expect_equal(captured$json$Entity$W$repository, "NewsWire")
  expect_equal(captured$json$Entity$W$dateFrom, "2024-01-01T00:00:00")
  expect_equal(captured$json$Entity$W$dateTo, "2024-01-31T00:00:00")
  # productName should be the API key from vault
  expect_equal(captured$json$Entity$W$productName, "test_key")
})

test_that("get_news_headlines drops NULL date params from payload", {
  local_refinitiv_state()

  captured <- list()
  conn <- make_mock_conn(function(json = NULL, service = "eikon", debug = FALSE,
                                  request_type = "POST", EndPoint = NULL, ...) {
    captured$json <<- json
    list(headlines = list())
  })

  conn$get_news_headlines(
    query      = "AAPL",
    count      = 10L,
    repository = "NewsWire",
    date_from  = NULL,
    date_to    = NULL,
    raw_output = TRUE,
    debug      = FALSE
  )

  # NULL date_from and date_to should be stripped from payload
  expect_null(captured$json$Entity$W$dateFrom)
  expect_null(captured$json$Entity$W$dateTo)
  # But query and number should still be present
  expect_equal(captured$json$Entity$W$query, "AAPL")
  expect_equal(captured$json$Entity$W$number, "10")
})


restore_refinitiv_state(.saved_state, "test-json-coverage")
