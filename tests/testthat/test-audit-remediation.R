# Regression tests for the 16-item code audit remediation (High #1-#3, Medium #4-#9, Low #10-#16)

library(testthat)
library(mockery)
library(data.table)

.saved_state <- save_refinitiv_state()

# =============================================================================
# #1 (High): Streaming batched WebSocket JSON array frames
# =============================================================================

test_that("#1: StreamManager .process_message processes all elements of batched JSON arrays", {
  local_refinitiv_state()

  mgr <- StreamManager$new(stream_type = "pricing")
  priv <- mgr$.__enclos_env__$private

  pong_sent <- character(0)
  priv$.ws <- list(
    send = function(msg) pong_sent <<- c(pong_sent, msg)
  )

  routed <- list()
  priv$.message_handlers[["2"]] <- list(function(msg, tp) {
    routed[[length(routed) + 1L]] <<- list(msg = msg, tp = tp)
  })
  priv$.message_handlers[["3"]] <- list(function(msg, tp) {
    routed[[length(routed) + 1L]] <<- list(msg = msg, tp = tp)
  })

  # Batched frame containing Refresh (ID 2), Ping, and Update (ID 3)
  batched_json <- jsonlite::toJSON(
    list(
      list(Type = "Refresh", ID = 2L, Fields = list(BID = 1.08, ASK = 1.09)),
      list(Type = "Ping"),
      list(Type = "Update", ID = 3L, Fields = list(BID = 150.25))
    ),
    auto_unbox = TRUE
  )

  priv$.process_message(list(data = as.character(batched_json)), debug = FALSE)

  expect_equal(length(routed), 2L)
  expect_equal(routed[[1]]$tp, "refresh")
  expect_equal(routed[[1]]$msg$ID, 2L)
  expect_equal(routed[[2]]$tp, "update")
  expect_equal(routed[[2]]$msg$ID, 3L)
  expect_equal(length(pong_sent), 1L)
  expect_true(grepl("Pong", pong_sent[1]))
})


# =============================================================================
# #2 (High): TestDataStreamCredentials HTTPS, URL-encoding, and password scrubbing
# =============================================================================

test_that("#2: TestDataStreamCredentials uses HTTPS, URL-encodes credentials, and never leaks password", {
  local_refinitiv_state()

  captured_url <- NULL
  secret_pw <- "MyS3cr3t&Pass#word%123"

  stub(TestDataStreamCredentials, "httr2::request", function(url) {
    captured_url <<- url
    httr2::request(url)
  })
  stub(TestDataStreamCredentials, "httr2::req_perform", function(req) {
    stop(paste0("Failed to connect to ", req$url))
  })

  msgs <- character(0)
  warns <- character(0)
  res <- withCallingHandlers(
    withCallingHandlers(
      TestDataStreamCredentials(
        DatastreamUsername = "user@domain.com",
        DatastreamPassword = secret_pw
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(res)
  expect_true(grepl("^https://", captured_url))
  expect_false(grepl("&Pass#", captured_url, fixed = TRUE))
  expect_true(grepl(utils::URLencode(secret_pw, reserved = TRUE, repeated = TRUE), captured_url, fixed = TRUE))

  # Neither messages nor warnings may contain the raw or URL-encoded password
  all_output <- paste(c(msgs, warns), collapse = "\n")
  expect_false(grepl(secret_pw, all_output, fixed = TRUE))
  expect_false(grepl(utils::URLencode(secret_pw, reserved = TRUE, repeated = TRUE), all_output, fixed = TRUE))
  expect_false(grepl("password=", all_output, fixed = TRUE))

  # Also verify HTTP 401 path does not leak password in warning
  stub(TestDataStreamCredentials, "httr2::req_perform", function(req) {
    httr2::response(status_code = 401L, body = charToRaw("{}"))
  })
  warns_401 <- capture_warnings(
    res_401 <- TestDataStreamCredentials("user@domain.com", secret_pw)
  )
  expect_false(res_401)
  expect_false(any(grepl(secret_pw, warns_401, fixed = TRUE)))
})


# =============================================================================
# #3 (High): verbose = "verbose" on cache hits and in rd_ConvertSymbol
# =============================================================================

test_that("#3: verbose = 'verbose' does not error on cache hits across data functions", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = "verbose", refinitiv_chunk_sleep = 0)
  suppressMessages(rd_ClearCache())

  # Pre-populate cache entries via first call and trigger cache hits on second call
  raw_chunk <- list(
    headers = list(list(
      list(name = "Instrument", displayName = "Instrument"),
      list(name = "Price", displayName = "Price")
    )),
    data = list(list("AAPL.O", 180))
  )
  dummy_conn <- list(
    set_app_key = function(...) invisible(NULL),
    get_data = function(...) raw_chunk,
    get_timeseries = function(...) {
      list(timeseriesData = list(list(
        ric = "AAPL.O",
        statusCode = "Normal",
        fields = list(list(name = "TIMESTAMP"), list(name = "CLOSE")),
        dataPoints = list(list("2024-01-02T00:00:00Z", 180))
      )))
    },
    get_symbology = function(...) {
      list(mappedSymbols = list(list(symbol = "AAPL.O", bestMatch = list(ISIN = "US0378331005"))))
    }
  )

  # 1. rd_GetData
  suppressMessages(rd_GetData(RDObject = dummy_conn, rics = "AAPL.O", Eikonformulas = "TR.Price", cache = TRUE))
  expect_message(
    res_rd <- rd_GetData(RDObject = dummy_conn, rics = "AAPL.O", Eikonformulas = "TR.Price", cache = TRUE),
    "Cache hit"
  )
  expect_equal(nrow(res_rd), 1L)

  # 2. EikonGetData
  suppressMessages(EikonGetData(EikonObject = dummy_conn, rics = "AAPL.O", Eikonformulas = "TR.Price", cache = TRUE))
  expect_message(
    res_ek <- EikonGetData(EikonObject = dummy_conn, rics = "AAPL.O", Eikonformulas = "TR.Price", cache = TRUE),
    "Cache hit"
  )
  expect_equal(nrow(res_ek$PostProcessedEikonGetData), 1L)

  # 3. EikonGetTimeseries
  suppressMessages(EikonGetTimeseries(EikonObject = dummy_conn, rics = "AAPL.O", fields = "CLOSE", cache = TRUE))
  expect_message(
    res_ts <- EikonGetTimeseries(EikonObject = dummy_conn, rics = "AAPL.O", fields = "CLOSE", cache = TRUE),
    "Cache hit"
  )
  expect_equal(nrow(res_ts), 1L)

  # 4. EikonGetSymbology
  suppressWarnings(suppressMessages(
    EikonGetSymbology(EikonObject = dummy_conn, symbol = "AAPL.O", to_symbol_type = "ISIN", cache = TRUE)
  ))
  expect_message(
    suppressWarnings(
      res_sym <- EikonGetSymbology(EikonObject = dummy_conn, symbol = "AAPL.O", to_symbol_type = "ISIN", cache = TRUE)
    ),
    "Cache hit"
  )
  expect_equal(nrow(res_sym), 1L)

  # 5. rd_ConvertSymbol with verbose = "verbose"
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("symbology boom"))
  stub(rd_ConvertSymbol, "EikonGetData", function(...) stop("bare ric boom"))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, fields, start, debug, ...) {
    expect_true(is.logical(debug))
    stop("history boom")
  })
  expect_no_error(
    suppressMessages(
      rd_ConvertSymbol(symbols = "AAPL", from_symbol_type = "RIC", to_symbol_type = "RIC", verbose = "verbose")
    )
  )
})


# =============================================================================
# #4 (Medium): Row order preserved regardless of cache setting
# =============================================================================

test_that("#4: rd_GetData, EikonGetData, and EikonGetSymbology preserve input instrument order when caching", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = FALSE, refinitiv_chunk_sleep = 0)
  suppressMessages(rd_ClearCache())

  input_rics <- c("MSFT.O", "AAPL.O", "GOOG.O")

  mock_conn <- list(
    set_app_key = function(...) invisible(NULL),
    get_data = function(instruments, ...) {
      list(
        headers = list(list(
          list(name = "Instrument", displayName = "Instrument"),
          list(name = "Price", displayName = "Price")
        )),
        data = lapply(seq_along(instruments), function(i) list(instruments[[i]], i * 100))
      )
    },
    get_symbology = function(symbol, ...) {
      list(
        mappedSymbols = lapply(symbol, function(s) {
          list(symbol = s, bestMatch = list(ISIN = paste0("ISIN_", s)))
        })
      )
    }
  )

  # rd_GetData: cache = FALSE vs cache = TRUE (miss) vs cache = TRUE (hit) vs reordered hit
  res_nocache <- rd_GetData(RDObject = mock_conn, rics = input_rics, Eikonformulas = "TR.Price", cache = FALSE)
  res_cache_miss <- rd_GetData(RDObject = mock_conn, rics = input_rics, Eikonformulas = "TR.Price", cache = TRUE)
  res_cache_hit <- rd_GetData(RDObject = mock_conn, rics = input_rics, Eikonformulas = "TR.Price", cache = TRUE)
  reordered_rics <- c("GOOG.O", "MSFT.O", "AAPL.O")
  res_reordered_hit <- rd_GetData(RDObject = mock_conn, rics = reordered_rics, Eikonformulas = "TR.Price", cache = TRUE)

  expect_equal(res_nocache$Instrument, input_rics)
  expect_equal(res_cache_miss$Instrument, input_rics)
  expect_equal(res_cache_hit$Instrument, input_rics)
  expect_equal(res_reordered_hit$Instrument, reordered_rics)

  # EikonGetData
  ek_miss <- EikonGetData(EikonObject = mock_conn, rics = input_rics, Eikonformulas = "TR.Price", cache = TRUE)
  ek_hit_reordered <- EikonGetData(EikonObject = mock_conn, rics = reordered_rics, Eikonformulas = "TR.Price", cache = TRUE)
  expect_equal(ek_miss$PostProcessedEikonGetData$Instrument, input_rics)
  expect_equal(ek_hit_reordered$PostProcessedEikonGetData$Instrument, reordered_rics)

  # EikonGetSymbology
  sym_miss <- suppressWarnings(
    EikonGetSymbology(EikonObject = mock_conn, symbol = input_rics, from_symbol_type = "RIC", to_symbol_type = "ISIN", cache = TRUE)
  )
  sym_hit_reordered <- suppressWarnings(
    EikonGetSymbology(EikonObject = mock_conn, symbol = reordered_rics, from_symbol_type = "RIC", to_symbol_type = "ISIN", cache = TRUE)
  )
  expect_equal(sym_miss$RIC, input_rics)
  expect_equal(sym_hit_reordered$RIC, reordered_rics)
})


# =============================================================================
# #5 & #12 (Medium & Low): chunked_download sleep, NULL return, and cache count
# =============================================================================

test_that("#5 & #12: chunked_download skips sleep after last chunk, preserves NULL elements, and reports true cache count", {
  local_refinitiv_state()
  suppressMessages(rd_ClearCache())

  # 1. Single chunk: 0 sleeps
  sleep_calls_1 <- 0L
  stub(chunked_download, "Sys.sleep", function(...) sleep_calls_1 <<- sleep_calls_1 + 1L)
  res1 <- chunked_download(
    n_chunks = 1L,
    fetch_fn = function(j) list(ok = j),
    sleep = 1.0,
    verbose = FALSE
  )
  expect_equal(sleep_calls_1, 0L)
  expect_equal(length(res1), 1L)

  # 2. 3 chunks: sleeps only between 1->2 and 2->3 (2 sleeps total)
  sleep_calls_3 <- 0L
  stub(chunked_download, "Sys.sleep", function(...) sleep_calls_3 <<- sleep_calls_3 + 1L)
  res3 <- chunked_download(
    n_chunks = 3L,
    fetch_fn = function(j) list(ok = j),
    sleep = 0.5,
    verbose = FALSE
  )
  expect_equal(sleep_calls_3, 2L)
  expect_equal(length(res3), 3L)

  # 3. fetch_fn returning NULL does not shrink results list
  res_null <- chunked_download(
    n_chunks = 2L,
    fetch_fn = function(j) if (j == 1L) NULL else "second_chunk",
    is_success = function(x) TRUE,
    sleep = 0,
    verbose = FALSE
  )
  expect_equal(length(res_null), 2L)
  expect_null(res_null[[1L]])
  expect_equal(res_null[[2L]], "second_chunk")

  # 4. (N from cache) count only counts actual cached chunks
  key_fn <- function(j) paste0("test_chunk_key_", j)
  cache_set(key_fn(1L), "cached_val_1", ttl = 300)

  msgs <- capture_messages(
    res_cached <- chunked_download(
      n_chunks = 2L,
      fetch_fn = function(j) paste0("fresh_val_", j),
      sleep = 0,
      verbose = TRUE,
      chunk_cache_key_fn = key_fn,
      cache_ttl = 300
    )
  )
  expect_equal(res_cached, list("cached_val_1", "fresh_val_2"))
  expect_true(any(grepl("\\(1 from cache\\)", msgs)))
})


# =============================================================================
# #6 & #14 (Medium & Low): HTTP status checks, ErrorCode 400 fail-fast, backoff
# =============================================================================

test_that("#6: send_json_request checks HTTP status codes and handles non-JSON error bodies", {
  local_refinitiv_state()

  # 1. Non-JSON 502 HTML error page
  stub(send_json_request, "httr2::req_perform", function(req) {
    httr2::response(
      status_code = 502L,
      headers = list("Content-Type" = "text/html"),
      body = charToRaw("<html><body>502 Bad Gateway</body></html>")
    )
  })
  expect_error(
    send_json_request(json = list(a = 1), request_type = "POST", url = "http://lh:9000/api/udf/", apikey = "k"),
    "HTTP 502 error \\(non-JSON response\\): <html><body>502 Bad Gateway</body></html>"
  )

  # 2. JSON 403 error response with RDP error object
  stub(send_json_request, "httr2::req_perform", function(req) {
    httr2::response(
      status_code = 403L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"error": {"message": "Insufficient scope for this endpoint"}}')
    )
  })
  expect_error(
    send_json_request(json = NULL, request_type = "GET", url = "http://lh:9000/api/rdp/test", apikey = "k"),
    "HTTP 403 error: Insufficient scope for this endpoint"
  )

  # 3. DELETE with HTTP 500
  stub(send_json_request, "httr2::req_perform", function(req) {
    httr2::response(
      status_code = 500L,
      headers = list("Content-Type" = "text/plain"),
      body = charToRaw("Internal Error")
    )
  })
  expect_error(
    send_json_request(json = NULL, request_type = "DELETE", url = "http://lh:9000/api/rdp/test", apikey = "k"),
    "HTTP 500 error on DELETE request"
  )
})

test_that("#14: send_json_request fails immediately on initial ErrorCode 400, retries ticket-poll 400, and sleeps >=5s", {
  local_refinitiv_state()

  # 1. Initial ErrorCode 400 fails immediately without sleeping
  calls_400 <- 0L
  sleeps_400 <- numeric(0)
  stub(send_json_request, "httr2::req_perform", function(req) {
    calls_400 <<- calls_400 + 1L
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"ErrorCode": 400, "ErrorMessage": "Invalid syntax in request"}')
    )
  })
  stub(send_json_request, "Sys.sleep", function(s) sleeps_400 <<- c(sleeps_400, s))

  expect_error(
    send_json_request(json = list(bad = TRUE), request_type = "POST", url = "http://lh:9000/api/udf/", apikey = "k"),
    "Error code: 400 Invalid syntax in request"
  )
  expect_equal(calls_400, 1L)
  expect_length(sleeps_400, 0L)

  # 2. Async ticket poll returning ErrorCode 400 on poll 2 retries with original_json and succeeds
  calls_tkt <- 0L
  sleeps_tkt <- numeric(0)
  stub(send_json_request, "httr2::req_perform", function(req) {
    calls_tkt <<- calls_tkt + 1L
    body <- if (calls_tkt == 1L) {
      '{"estimatedDuration": 1000, "ticket": "TKT_1"}'
    } else if (calls_tkt == 2L) {
      '{"ErrorCode": 400, "ErrorMessage": "Invalid ticket"}'
    } else {
      '{"data": "recovered_ok"}'
    }
    resp <- httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(body)
    )
    resp$request <- list(
      body = list(data = list(Entity = list(E = "DataGrid_StandardAsync", W = list(requests = list(list(instruments = "AAPL.O"))))))
    )
    resp
  })
  stub(send_json_request, "Sys.sleep", function(s) sleeps_tkt <<- c(sleeps_tkt, s))

  res_tkt <- suppressMessages(
    send_json_request(
      json = list(Entity = list(E = "DataGrid_StandardAsync", W = list(requests = list(list(instruments = "AAPL.O"))))),
      request_type = "POST",
      url = "http://lh:9000/api/udf/",
      apikey = "k"
    )
  )
  expect_equal(res_tkt$data, "recovered_ok")
  expect_equal(calls_tkt, 3L)
  # First sleep is 1s (from estimatedDuration = 1000ms), second sleep is 5 * (1 + 1) = 10s
  expect_equal(sleeps_tkt, c(1, 10))

  # 3. Initial ErrorCode 2504 waits 5s on first retry (not 0s)
  calls_2504 <- 0L
  sleeps_2504 <- numeric(0)
  stub(send_json_request, "httr2::req_perform", function(req) {
    calls_2504 <<- calls_2504 + 1L
    body <- if (calls_2504 == 1L) '{"ErrorCode": 2504, "ErrorMessage": "Timeout"}' else '{"data": "ok"}'
    httr2::response(status_code = 200L, headers = list("Content-Type" = "application/json"), body = charToRaw(body))
  })
  stub(send_json_request, "Sys.sleep", function(s) sleeps_2504 <<- c(sleeps_2504, s))

  res_2504 <- suppressMessages(
    send_json_request(json = list(a = 1), request_type = "POST", url = "http://lh:9000/api/udf/", apikey = "k")
  )
  expect_equal(res_2504$data, "ok")
  expect_equal(sleeps_2504, 5)
})


# =============================================================================
# #7 (Medium): rd_GetOwnership pagination stopping guards
# =============================================================================

test_that("#7: rd_GetOwnership stops on duplicate page or max_pages cap", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = FALSE)

  make_adc_page <- function(val, n = 100L) {
    list(
      headers = list(
        list(name = "instrument", title = "Instrument", type = "string"),
        list(name = "holder", title = "Holder", type = "string")
      ),
      data = lapply(seq_len(n), function(i) list("AAPL.O", paste0(val, "_", i)))
    )
  }

  # 1. Duplicate page detection when endpoint ignores offset
  dup_conn <- list(
    get_ownership = function(...) make_adc_page("SAME_HOLDER", 100L)
  )
  expect_warning(
    res_dup <- rd_GetOwnership(
      RDObject = dup_conn,
      universe = "AAPL.O",
      view = "fund/investors",
      stat_type = 1,
      limit = NULL,
      cache = FALSE
    ),
    "duplicate page received"
  )
  expect_equal(nrow(res_dup), 100L)

  # 2. Max page cap when unique 100-row pages keep arriving
  page_counter <- 0L
  inf_conn <- list(
    get_ownership = function(...) {
      page_counter <<- page_counter + 1L
      make_adc_page(paste0("PAGE_", page_counter), 100L)
    }
  )
  withr::with_options(list(refinitiv_ownership_max_pages = 3L), {
    expect_warning(
      res_cap <- rd_GetOwnership(
        RDObject = inf_conn,
        universe = "AAPL.O",
        view = "fund/investors",
        stat_type = 1,
        limit = NULL,
        cache = FALSE
      ),
      "reached maximum page limit \\(3 pages\\)"
    )
  })
  expect_equal(nrow(res_cap), 300L)
})


# =============================================================================
# #8 (Medium): Non-standard date-named columns do not crash rd_OutputProcesser
# =============================================================================

test_that("#8: rd_OutputProcesser and rd_GetHistory do not crash on non-ISO date strings like 'Q1 2023'", {
  local_refinitiv_state()

  # 1. Pure non-ISO date column ("Q1 2023", "Q2 2023")
  raw_qtr <- list(
    headers = list(list(
      list(name = "Instrument", displayName = "Instrument"),
      list(name = "TR.PeriodEndDate", displayName = "Period End Date")
    )),
    data = list(
      list("AAPL.O", "Q1 2023"),
      list("MSFT.O", "Q2 2023")
    )
  )
  dt_qtr <- expect_no_error(rd_OutputProcesser(raw_qtr))
  expect_equal(dt_qtr[["Period End Date"]], c("Q1 2023", "Q2 2023"))

  # 2. Mixed timestamp + non-ISO string in date-named column
  raw_mixed <- list(
    headers = list(list(
      list(name = "Instrument", displayName = "Instrument"),
      list(name = "TR.EventDate", displayName = "Event Date")
    )),
    data = list(
      list("AAPL.O", "2024-01-15T14:30:00Z"),
      list("MSFT.O", "Q1 2023")
    )
  )
  dt_mixed <- expect_no_error(rd_OutputProcesser(raw_mixed))
  expect_equal(dt_mixed[["Event Date"]], c("2024-01-15T14:30:00Z", "Q1 2023"))
})


# =============================================================================
# #9, #13, #16b, #16c: Handshake default, token validation, port resolution
# =============================================================================

test_that("#9 & #13: rd_handshake defaults to force = FALSE and StreamManager resolves port dynamically and validates token", {
  local_refinitiv_state()

  expect_false(formals(rd_handshake)$force)

  # StreamManager resolves port from eikon_port when streaming_port is NULL
  withr::with_options(list(streaming_port = NULL, eikon_port = 9060L), {
    mgr <- StreamManager$new(stream_type = "pricing")
    expect_equal(mgr$.__enclos_env__$private$.port, 9060L)
  })

  # StreamManager$connect() fails cleanly when handshake returns NULL token
  refinitiv_vault_clear()
  mgr2 <- StreamManager$new(stream_type = "pricing")
  stub(mgr2$connect, "rd_handshake", function(...) NULL)
  expect_error(
    mgr2$connect(),
    "Failed to obtain access token via rd_handshake"
  )
})

test_that("#16b: rd_handshake fallback version uses get_pkg_version()", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  captured_body <- NULL
  stub(rd_handshake, "CheckTerminalType", function(...) invisible(NULL))
  stub(rd_handshake, "send_json_request", function(json, ...) {
    captured_body <<- json
    list(access_token = "tok_123", expires_in = 3600L, token_type = "bearer")
  })

  res <- rd_handshake(force = TRUE)
  expect_equal(res$access_token, "tok_123")
  expect_equal(captured_body$LibraryVersion, Refinitiv:::get_pkg_version())
})


# =============================================================================
# #10 & #11 (Low): Historical pricing URL encoding & manage_custom_instrument debug
# =============================================================================

test_that("#10 & #11: get_historical_pricing encodes '#' and '+' in URLs and manage_custom_instrument respects debug = FALSE", {
  local_refinitiv_state()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  captured <- list()
  conn <- RefinitivJsonConnect("test_key")
  closure_env <- environment(conn$get_historical_pricing)
  closure_env$send_json_request <- function(json = NULL, service = "eikon", debug = FALSE,
                                            request_type = "POST", EndPoint = NULL, ...) {
    captured$endpoint <<- EndPoint
    captured$debug <<- debug
    list(data = list())
  }

  conn$get_historical_pricing(
    EikonObject = conn,
    universe    = "0#EURAB6E=R",
    interval    = "P1D",
    start       = "2024-01-01T00:00:00+01:00",
    end         = "2024-01-31T00:00:00+01:00",
    adjustments = NULL,
    count       = 10L,
    fields      = NULL,
    sessions    = NULL,
    debug       = FALSE
  )

  expect_true(grepl("summaries/0%23EURAB6E%3DR\\?", captured$endpoint))
  expect_true(grepl("2024-01-01T00%3A00%3A00%2B01%3A00", captured$endpoint))
  expect_false(grepl("#", captured$endpoint, fixed = TRUE))

  # #11: manage_custom_instrument respects debug = FALSE (default)
  conn$manage_custom_instrument(symbol = "S)inst", operation = "GET", debug = FALSE)
  expect_false(captured$debug)
})


# =============================================================================
# #15 (Low): Credential vault isolation for API key and DataStream credentials
# =============================================================================

test_that("#15: API key and DataStream credentials are stored in vault and never written to options()", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  withr::local_options(
    .EikonApiKey = NULL,
    Datastream.Username = NULL,
    Datastream.Password = NULL,
    eikon_port = 9000L
  )

  conn <- RefinitivJsonConnect("vault_only_api_key")
  expect_equal(refinitiv_vault_get("api_key"), "vault_only_api_key")
  expect_null(getOption(".EikonApiKey"))

  # Calling RefinitivJsonConnect(getOption(".EikonApiKey")) when option is NULL
  # resolves the key from the vault without erroring on is.na(NULL)
  conn2 <- expect_no_error(RefinitivJsonConnect(getOption(".EikonApiKey")))
  expect_equal(conn2$get_app_key(), "vault_only_api_key")

  # DataStreamConnect stores in vault and subsequent NA call reads from vault
  captured_ds <- NULL
  stub(DataStreamConnect, "DatastreamDSWS2R::dsws$new", function(...) {
    captured_ds <<- list(...)
    "dsws_instance"
  })

  DataStreamConnect("vault_ds_user", "vault_ds_pass")
  expect_null(getOption("Datastream.Username"))
  expect_null(getOption("Datastream.Password"))

  # Second call with NA arguments retrieves credentials from the vault
  captured_ds <- NULL
  DataStreamConnect()
  expect_equal(captured_ds$username, "vault_ds_user")
  expect_equal(captured_ds$password, "vault_ds_pass")

  # Follow-up #5: Pre-existing .Rprofile options(Datastream.Username/Password)
  # are NOT cleared by DataStreamConnect()
  refinitiv_vault_clear()
  withr::with_options(
    list(Datastream.Username = "rprofile_user", Datastream.Password = "rprofile_pass"),
    {
      DataStreamConnect()
      expect_equal(getOption("Datastream.Username"), "rprofile_user")
      expect_equal(getOption("Datastream.Password"), "rprofile_pass")
    }
  )
})


# =============================================================================
# Follow-up Audit Findings (F1–F6)
# =============================================================================

test_that("F1: %||% is imported from rlang into the package namespace for R 4.1-4.3 compatibility", {
  ns_imports <- getNamespaceImports("Refinitiv")
  expect_true("rlang" %in% names(ns_imports))
  expect_true("%||%" %in% unname(ns_imports[["rlang"]]))
})

test_that("F2: .restore_input_order remaps Eikon_Error_Data$row and preserves repeated RICs c('A','B','A')", {
  local_refinitiv_state()
  withr::local_options(refinitiv_chunk_sleep = 0)
  suppressMessages(rd_ClearCache())

  # 1. Repeated RICs c("A", "B", "A") preserved with caching off and caching on
  df_sorted <- data.frame(
    Instrument = c("A", "A", "B"),
    Val = c(10, 30, 20),
    stringsAsFactors = FALSE
  )
  reordered <- Refinitiv:::.restore_input_order(df_sorted, c("A", "B", "A"), "Instrument")
  expect_equal(reordered$Instrument, c("A", "B", "A"))
  expect_equal(reordered$Val, c(10, 20, 30))

  # Already in c("A", "B", "A") order stays c("A", "B", "A")
  df_aba <- data.frame(
    Instrument = c("A", "B", "A"),
    Val = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  reordered_aba <- Refinitiv:::.restore_input_order(df_aba, c("A", "B", "A"), "Instrument")
  expect_equal(reordered_aba$Instrument, c("A", "B", "A"))
  expect_equal(reordered_aba$Val, c(10, 20, 30))

  # 2. EikonResult error row indices remapped when data rows are reordered
  eikon_res <- Refinitiv:::new_EikonResult(list(
    PostProcessedEikonGetData = data.frame(
      Instrument = c("AAPL.O", "MSFT.O", "ZZZ.O"),
      Price = c(180, NA, 50),
      stringsAsFactors = FALSE
    ),
    Eikon_Error_Data = data.frame(
      code = c(412L, 251658243L),
      col = c(1L, 1L),
      message = c("Unable to resolve MSFT.O", "Bad field for AAPL.O"),
      row = c(1L, 0L), # 0-indexed: row 1 is MSFT.O, row 0 is AAPL.O
      stringsAsFactors = FALSE
    )
  ))

  # Reorder to c("ZZZ.O", "MSFT.O", "AAPL.O"):
  # ZZZ.O -> row 0, MSFT.O -> row 1, AAPL.O -> row 2
  reordered_res <- Refinitiv:::.restore_input_order(
    eikon_res, c("ZZZ.O", "MSFT.O", "AAPL.O"), "Instrument"
  )
  expect_equal(
    reordered_res$PostProcessedEikonGetData$Instrument,
    c("ZZZ.O", "MSFT.O", "AAPL.O")
  )
  expect_equal(reordered_res$Eikon_Error_Data$row, c(1L, 2L))
  expect_equal(
    reordered_res$Eikon_Error_Data$message,
    c("Unable to resolve MSFT.O", "Bad field for AAPL.O")
  )
})

test_that("F3: rd_handshake(force = FALSE) does not warn on expired token and Login Status: Closed invalidates token", {
  local_refinitiv_state()
  refinitiv_vault_clear()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  # Create a valid-format JWT whose exp is in the past
  header_b64 <- base64enc::base64encode(charToRaw('{"alg":"HS256","typ":"JWT"}'))
  exp_past <- as.integer(Sys.time()) - 300L
  payload_b64 <- base64enc::base64encode(charToRaw(sprintf('{"exp":%d}', exp_past)))
  expired_jwt <- paste(header_b64, payload_b64, "sig", sep = ".")

  refinitiv_vault_set("access_token", expired_jwt)
  refinitiv_vault_set("token_expiration", exp_past)
  refinitiv_vault_set("token_type", "bearer")

  stub(rd_handshake, "CheckTerminalType", function(...) invisible(NULL))
  stub(rd_handshake, "send_json_request", function(...) {
    list(access_token = "fresh_tok", expires_in = 3600L, token_type = "bearer")
  })

  # Routine token refresh in rd_handshake(force = FALSE) should NOT warn
  expect_no_warning(res <- rd_handshake(force = FALSE))
  expect_equal(res$access_token, "fresh_tok")

  # Login Status: Closed in StreamManager clears the cached token
  mgr <- StreamManager$new(stream_type = "pricing")
  priv <- mgr$.__enclos_env__$private
  priv$.logged_in <- TRUE
  expect_equal(refinitiv_vault_get("access_token"), "fresh_tok")

  login_closed_msg <- list(
    ID = 1L,
    Domain = "Login",
    Type = "Status",
    State = list(Stream = "Closed", Data = "Suspect", Code = "ForceLogout", Text = "Token rejected")
  )
  suppressWarnings(priv$.handle_status(login_closed_msg))
  expect_false(priv$.logged_in)
  expect_null(refinitiv_vault_get("access_token"))
})

test_that("F4: permanent HTTP 4xx errors are not retried by retry() or chunked_download()", {
  local_refinitiv_state()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  # 1. retry() stops after 1 attempt on refinitiv_http_error
  attempts <- 0L
  expect_warning(
    res_na <- Refinitiv:::retry(
      function() {
        attempts <<- attempts + 1L
        stop(errorCondition("HTTP 401 error: Unauthorized", class = "refinitiv_http_error", http_status = 401L))
      },
      max_attempts = 3L,
      backoff = 0,
      on_failure = "NA"
    ),
    "HTTP 401 error"
  )
  expect_equal(attempts, 1L)
  expect_true(isTRUE(attr(res_na, "permanent_error")))

  # 2. chunked_download() does not retry chunks that fail with permanent_error
  chunk_calls <- 0L
  expect_error(
    Refinitiv:::chunked_download(
      n_chunks = 1L,
      fetch_fn = function(j) {
        chunk_calls <<- chunk_calls + 1L
        structure(NA, permanent_error = TRUE)
      },
      max_retries = 3L,
      sleep = 0,
      backoff = 0,
      on_failure = "stop",
      verbose = FALSE,
      fail_message = "auth failed"
    ),
    "auth failed"
  )
  expect_equal(chunk_calls, 1L)
})

test_that("F6: error in one message of a batched WebSocket frame does not drop remaining messages", {
  local_refinitiv_state()
  mgr <- StreamManager$new(stream_type = "pricing")
  priv <- mgr$.__enclos_env__$private

  stub(priv$.process_message, "private$.handle_refresh", function(msg, debug) {
    stop("malformed refresh payload")
  })

  received_updates <- list()
  mgr$register_handler("2", function(msg, type) {
    received_updates[[length(received_updates) + 1L]] <<- msg
  })

  batched_json <- jsonlite::toJSON(
    list(
      list(ID = 2L, Type = "Refresh", Fields = list(BID = 1.1)),
      list(ID = 2L, Type = "Update", Fields = list(BID = 1.2))
    ),
    auto_unbox = TRUE
  )

  expect_warning(
    priv$.process_message(list(data = batched_json), debug = FALSE),
    "malformed refresh payload"
  )
  expect_length(received_updates, 1L)
  expect_equal(received_updates[[1L]]$Fields$BID, 1.2)
})

restore_refinitiv_state(.saved_state, "test-audit-remediation")
