# Tests for M6a coverage quick wins (#1–#8)
#
# Covers previously-uncovered lines in json.r and HistoricalPricing.r
# that are testable offline without new httptest2 fixtures.

library(testthat)
library(mockery)
library(data.table)

.saved_state <- save_refinitiv_state()


# ── #1: HistoricalPricing cache hit/store (lines 167-172, 258) ──

test_that("rd_GetHistoricalPricing returns cached result on second call", {
  local_refinitiv_state()
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  call_count <- 0L
  mock_response <- list(list(
    universe = "AAPL.O",
    headers  = list(list(title = "Date"), list(title = "BID")),
    data     = list(list("2024-01-02", 100), list("2024-01-03", 101))
  ))

  stub(rd_GetHistoricalPricing, "CheckifCustomInstrument", function(x, ...) FALSE)
  stub(rd_GetHistoricalPricing, "chunked_download", function(n_chunks, fetch_fn, ...) {
    # Execute each chunk's fetch_fn to mirror real behavior
    lapply(seq_len(n_chunks), fetch_fn)
  })
  stub(rd_GetHistoricalPricing, "retry", function(fn, ...) fn())
  stub(rd_GetHistoricalPricing, "rd_OutputProcesser", function(x, ...) {
    call_count <<- call_count + 1L
    data.table::data.table(
      Date = as.Date(c("2024-01-02", "2024-01-03")),
      BID = c(100, 101), Universe = "AAPL.O"
    )
  })

  dummy_RD <- list(
    set_app_key = function(...) invisible(NULL),
    get_historical_pricing = function(...) mock_response
  )

  # First call — cache miss

  result1 <- rd_GetHistoricalPricing(
    RDObject = dummy_RD, universe = "AAPL.O", interval = "P1D",
    count = 2L, cache = TRUE
  )
  expect_equal(call_count, 1L)
  expect_s3_class(result1, "data.frame")

  # Second call — cache hit (lines 167-172)
  result2 <- rd_GetHistoricalPricing(
    RDObject = dummy_RD, universe = "AAPL.O", interval = "P1D",
    count = 2L, cache = TRUE
  )
  expect_equal(call_count, 1L) # NOT incremented
  expect_equal(result1, result2)
})

test_that("rd_GetHistoricalPricing stores result in cache (line 258)", {
  local_refinitiv_state()
  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  stub(rd_GetHistoricalPricing, "CheckifCustomInstrument", function(x, ...) FALSE)
  stub(rd_GetHistoricalPricing, "chunked_download", function(n_chunks, fetch_fn, ...) {
    lapply(seq_len(n_chunks), fetch_fn)
  })
  stub(rd_GetHistoricalPricing, "retry", function(fn, ...) fn())
  stub(rd_GetHistoricalPricing, "rd_OutputProcesser", function(x, ...) {
    data.table::data.table(Date = as.Date("2024-01-02"), BID = 100)
  })

  dummy_RD <- list(
    set_app_key = function(...) invisible(NULL),
    get_historical_pricing = function(...) {
      list(list(
        headers = list(list(title = "Date"), list(title = "BID")),
        data    = list(list("2024-01-02", 100))
      ))
    }
  )

  rd_GetHistoricalPricing(
    RDObject = dummy_RD, universe = "AAPL.O", interval = "P1D",
    count = 1L, cache = TRUE
  )

  # Verify cache was populated
  ck <- Refinitiv:::cache_key(
    "rd_GetHistoricalPricing", "AAPL.O", "P1D",
    NULL, NULL, NULL, 1L, NULL, NULL, "."
  )
  hit <- Refinitiv:::cache_get(ck)
  expect_true(hit$found)
})


# ── #2: HistoricalPricing custom instrument branches (lines 212-235) ──

test_that("rd_GetHistoricalPricing routes intraday custom instrument correctly", {
  local_refinitiv_state()
  withr::local_options(.RefinitivUUID = "ABCDE-123456", refinitiv_cache = FALSE)

  intraday_called <- FALSE
  stub(rd_GetHistoricalPricing, "CheckifCustomInstrument", function(x, ...) TRUE)
  stub(rd_GetHistoricalPricing, "chunked_download", function(n_chunks, fetch_fn, ...) {
    lapply(seq_len(n_chunks), fetch_fn)
  })
  stub(rd_GetHistoricalPricing, "retry", function(fn, ...) fn())
  stub(rd_GetHistoricalPricing, "rd_OutputProcesser", function(x, ...) {
    data.table::data.table(DATE_TIME = "2024-01-02T10:00:00", BID = 100)
  })

  dummy_RD <- list(
    set_app_key = function(...) invisible(NULL),
    get_historical_pricing = function(...) {
      stop("Should not call regular historical pricing for custom instrument")
    },
    get_intraday_custominstrument_pricing = function(...) {
      intraday_called <<- TRUE
      list(list(
        headers = list(list(title = "DATE_TIME"), list(title = "BID")),
        data    = list(list("2024-01-02T10:00:00", 100))
      ))
    },
    get_interday_custominstrument_pricing = function(...) {
      stop("Should not call interday for intraday interval")
    }
  )

  result <- rd_GetHistoricalPricing(
    RDObject = dummy_RD,
    universe = "S)lseg_epam4.ABCDE-123456",
    interval = "PT1M",
    count    = 1L
  )

  expect_true(intraday_called)
  expect_s3_class(result, "data.frame")
})

test_that("rd_GetHistoricalPricing routes interday custom instrument correctly", {
  local_refinitiv_state()
  withr::local_options(.RefinitivUUID = "ABCDE-123456", refinitiv_cache = FALSE)

  interday_called <- FALSE
  stub(rd_GetHistoricalPricing, "CheckifCustomInstrument", function(x, ...) TRUE)
  stub(rd_GetHistoricalPricing, "chunked_download", function(n_chunks, fetch_fn, ...) {
    lapply(seq_len(n_chunks), fetch_fn)
  })
  stub(rd_GetHistoricalPricing, "retry", function(fn, ...) fn())
  stub(rd_GetHistoricalPricing, "rd_OutputProcesser", function(x, ...) {
    data.table::data.table(Date = as.Date("2024-01-02"), BID = 100)
  })

  dummy_RD <- list(
    set_app_key = function(...) invisible(NULL),
    get_historical_pricing = function(...) {
      stop("Should not call regular historical pricing for custom instrument")
    },
    get_intraday_custominstrument_pricing = function(...) {
      stop("Should not call intraday for interday interval")
    },
    get_interday_custominstrument_pricing = function(...) {
      interday_called <<- TRUE
      list(list(
        headers = list(list(title = "Date"), list(title = "BID")),
        data    = list(list("2024-01-02", 100))
      ))
    }
  )

  result <- rd_GetHistoricalPricing(
    RDObject = dummy_RD,
    universe = "S)lseg_epam4.ABCDE-123456",
    interval = "P1D",
    count    = 1L
  )

  expect_true(interday_called)
  expect_s3_class(result, "data.frame")
})


# ── #3: ConstructTicketJsonBody non-Entity branch (line 273) ──

test_that("ConstructTicketJsonBody handles non-Entity body (RDP-style)", {
  # Build a mock query whose body$data does NOT contain an "Entity" key
  mock_query <- list(
    request = list(
      body = list(
        data = list(
          universe = list("AAPL.O"),
          fields   = list("TR.Revenue")
        )
      )
    )
  )

  result <- suppressMessages(
    ConstructTicketJsonBody(query = mock_query, ticket = "ticket_hash_123", debug = FALSE)
  )

  # Non-Entity branch replaces entire body$data with ticket

  expect_equal(result, list(ticket = "ticket_hash_123"))
})


# ── #4: set_app_port / get_app_port closures (lines 345-346) ──

test_that("RefinitivJsonConnect set_app_port and get_app_port work", {
  local_refinitiv_state()

  # Stub CheckTerminalType to avoid probing
  stub(RefinitivJsonConnect, "CheckTerminalType", function(...) invisible(NULL))

  conn <- RefinitivJsonConnect(Eikonapplication_id = "test_key")

  withr::local_options(.EikonApplicationPort = NULL)
  conn$set_app_port(1234L)
  expect_equal(conn$get_app_port(), 1234L)

  conn$set_app_port(5678L)
  expect_equal(conn$get_app_port(), 5678L)
})


# ── #5: build_get_query_string non-list input guard (line 956) ──

test_that("build_get_query_string errors on non-list input", {
  expect_error(build_get_query_string("not a list"), "Input must be a named list")
  expect_error(build_get_query_string(42), "Input must be a named list")
  expect_error(build_get_query_string(NULL), "Input must be a named list")
})


# ── #6: ESG get_esg universe view path (line 828) ──
#
# These tests inject mocks directly into the closure's enclosing environment.
# This is more robust than local_mocked_bindings() because covr's tracing
# changes the namespace environment chain, preventing closures from finding
# namespace-level mocks.

test_that("RefinitivJsonConnect get_esg builds correct endpoint for 'universe' view", {
  local_refinitiv_state()
  # Pre-set port so CheckTerminalType short-circuits
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  conn <- RefinitivJsonConnect(Eikonapplication_id = "test_key")

  captured_endpoint <- NULL
  closure_env <- environment(conn$get_esg)
  closure_env$send_json_request <- function(json = NULL, service = "eikon",
                                            debug = FALSE, request_type = "POST",
                                            EndPoint = NULL, ...) {
    captured_endpoint <<- EndPoint
    list(headers = list(list(name = "instrument")), data = list(list("AAPL.O")))
  }

  result <- conn$get_esg(universe = "AAPL.O", view = "universe")

  # The "universe" view should NOT have "views/" prefix
  expect_true(grepl("environmental-social-governance/v2/universe", captured_endpoint))
  expect_false(grepl("views/universe", captured_endpoint))
})

test_that("RefinitivJsonConnect get_esg builds correct endpoint for non-universe view", {
  local_refinitiv_state()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  conn <- RefinitivJsonConnect(Eikonapplication_id = "test_key")

  captured_endpoint <- NULL
  closure_env <- environment(conn$get_esg)
  closure_env$send_json_request <- function(json = NULL, service = "eikon",
                                            debug = FALSE, request_type = "POST",
                                            EndPoint = NULL, ...) {
    captured_endpoint <<- EndPoint
    list(headers = list(list(name = "instrument")), data = list(list("AAPL.O")))
  }

  result <- conn$get_esg(universe = "AAPL.O", view = "scores-full")

  # Non-universe views use "views/" prefix
  expect_true(grepl("views/scores-full", captured_endpoint))
})


# ── #7: search NullRemover for list columns (lines 456-458) ──

test_that("RefinitivJsonConnect search handles NULL-containing list columns", {
  local_refinitiv_state()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  conn <- RefinitivJsonConnect(Eikonapplication_id = "test_key")

  # Inject mocks into the closure's enclosing environment
  closure_env <- environment(conn$search)
  closure_env$send_json_request <- function(...) {
    list(Hits = list(list(Title = "Apple")))
  }
  # Return a data.table with a list column containing NULLs
  # Each list element must be a single value (NullRemover unlists them)
  closure_env$ConvertNestedlisttoDT <- function(input) {
    data.table::data.table(
      DocumentTitle = c("Apple", "Microsoft", "Google"),
      Tags = list(NULL, "", "tech")
    )
  }

  result <- conn$search(query = "test", top = 3)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  # NULLs and empty strings should be replaced with NA
  expect_true(is.na(result$Tags[1])) # was NULL
  expect_true(is.na(result$Tags[2])) # was ""
  expect_equal(result$Tags[3], "tech")
})


# ── #8: get_historical_pricing adjustments param (line 497) ──

test_that("RefinitivJsonConnect get_historical_pricing passes adjustments", {
  local_refinitiv_state()
  withr::local_options(eikon_port = 9000L, refinitiv_base_url = "http://lh")

  conn <- RefinitivJsonConnect(Eikonapplication_id = "test_key")

  captured_endpoint <- NULL
  closure_env <- environment(conn$get_historical_pricing)
  closure_env$send_json_request <- function(json = NULL, service = "eikon",
                                            debug = FALSE, request_type = "POST",
                                            EndPoint = NULL, ...) {
    captured_endpoint <<- EndPoint
    list(
      universe = "VOD.L",
      headers = list(list(title = "Date"), list(title = "BID")),
      data = list(list("2024-01-02", 100))
    )
  }

  conn$get_historical_pricing(
    EikonObject = conn,
    universe    = "VOD.L",
    interval    = "P1D",
    start       = "2024-01-01",
    end         = "2024-01-31",
    adjustments = c("exchangeCorrection", "CCH"),
    count       = 20L,
    fields      = NULL,
    sessions    = NULL
  )

  # adjustments should appear in the endpoint as comma-separated
  expect_true(grepl("adjustments=exchangeCorrection,CCH", captured_endpoint))
})


restore_refinitiv_state(.saved_state, "test-M6a-coverage-quickwins")
