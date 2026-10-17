# Regression tests for v0.2.3 Quantitative Developer Audit fixes

library(testthat)
library(mockery)
library(data.table)

.saved_state <- save_refinitiv_state()

# =============================================================================
# 1. P0 — rd_GetHistory() Data Integrity & Edge-Case Fixes
# =============================================================================

test_that("rd_GetHistory sets EDate when end is supplied and start is NULL", {
  local_refinitiv_state()
  captured_params <- NULL
  dummy_rd <- list()

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetData", function(RDObject, rics, Eikonformulas, Parameters, ...) {
    captured_params <<- Parameters
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-06-01"), `TR.Revenue` = 100)
  })

  res <- rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = "TR.Revenue",
    start = NULL,
    end = "2024-06-30",
    cache = FALSE
  )

  expect_false(is.null(captured_params))
  expect_equal(captured_params[["EDate"]], "2024-06-30")
  expect_false("SDate" %in% names(captured_params))
  expect_equal(nrow(res), 1L)
})

test_that("rd_GetHistory maps interval to parameters$FRQ for fundamental fields", {
  local_refinitiv_state()
  dummy_rd <- list()
  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)

  expected_map <- list(
    "daily"     = "D",
    "weekly"    = "W",
    "monthly"   = "M",
    "quarterly" = "CQ",
    "6M"        = "CS",
    "6m"        = "CS",
    "yearly"    = "CY"
  )

  for (iv in names(expected_map)) {
    captured_params <- NULL
    stub(rd_GetHistory, "rd_GetData", function(RDObject, rics, Eikonformulas, Parameters, ...) {
      captured_params <<- Parameters
      data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-01"), `TR.Revenue` = 100)
    })

    rd_GetHistory(
      RD = dummy_rd,
      universe = "AAPL.O",
      fields = "TR.Revenue",
      interval = iv,
      cache = FALSE
    )
    expect_equal(captured_params[["FRQ"]], expected_map[[iv]], info = paste("interval =", iv))
  }

  # Explicit FRQ in parameters is NOT overwritten
  captured_explicit <- NULL
  stub(rd_GetHistory, "rd_GetData", function(RDObject, rics, Eikonformulas, Parameters, ...) {
    captured_explicit <<- Parameters
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-01"), `TR.Revenue` = 100)
  })
  rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = "TR.Revenue",
    interval = "quarterly",
    parameters = list(FRQ = "FQ"),
    cache = FALSE
  )
  expect_equal(captured_explicit[["FRQ"]], "FQ")
})

test_that("rd_GetHistory forwards cache parameter to rd_GetData and rd_GetHistoricalPricing", {
  local_refinitiv_state()
  dummy_rd <- list()
  getdata_cache <- NULL
  hp_cache <- NULL

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetData", function(RDObject, rics, Eikonformulas, Parameters, cache, ...) {
    getdata_cache <<- cache
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-02"), `TR.Revenue` = 100)
  })
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(RDObject, universe, interval, start, end, adjustments, count, fields, cache, ...) {
    hp_cache <<- cache
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-02"), BID = 185.5)
  })

  rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = c("TR.Revenue", "BID"),
    cache = 120
  )

  expect_equal(getdata_cache, 120)
  expect_equal(hp_cache, 120)
})

test_that("rd_GetHistory handles 0-row GetDataOutput or HistoricalPricingOutput missing key columns", {
  local_refinitiv_state()
  dummy_rd <- list()
  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)

  # Case 1: GetDataOutput is empty data.table() without Instrument/Date, HistoricalPricingOutput has data
  stub(rd_GetHistory, "rd_GetData", function(...) data.table())
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) {
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-02"), BID = 185.5)
  })

  res1 <- rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = c("TR.Revenue", "BID"),
    cache = FALSE
  )
  expect_equal(nrow(res1), 1L)
  expect_equal(res1$BID, 185.5)

  # Case 2: Both return empty data.table() without columns
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) data.table())
  res2 <- rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = c("TR.Revenue", "BID"),
    cache = FALSE
  )
  expect_equal(nrow(res2), 0L)
  expect_true(all(c("Instrument", "Date") %in% names(res2)))
})

test_that("rd_GetHistory preserves POSIXct timestamps for intraday data and DATE_TIME column", {
  local_refinitiv_state()
  dummy_rd <- list()
  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)

  ts1 <- as.POSIXct("2024-01-15 14:30:00", tz = "UTC")
  ts2 <- as.POSIXct("2024-01-15 14:31:00", tz = "UTC")

  # HistoricalPricingOutput has DATE_TIME instead of Date, and GetDataOutput has Date (as.Date)
  stub(rd_GetHistory, "rd_GetData", function(...) {
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-15"), `TR.Revenue` = 100)
  })
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) {
    data.table(Instrument = c("AAPL.O", "AAPL.O"), DATE_TIME = c(ts1, ts2), BID = c(185.1, 185.2))
  })

  res <- suppressWarnings(rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = c("TR.Revenue", "BID"),
    interval = "1min",
    cache = FALSE
  ))

  expect_s3_class(res$Date, "POSIXct")
  expect_equal(attr(res$Date, "tzone"), "UTC")
  expect_true(any(res$Date == ts1))

  # Also test when GetDataOutput itself has POSIXct Date and HistoricalPricingOutput has Date
  stub(rd_GetHistory, "rd_GetData", function(...) {
    data.table(Instrument = "AAPL.O", Date = ts1, `TR.Revenue` = 100)
  })
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) {
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-15"), BID = 185.1)
  })
  res_b <- suppressWarnings(rd_GetHistory(
    RD = dummy_rd,
    universe = "AAPL.O",
    fields = c("TR.Revenue", "BID"),
    cache = FALSE
  ))
  expect_s3_class(res_b$Date, "POSIXct")
})


# =============================================================================
# 2. P0 — Session Cache Safety & Partial-Failure Protection
# =============================================================================

test_that("cache_set and cache_get clone data.table to prevent in-place mutation corruption", {
  local_refinitiv_state()
  suppressMessages(rd_ClearCache())

  dt_orig <- data.table(Instrument = c("AAPL.O", "MSFT.O"), Price = c(180.0, 400.0))
  key <- cache_lookup("test_fn", "AAPL.O")$key
  cache_set(key, dt_orig, ttl = 300)

  # Mutating dt_orig after cache_set must not affect cached value
  dt_orig[, Price := c(0.0, 0.0)]
  hit1 <- cache_get(key)
  expect_equal(hit1$value$Price, c(180.0, 400.0))

  # Mutating returned data.table from cache_get must not affect subsequent cache_get
  hit1$value[, Price := c(-999.0, -999.0)]
  setnames(hit1$value, "Price", "MutatedPrice")

  hit2 <- cache_get(key)
  expect_true("Price" %in% names(hit2$value))
  expect_equal(hit2$value$Price, c(180.0, 400.0))

  # Also test EikonResult list containing a data.table inside
  er <- new_EikonResult(list(
    PostProcessedEikonGetData = data.table(Instrument = "AAPL.O", BID = 100),
    Eikon_Error_Data = data.table()
  ))
  key_er <- cache_lookup("test_er", "AAPL.O")$key
  cache_set(key_er, er, ttl = 300)
  er_hit <- cache_get(key_er)$value
  er_hit$PostProcessedEikonGetData[, BID := 999]
  expect_equal(cache_get(key_er)$value$PostProcessedEikonGetData$BID, 100)
})

test_that("rd_GetData, rd_GetHistoricalPricing, EikonGetData, EikonGetTimeseries, and EikonGetSymbology do not cache partial chunk failures", {
  local_refinitiv_state()
  rd_ClearCache()

  # 1. rd_GetData with partial chunk failure
  stub(rd_GetData, "CheckifCustomInstrument", function(...) FALSE)
  stub(rd_GetData, "EikonChunker", function(...) list("AAPL.O", "MSFT.O"))
  stub(rd_GetData, "chunked_download", function(...) {
    structure(
      list(list(headers = list(), data = list()), NA),
      chunk_Attempt = c(1L, 4L)
    )
  })
  stub(rd_GetData, "rd_OutputProcesser", function(...) {
    data.table(Instrument = "AAPL.O", BID = 180)
  })

  res_gd <- rd_GetData(RDObject = list(), rics = c("AAPL.O", "MSFT.O"), Eikonformulas = "BID", cache = TRUE)
  expect_equal(suppressMessages(rd_CacheInfo())$total_keys, 0L)

  # 2. rd_GetHistoricalPricing with partial chunk failure
  stub(rd_GetHistoricalPricing, "CheckifCustomInstrument", function(...) FALSE)
  stub(rd_GetHistoricalPricing, "chunked_download", function(...) {
    structure(list(list(ok = TRUE), NA), chunk_Attempt = c(1L, 4L))
  })
  stub(rd_GetHistoricalPricing, "rd_OutputProcesser", function(...) {
    data.table(Instrument = "AAPL.O", Date = as.Date("2024-01-01"), BID = 180)
  })

  res_hp <- rd_GetHistoricalPricing(RDObject = list(), universe = c("AAPL.O", "MSFT.O"), cache = TRUE)
  expect_equal(suppressMessages(rd_CacheInfo())$total_keys, 0L)

  # 3. EikonGetData with partial chunk failure
  stub(EikonGetData, "EikonChunker", function(...) list("AAPL.O", "MSFT.O"))
  stub(EikonGetData, "chunked_download", function(...) {
    structure(list(list(ok = TRUE), NA), chunk_Attempt = c(1L, 4L))
  })
  stub(EikonGetData, "EikonPostProcessor", function(...) {
    list(PostProcessedEikonGetData = data.frame(Instrument = "AAPL.O"), Eikon_Error_Data = data.frame())
  })

  res_egd <- EikonGetData(EikonObject = list(), rics = c("AAPL.O", "MSFT.O"), Eikonformulas = "BID", cache = TRUE)
  expect_equal(suppressMessages(rd_CacheInfo())$total_keys, 0L)

  # 4. EikonGetTimeseries with partial chunk failure
  stub(EikonGetTimeseries, "EikonChunker", function(...) list("AAPL.O", "MSFT.O"))
  stub(EikonGetTimeseries, "chunked_download", function(...) {
    structure(list(list(ok = TRUE), NA), chunk_Attempt = c(1L, 4L))
  })
  stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(...) {
    data.frame(Date = as.Date("2024-01-01"), Security = "AAPL.O", CLOSE = 180)
  })

  res_ets <- EikonGetTimeseries(EikonObject = list(), rics = c("AAPL.O", "MSFT.O"), cache = TRUE)
  expect_equal(suppressMessages(rd_CacheInfo())$total_keys, 0L)

  # 5. EikonGetSymbology with partial chunk failure
  stub(EikonGetSymbology, "EikonChunker", function(...) list("AAPL.O", "MSFT.O"))
  stub(EikonGetSymbology, "chunked_download", function(...) {
    structure(list(list(mappedSymbols = list()), NA), chunk_Attempt = c(1L, 4L))
  })
  stub(EikonGetSymbology, "ProcessSymbology", function(...) {
    data.table(symbol = "AAPL.O", ISIN = "US0378331005")
  })

  res_sym <- suppressWarnings(EikonGetSymbology(EikonObject = list(), symbol = c("AAPL.O", "MSFT.O"), cache = TRUE))
  expect_equal(suppressMessages(rd_CacheInfo())$total_keys, 0L)
})

test_that("chunk_cache_key_fn canonicalizes reordered fields and symbols in rd_GetData, EikonGetData, EikonGetSymbology", {
  local_refinitiv_state()
  rd_ClearCache()

  # Capture chunk_cache_key_fn passed to chunked_download in rd_GetData
  captured_key_fn <- NULL
  stub(rd_GetData, "CheckifCustomInstrument", function(...) FALSE)
  stub(rd_GetData, "EikonChunker", function(RICS, ...) list(RICS))
  stub(rd_GetData, "chunked_download", function(n_chunks, fetch_fn, ..., chunk_cache_key_fn) {
    captured_key_fn <<- chunk_cache_key_fn
    list(list(headers = list(), data = list()))
  })
  stub(rd_GetData, "rd_OutputProcesser", function(...) data.table(Instrument = "AAPL.O"))

  rd_GetData(
    RDObject = list(),
    rics = c("MSFT.O", "AAPL.O"),
    Eikonformulas = c("ASK", "BID"),
    Parameters = list(EDate = "2024-01-10", SDate = "2024-01-01"),
    cache = TRUE
  )
  k1 <- captured_key_fn(1L)

  rd_ClearCache()
  rd_GetData(
    RDObject = list(),
    rics = c("AAPL.O", "MSFT.O"),
    Eikonformulas = c("BID", "ASK"),
    Parameters = list(SDate = "2024-01-01", EDate = "2024-01-10"),
    cache = TRUE
  )
  k2 <- captured_key_fn(1L)
  expect_equal(k1, k2)

  # Same for EikonGetData
  captured_egd_key_fn <- NULL
  stub(EikonGetData, "EikonChunker", function(RICS, ...) list(RICS))
  stub(EikonGetData, "chunked_download", function(n_chunks, fetch_fn, ..., chunk_cache_key_fn) {
    captured_egd_key_fn <<- chunk_cache_key_fn
    list(list())
  })
  stub(EikonGetData, "EikonPostProcessor", function(...) {
    list(PostProcessedEikonGetData = data.frame(), Eikon_Error_Data = data.frame())
  })
  rd_ClearCache()
  EikonGetData(EikonObject = list(), rics = c("B", "A"), Eikonformulas = c("F2", "F1"), cache = TRUE)
  ek1 <- captured_egd_key_fn(1L)
  rd_ClearCache()
  EikonGetData(EikonObject = list(), rics = c("A", "B"), Eikonformulas = c("F1", "F2"), cache = TRUE)
  ek2 <- captured_egd_key_fn(1L)
  expect_equal(ek1, ek2)

  # Same for EikonGetSymbology
  captured_sym_key_fn <- NULL
  stub(EikonGetSymbology, "EikonChunker", function(RICS, ...) list(RICS))
  stub(EikonGetSymbology, "chunked_download", function(n_chunks, fetch_fn, ..., chunk_cache_key_fn) {
    captured_sym_key_fn <<- chunk_cache_key_fn
    list(list(mappedSymbols = list()))
  })
  stub(EikonGetSymbology, "ProcessSymbology", function(...) data.table(symbol = "A"))
  rd_ClearCache()
  suppressWarnings(EikonGetSymbology(EikonObject = list(), symbol = c("B", "A"), to_symbol_type = c("RIC", "ISIN"), cache = TRUE))
  sk1 <- captured_sym_key_fn(1L)
  rd_ClearCache()
  suppressWarnings(EikonGetSymbology(EikonObject = list(), symbol = c("A", "B"), to_symbol_type = c("ISIN", "RIC"), cache = TRUE))
  sk2 <- captured_sym_key_fn(1L)
  expect_equal(sk1, sk2)
})


# =============================================================================
# 3. P0 — Multi-Period Chunking & 10,000-Row Server Truncation Guard
# =============================================================================

test_that("EikonChunker supports simultaneous Eikonfields and Duration and multi-period Parameters", {
  rics <- paste0("RIC", seq_len(200))
  fields <- paste0("F", seq_len(10))

  # 1. Both Eikonfields (10) and Duration (100): total data points = 200 * 10 * 100 = 200,000
  # Total rows = 200 * 100 = 20,000. Needs at least ceiling(200000 / 12000) = 17 chunks
  chunks <- EikonChunker(
    RICS = rics,
    Eikonfields = fields,
    Duration = 100,
    MaxCallsPerChunk = 12000,
    MaxRowsPerChunk = 10000L,
    verbose = FALSE
  )
  expect_gte(length(chunks), 17L)
  expect_equal(sum(lengths(chunks)), 200L)

  # 2. Multi-period Parameters with relative offsets (SDate = 0, EDate = -99 -> 100 rows per RIC)
  # 200 RICs * 100 rows = 20,000 rows > MaxRowsPerChunk (10,000) -> splits into >= 2 chunks even with 1 field
  chunks_rel <- EikonChunker(
    RICS = rics,
    Eikonfields = "TR.PriceClose",
    Parameters = list(SDate = 0, EDate = -99),
    MaxCallsPerChunk = 12000,
    MaxRowsPerChunk = 10000L,
    verbose = FALSE
  )
  expect_gte(length(chunks_rel), 2L)

  # 3. Multi-period Parameters with ISO dates and different frequencies (D, W, M, Q, S, Y, unknown)
  for (frq in c("D", "W", "M", "Q", "S", "Y", "OTHER")) {
    est <- .estimate_rows_per_ric(list(SDate = "2020-01-01", EDate = "2024-01-01", Frq = frq))
    expect_gte(est, 1L)
  }

  # 4. Edge cases in .estimate_rows_per_ric
  expect_equal(.estimate_rows_per_ric(NULL), 1L)
  expect_equal(.estimate_rows_per_ric(list()), 1L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "2024-01-01")), 1L)
  expect_equal(.estimate_rows_per_ric(list(SDate = NULL, EDate = NULL)), 1L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "0D", EDate = "-10Y", FRQ = "CY")), 11L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "0D", EDate = "-4Q", FRQ = "CQ")), 5L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "0D", EDate = "-6M", FRQ = "M")), 7L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "0D", EDate = "-2W", FRQ = "W")), 3L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "0D", EDate = "-2S", FRQ = "CS")), 3L)
  expect_equal(.estimate_rows_per_ric(list(SDate = "not_a_date", EDate = "also_bad")), 1L)
})

test_that("rd_OutputProcesser and EikonPostProcessor warn when a chunk reaches 10,000 rows", {
  # 1. rd_OutputProcesser 10,000-row truncation warning (single chunk x)
  rows_10k <- replicate(10000L, list("AAPL.O", 180.0), simplify = FALSE)
  mock_rd_10k <- list(
    headers = list(
      list(name = "instrument", title = "Instrument", type = "string", decimalChar = "."),
      list(name = "BID", title = "BID", type = "number", decimalChar = ".")
    ),
    data = rows_10k
  )

  expect_warning(
    res_rd <- rd_OutputProcesser(mock_rd_10k, use_field_names_in_headers = FALSE),
    "10,000-row server limit"
  )
  expect_equal(nrow(res_rd), 10000L)

  # 2. EikonPostProcessor 10,000-row truncation warning
  mock_eikon_10k <- list(list(
    columnHeadersCount = 1L,
    data = rows_10k,
    headerOrientation = "horizontal",
    headers = list(list(
      list(displayName = "Instrument"),
      list(displayName = "BID", field = "BID")
    )),
    rowHeadersCount = 1L,
    totalColumnsCount = 2L,
    totalRowsCount = 10000L
  ))

  expect_warning(
    res_eik <- EikonPostProcessor(mock_eikon_10k),
    "10,000-row server limit"
  )
  expect_equal(nrow(res_eik$PostProcessedEikonGetData), 10000L)
})


# =============================================================================
# 4. P1 — Intraday Timestamp Fidelity & "6M" Interval Handling
# =============================================================================

test_that("rd_GetHistoricalPricing rejects 6M interval cleanly instead of coercing to P3M", {
  expect_error(
    rd_GetHistoricalPricing(RDObject = list(), universe = "AAPL.O", interval = "6M"),
    "Interval is 6M but can only be one of"
  )
})

test_that("rd_OutputProcesser preserves intraday POSIXct timestamps and keeps daily dates as Date", {
  # Intraday timestamps -> POSIXct (UTC)
  mock_intraday <- list(
    universe = "AAPL.O",
    headers = list(
      list(name = "DATE", type = "string", decimalChar = NA),
      list(name = "BID", type = "number", decimalChar = ".")
    ),
    data = list(
      list("2024-01-15T14:30:00Z", 185.1),
      list("2024-01-15T14:35:00.500Z", 185.2),
      list("2024-01-15T00:00:00Z", 185.0)
    )
  )

  res_intra <- rd_OutputProcesser(mock_intraday)
  expect_s3_class(res_intra$Date, "POSIXct")
  expect_equal(attr(res_intra$Date, "tzone"), "UTC")
  expect_equal(format(res_intra$Date[1], "%H:%M:%S", tz = "UTC"), "14:30:00")

  # Daily-only midnight timestamps -> Date
  mock_daily <- list(
    universe = "AAPL.O",
    headers = list(
      list(name = "DATE", type = "string", decimalChar = NA),
      list(name = "BID", type = "number", decimalChar = ".")
    ),
    data = list(
      list("2024-01-15T00:00:00Z", 185.1),
      list("2024-01-16", 186.0)
    )
  )

  res_daily <- rd_OutputProcesser(mock_daily)
  expect_s3_class(res_daily$Date, "Date")
})

test_that("PostProcessTimeSeriesRequest preserves POSIXct for sub-second and Z-less intraday timestamps", {
  raw_intra <- list(list(
    timeseriesData = list(list(
      dataPoints = list(
        list("2020-01-15T14:30:00.500", 100.1, 101.0, 99.5, 100.5, 500L),
        list("2020-01-15T14:31:00Z", 100.5, 101.2, 100.0, 101.0, 600L)
      ),
      fields = list(
        list(name = "TIMESTAMP", type = "DateTime"),
        list(name = "OPEN", type = "Double"),
        list(name = "HIGH", type = "Double"),
        list(name = "LOW", type = "Double"),
        list(name = "CLOSE", type = "Double"),
        list(name = "VOLUME", type = "Integer")
      ),
      ric = "AAPL.O",
      statusCode = "Normal"
    ))
  ))

  res <- PostProcessTimeSeriesRequest(raw_intra)
  expect_s3_class(res$Date, "POSIXct")
  expect_equal(attr(res$Date, "tzone"), "UTC")
  expect_equal(format(res$Date, "%H:%M:%S", tz = "UTC"), c("14:30:00", "14:31:00"))
})


# =============================================================================
# 5. P1 — WebSocket Streaming Resilience & Custom API Key Propagation
# =============================================================================

test_that("Stream$close() unregisters reconnect callbacks and prevents zombie callback invocation", {
  manager <- StreamManager$new()
  def <- StreamDefinition$new(universe = "EUR=", fields = c("BID", "ASK"))
  stream <- Stream$new(definition = def, manager = manager)

  reconn_called <- 0L
  reconnected_called <- 0L
  stream$on_reconnecting(function(s, attempt) reconn_called <<- reconn_called + 1L)
  stream$on_reconnected(function(s, attempt) reconnected_called <<- reconnected_called + 1L)

  priv_mgr <- manager$.__enclos_env__$private
  expect_length(priv_mgr$.reconnect_callbacks$on_reconnecting, 1L)
  expect_length(priv_mgr$.reconnect_callbacks$on_reconnected, 1L)

  # Even before stream$close() is called, if stream is not open, the callback wrapper guards against firing
  priv_mgr$.fire_reconnect_callbacks("on_reconnecting", 1L)
  priv_mgr$.fire_reconnect_callbacks("on_reconnected", 1L)
  expect_equal(reconn_called, 0L)
  expect_equal(reconnected_called, 0L)

  # Closing the stream unregisters its callbacks from StreamManager
  stream$close()
  expect_length(priv_mgr$.reconnect_callbacks$on_reconnecting, 0L)
  expect_length(priv_mgr$.reconnect_callbacks$on_reconnected, 0L)
})

test_that("StreamManager$.attempt_reconnect passes vaulted token to rd_VerifyToken and forwards base_url", {
  local_refinitiv_state()
  refinitiv_vault_set("access_token", "valid_jwt_token")

  manager <- StreamManager$new(base_url = "http://10.20.30.40", port = 9000L, stream_type = "pricing")
  priv <- manager$.__enclos_env__$private
  priv$.user_initiated_close <- FALSE

  queued <- list()
  stub(priv$.attempt_reconnect, "later::later", function(func, delay) {
    queued[[length(queued) + 1L]] <<- list(fn = func, delay = delay)
  })
  stub(priv$.attempt_reconnect, "requireNamespace", function(pkg, quietly) TRUE)

  priv$.attempt_reconnect(debug = FALSE)
  expect_length(queued, 1L)

  verified_token <- NULL
  handshake_called <- FALSE
  captured_url <- NULL

  stub(queued[[1]]$fn, "rd_VerifyToken", function(token) {
    verified_token <<- token
    TRUE
  })
  stub(queued[[1]]$fn, "rd_handshake", function(...) {
    handshake_called <<- TRUE
    list(access_token = "new_tok")
  })
  mock_ws <- list(
    onError = function(h) NULL,
    onMessage = function(h) NULL,
    onOpen = function(h) NULL,
    onClose = function(h) NULL,
    connect = function() NULL
  )
  stub(queued[[1]]$fn, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) {
    captured_url <<- url
    mock_ws
  })

  queued[[1]]$fn()

  expect_equal(verified_token, "valid_jwt_token")
  expect_false(handshake_called)
  expect_match(captured_url, "^ws://10\\.20\\.30\\.40:9000/")
})

test_that("StreamManager$connect forwards custom base_url to get_streaming_url", {
  local_refinitiv_state()
  manager <- StreamManager$new(base_url = "http://10.0.0.99", port = 9060L, stream_type = "pricing")
  priv <- manager$.__enclos_env__$private

  captured_url <- NULL
  mock_ws <- list(
    onError = function(h) NULL,
    onMessage = function(h) NULL,
    onOpen = function(h) NULL,
    onClose = function(h) NULL,
    connect = function() NULL,
    send = function(msg) NULL,
    readyState = function() 1L
  )

  stub(manager$connect, "rd_handshake", function() list(access_token = "tok"))
  stub(manager$connect, "poll_until_connected", function(ws, timeout) {
    priv$.logged_in <- TRUE
    TRUE
  })
  stub(manager$connect, "websocket::WebSocket$new", function(url, protocols, headers, autoConnect) {
    captured_url <<- url
    mock_ws
  })
  stub(manager$connect, "requireNamespace", function(pkg, quietly) TRUE)

  manager$connect(debug = FALSE)
  expect_equal(captured_url, "ws://10.0.0.99:9060/api/rdp/streaming/pricing/v1/WebSocket")
})

test_that("StreamManager$.handle_refresh passes actual attempt count to on_reconnected callbacks before resetting", {
  manager <- StreamManager$new()
  priv <- manager$.__enclos_env__$private
  priv$.reconnecting <- TRUE
  priv$.reconnect_attempt <- 3L

  observed_attempt <- NULL
  manager$register_reconnect_callback("on_reconnected", function(attempt) {
    observed_attempt <<- attempt
  })

  stub(priv$.handle_refresh, "later::run_now", function(timeout) NULL)
  msg <- list(Type = "Refresh", Domain = "Login", ID = 1L, State = list(Stream = "Open", Data = "Ok"))
  priv$.handle_refresh(msg, debug = FALSE)

  expect_equal(observed_attempt, 3L)
  expect_equal(priv$.reconnect_attempt, 0L)
  expect_false(priv$.reconnecting)
})

test_that("Stream handles OMM State$Data == 'Suspect', State$Stream == 'Closed', and State$Text error fields", {
  manager <- StreamManager$new()
  def <- StreamDefinition$new(universe = "INVALID.RIC", fields = c("BID"))
  stream <- Stream$new(definition = def, manager = manager)
  priv <- stream$.__enclos_env__$private
  priv$.is_open <- TRUE

  errors_seen <- character(0)
  stream$on_error(function(s, err_msg) {
    errors_seen <<- c(errors_seen, err_msg)
  })

  status_msg <- list(
    Type = "Status",
    ID = 2L,
    Key = list(Name = "INVALID.RIC"),
    State = list(Stream = "Closed", Data = "Suspect", Code = "NotFound", Text = "The record could not be found")
  )

  priv$.handle_message(status_msg, "STATUS")
  expect_false(stream$is_open())
  expect_equal(errors_seen, "The record could not be found")
})

test_that("create_omm_login_request, create_streaming_headers, and rd_handshake use custom vaulted api_key", {
  local_refinitiv_state()
  refinitiv_vault_set("api_key", "MY_CUSTOM_APP_KEY")

  login_json <- create_omm_login_request(access_token = "tok123")
  parsed_login <- jsonlite::fromJSON(login_json)
  expect_equal(parsed_login$Key$Elements$AppKey, "MY_CUSTOM_APP_KEY")

  hdrs <- create_streaming_headers("tok123")
  expect_equal(hdrs[["x-tr-applicationid"]], "MY_CUSTOM_APP_KEY")

  # rd_handshake passes custom api_key in both payload$AppKey and send_json_request(apikey = ...)
  captured_apikey <- NULL
  captured_payload <- NULL
  withr::local_options(refinitiv_base_url = "http://lh", eikon_port = 9000L)
  stub(rd_handshake, "CheckTerminalType", function(...) NULL)
  stub(rd_handshake, "send_json_request", function(json, request_type, debug, apikey, url) {
    captured_payload <<- json
    captured_apikey <<- apikey
    list(access_token = "new_jwt", expires_in = 3600, token_type = "bearer")
  })

  rd_handshake(force = TRUE, debug = FALSE)
  expect_equal(captured_payload$AppKey, "MY_CUSTOM_APP_KEY")
  expect_equal(captured_apikey, "MY_CUSTOM_APP_KEY")
})


# =============================================================================
# 6 & 7. P2 — Parser Hardening, ADC Diagnostics & Pagination
# =============================================================================

test_that("JsonHeaderAnalyzer falls back to headers[['name']] when schema keys differ", {
  mock_json <- list(
    headers = list(
      list(name = "Instrument", type = "string"),
      list(name = "TR.Revenue", type = "number")
    )
  )
  res <- JsonHeaderAnalyzer(mock_json, Selectedheader = "name", use_field_names_in_headers = FALSE)
  expect_equal(res, c("Instrument", "TR.Revenue"))
})

test_that("ProcessSymbology handles first symbol having empty bestMatch when later symbol succeeds", {
  mock_sym <- list(
    mappedSymbols = list(
      list(symbol = "BAD_SYM", bestMatch = list(error = "No match")),
      list(symbol = "AAPL.O", bestMatch = list(ISIN = "US0378331005"))
    )
  )
  res <- ProcessSymbology(list(mock_sym), from_symbol_type = "RIC", to_symbol_type = "ISIN")
  expect_equal(nrow(res), 2L)
  expect_true("ISIN" %in% names(res))
  expect_equal(res$ISIN[res$RIC == "AAPL.O"], "US0378331005")

  # Also test when ALL symbols have empty bestMatch (falls back to to_symbol_type[1L])
  mock_all_bad <- list(
    mappedSymbols = list(
      list(symbol = "BAD1", bestMatch = list()),
      list(symbol = "BAD2", bestMatch = list())
    )
  )
  res_bad <- ProcessSymbology(list(mock_all_bad), from_symbol_type = "RIC", to_symbol_type = "ISIN")
  expect_equal(nrow(res_bad), 2L)
  expect_true("ISIN" %in% names(res_bad))
})

test_that("process_adc_response warns when x$universe contains unresolved identifier messages", {
  mock_adc_unresolved <- list(
    headers = list(
      list(name = "instrument", title = "Instrument", type = "String"),
      list(name = "score", title = "ESG Score", type = "Double")
    ),
    data = list(
      list("AAPL.O", 82.5)
    ),
    universe = list(
      list(Instrument = "AAPL.O", `Company Common Name` = "Apple Inc"),
      list(Instrument = "INVALID_RIC", Message = "Failed to resolve identifier(s).")
    )
  )

  expect_warning(
    df <- process_adc_response(mock_adc_unresolved),
    "Unresolved identifier\\(s\\) in ADC response: INVALID_RIC \\(Failed to resolve identifier\\(s\\)\\.\\)"
  )
  expect_equal(nrow(df), 1L)
})

test_that("rd_GetOwnership supports offset parameter and advances offset by actual rows returned", {
  local_refinitiv_state()
  offsets_seen <- integer(0)
  limits_seen <- integer(0)

  dummy_rd <- list(
    get_ownership = function(universe, view, statType, limit, offset, ...) {
      offsets_seen <<- c(offsets_seen, if (is.null(offset)) 0L else offset)
      limits_seen <<- c(limits_seen, limit)
      n <- min(limit, 100L)
      rows <- lapply(seq_len(n), function(i) list("AAPL.O", paste0("Inv", i)))
      list(
        headers = list(
          list(name = "instrument", title = "Instrument", type = "String"),
          list(name = "investor", title = "Investor", type = "String")
        ),
        data = rows
      )
    }
  )

  stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  # Paginated request starting at offset = 50 with limit = 150
  res <- rd_GetOwnership(
    RDObject = dummy_rd,
    universe = "AAPL.O",
    view = "consolidated/investors",
    limit = 150,
    offset = 50,
    cache = FALSE
  )
  expect_equal(nrow(res), 150L)
  expect_equal(offsets_seen, c(50L, 150L))
  expect_equal(limits_seen, c(100L, 50L))

  # Single-page request with offset = 25 and limit = 40
  offsets_seen <- integer(0)
  res_single <- rd_GetOwnership(
    RDObject = dummy_rd,
    universe = "AAPL.O",
    view = "consolidated/investors",
    limit = 40,
    offset = 25,
    cache = FALSE
  )
  expect_equal(nrow(res_single), 40L)
  expect_equal(offsets_seen, 25L)
})

test_that("CorrectCustomInstrument is vectorized over symbol vectors of length > 1", {
  res <- CorrectCustomInstrument(
    symbol = c("test1", "S)test2", "test3.ABCDE-123456", "S)test4.ABCDE-123456"),
    UUID = "ABCDE-123456"
  )
  expect_equal(
    res,
    c(
      "S)test1.ABCDE-123456",
      "S)test2.ABCDE-123456",
      "S)test3.ABCDE-123456",
      "S)test4.ABCDE-123456"
    )
  )
})

restore_refinitiv_state(.saved_state, "test-v023-audit-fixes")
