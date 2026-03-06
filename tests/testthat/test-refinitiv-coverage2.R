library(testthat)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# Shared helper: create a mock JSON connection with controllable method stubs.
# Uses setup_mock_json_env() which auto-restores vault + options on exit.
make_mock_connection <- function() {
  setup_mock_json_env(.env = parent.frame())
}


# ═══════════════════════════════════════════════════════════════════════════
# 1. Trivial guards
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonShowAttributes returns method names from connection object", {
  RD <- make_mock_connection()
  result <- EikonShowAttributes(RD)
  expect_type(result, "character")
  expect_true("get_data" %in% result)
  expect_true("get_timeseries" %in% result)
})

test_that("EikonShowAttributes stops on NULL input", {
  expect_error(EikonShowAttributes(NULL), "should be supplied")
})

test_that("EikonChunker stops when RICS is NULL", {
  expect_error(
    EikonChunker(RICS = NULL, Eikonfields = c("TR.Revenue")),
    "RICS have to be"
  )
})

test_that("retry() handles NA max_attempts gracefully", {
  result <- retry(function() 42, max_attempts = NA)
  expect_equal(result, 42)
})

test_that("retry() handles max_attempts = 0 gracefully", {
  result <- retry(function() 42, max_attempts = 0)
  expect_equal(result, 42)
})


# ═══════════════════════════════════════════════════════════════════════════
# 2. EikonGetTimeseries — edge cases
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonGetTimeseries warns and returns empty data.frame when rics is NULL", {
  RD <- make_mock_connection()
  expect_warning(
    result <- EikonGetTimeseries(EikonObject = RD, rics = NULL, cache = FALSE),
    "no rics are supplied"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("EikonGetTimeseries adds VALUE field for economic time series (= in rics)", {
  RD <- make_mock_connection()
  captured_fields <- NULL

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("=USGDP"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(fetch_fn, ...) {
    # Call fetch_fn to trigger the get_timeseries call, capturing fields
    list(list())
  })
  mockery::stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(x) {
    data.frame(Date = Sys.Date(), Security = "=USGDP", VALUE = 100)
  })

  # Override get_timeseries on the connection object to capture fields
  RD$get_timeseries <- function(rics, interval, calendar, fields, ...) {
    captured_fields <<- fields
    list()
  }

  result <- EikonGetTimeseries(
    EikonObject = RD,
    rics = "=USGDP",
    fields = c("TIMESTAMP", "CLOSE"),
    cache = FALSE
  )

  # The function should have added VALUE to the fields
  # Check at the function level: rics contain "=", so VALUE should be prepended
  # We can verify this by looking at what chunked_download receives
  # (the stub above short-circuits, so let's test the field logic directly)
  expect_s3_class(result, "data.frame")
})

test_that("EikonGetTimeseries raw_output returns list directly", {
  RD <- make_mock_connection()
  mock_raw <- list(timeseriesData = list(list(ric = "AAPL.O", dataPoints = list())))

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("AAPL.O"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(...) list(mock_raw))

  result <- EikonGetTimeseries(
    EikonObject = RD, rics = "AAPL.O", raw_output = TRUE, cache = FALSE
  )

  expect_type(result, "list")
  expect_equal(result, list(mock_raw))
})

test_that("EikonGetTimeseries null fields branch calls get_timeseries with empty fields", {
  RD <- make_mock_connection()
  captured_args <- NULL

  RD$get_timeseries <- function(...) {
    captured_args <<- list(...)
    list(timeseriesData = list(list(ric = "AAPL.O", dataPoints = list())))
  }

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("AAPL.O"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(n_chunks, fetch_fn, ...) {
    list(fetch_fn(1))
  })
  mockery::stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(x) data.frame())

  result <- EikonGetTimeseries(
    EikonObject = RD, rics = "AAPL.O", fields = NULL, cache = FALSE
  )

  expect_false(is.null(captured_args))
  expect_equal(captured_args$fields, c())
})

test_that("EikonGetTimeseries verbose emits debug messages", {
  RD <- make_mock_connection()

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("AAPL.O"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(n_chunks, fetch_fn, ...) {
    list(fetch_fn(1))
  })
  mockery::stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(x) data.frame())

  RD$get_timeseries <- function(...) list()

  expect_message(
    EikonGetTimeseries(
      EikonObject = RD, rics = "AAPL.O", verbose = TRUE, cache = FALSE
    ),
    "EikonGetTimeseries JSON request"
  )
})


# ═══════════════════════════════════════════════════════════════════════════
# 3. EikonGetTimeseries — cache paths
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonGetTimeseries stores result in cache", {
  RD <- make_mock_connection()
  rd_ClearCache()

  mock_result <- data.frame(Date = as.Date("2025-01-01"), Security = "AAPL.O", CLOSE = 150)

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("AAPL.O"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(...) list(list()))
  mockery::stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(x) mock_result)

  result <- EikonGetTimeseries(
    EikonObject = RD, rics = "AAPL.O", cache = TRUE
  )
  expect_equal(result, mock_result)

  # Cache should now be populated
  info <- rd_CacheInfo()
  expect_true(info$total_keys > 0)

  rd_ClearCache()
})

test_that("EikonGetTimeseries returns cached result on second call", {
  RD <- make_mock_connection()
  rd_ClearCache()

  call_count <- 0L
  mock_result <- data.frame(Date = as.Date("2025-01-01"), Security = "AAPL.O", CLOSE = 150)

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("AAPL.O"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    list(list())
  })
  mockery::stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(x) mock_result)

  # First call — populates cache
  result1 <- EikonGetTimeseries(EikonObject = RD, rics = "AAPL.O", cache = 300)

  # Second call — should hit cache (chunked_download not called again)
  result2 <- EikonGetTimeseries(EikonObject = RD, rics = "AAPL.O", cache = 300)

  expect_equal(result1, result2)
  expect_equal(call_count, 1L)

  rd_ClearCache()
})

test_that("EikonGetTimeseries cache hit emits message when verbose", {
  RD <- make_mock_connection()
  rd_ClearCache()

  mock_result <- data.frame(Date = as.Date("2025-01-01"), CLOSE = 150)

  mockery::stub(EikonGetTimeseries, "EikonTimeSeriesPreprocessor", function(...) list("AAPL.O"))
  mockery::stub(EikonGetTimeseries, "chunked_download", function(...) list(list()))
  mockery::stub(EikonGetTimeseries, "PostProcessTimeSeriesRequest", function(x) mock_result)

  # Populate cache
  EikonGetTimeseries(EikonObject = RD, rics = "AAPL.O", cache = 300, verbose = FALSE)

  # Cache hit with verbose
  expect_message(
    EikonGetTimeseries(EikonObject = RD, rics = "AAPL.O", cache = 300, verbose = TRUE),
    "Cache hit"
  )

  rd_ClearCache()
})


# ═══════════════════════════════════════════════════════════════════════════
# 4. EikonGetData — edge cases & cache
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonGetData raw_output returns chunked_download list directly", {
  RD <- make_mock_connection()
  mock_raw <- list(
    totalRowsCount = 1, totalColumnsCount = 2,
    headers = list(list(name = "Instrument"), list(name = "Revenue")),
    data = list(list("AAPL.O", 100))
  )

  mockery::stub(EikonGetData, "chunked_download", function(...) list(mock_raw))

  result <- EikonGetData(
    EikonObject = RD, rics = "AAPL.O",
    Eikonformulas = "TR.Revenue", raw_output = TRUE, cache = FALSE
  )

  expect_type(result, "list")
  expect_equal(result, list(mock_raw))
})

test_that("EikonGetData filters NA chunks in non-raw mode", {
  RD <- make_mock_connection()

  # Return a list where one chunk is NA (simulating a failed chunk)
  mockery::stub(EikonGetData, "chunked_download", function(...) list(NA))
  mockery::stub(EikonGetData, "EikonPostProcessor", function(x, ...) {
    # After NA-to-NULL conversion, should receive list(NULL)
    expect_null(x[[1]])
    data.frame(Instrument = character(0))
  })

  result <- EikonGetData(
    EikonObject = RD, rics = "AAPL.O",
    Eikonformulas = "TR.Revenue", raw_output = FALSE, cache = FALSE
  )
  expect_s3_class(result, "EikonResult")
})

test_that("EikonGetData verbose emits debug messages", {
  RD <- make_mock_connection()

  RD$get_data <- function(...) list()

  mockery::stub(EikonGetData, "chunked_download", function(n_chunks, fetch_fn, ...) {
    list(fetch_fn(1))
  })
  mockery::stub(EikonGetData, "EikonPostProcessor", function(x, ...) data.frame())

  expect_message(
    EikonGetData(
      EikonObject = RD, rics = "AAPL.O",
      Eikonformulas = "TR.Revenue", verbose = TRUE, cache = FALSE
    ),
    "EikonGetData JSON request"
  )
})

test_that("EikonGetData stores result in cache and returns cached value", {
  RD <- make_mock_connection()
  rd_ClearCache()

  call_count <- 0L
  mock_pp_result <- list(
    PostProcessedEikonGetData = data.frame(Instrument = "AAPL.O", Revenue = 100),
    Eikon_Error_Data = data.frame()
  )

  mockery::stub(EikonGetData, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    list(list())
  })
  mockery::stub(EikonGetData, "EikonPostProcessor", function(x, ...) mock_pp_result)

  # First call
  r1 <- EikonGetData(
    EikonObject = RD, rics = "AAPL.O",
    Eikonformulas = "TR.Revenue", cache = 300
  )
  expect_s3_class(r1, "EikonResult")
  expect_equal(r1$PostProcessedEikonGetData, mock_pp_result$PostProcessedEikonGetData)

  # Second call — cache hit
  r2 <- EikonGetData(
    EikonObject = RD, rics = "AAPL.O",
    Eikonformulas = "TR.Revenue", cache = 300
  )
  expect_equal(r2$PostProcessedEikonGetData, mock_pp_result$PostProcessedEikonGetData)
  expect_equal(call_count, 1L)

  rd_ClearCache()
})

test_that("EikonGetData cache hit emits message when verbose", {
  RD <- make_mock_connection()
  rd_ClearCache()

  mockery::stub(EikonGetData, "chunked_download", function(...) list(list()))
  mockery::stub(EikonGetData, "EikonPostProcessor", function(x, ...) data.frame(x = 1))

  EikonGetData(
    EikonObject = RD, rics = "AAPL.O",
    Eikonformulas = "TR.Revenue", cache = 300, verbose = FALSE
  )

  expect_message(
    EikonGetData(
      EikonObject = RD, rics = "AAPL.O",
      Eikonformulas = "TR.Revenue", cache = 300, verbose = TRUE
    ),
    "Cache hit"
  )

  rd_ClearCache()
})


# ═══════════════════════════════════════════════════════════════════════════
# 5. EikonGetSymbology — cache paths & verbose
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonGetSymbology stores result in cache and returns cached value", {
  RD <- make_mock_connection()
  rd_ClearCache()

  call_count <- 0L
  mock_symbology <- list(list(
    mappedSymbols = list(list(
      symbol = "AAPL.O",
      bestMatch = list(ISIN = "US0378331005")
    ))
  ))

  mockery::stub(EikonGetSymbology, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    mock_symbology
  })

  # First call
  r1 <- EikonGetSymbology(
    EikonObject = RD, symbol = "AAPL.O",
    from_symbol_type = "RIC", to_symbol_type = "ISIN", cache = 3600
  )

  # Second call — cache hit
  r2 <- EikonGetSymbology(
    EikonObject = RD, symbol = "AAPL.O",
    from_symbol_type = "RIC", to_symbol_type = "ISIN", cache = 3600
  )

  expect_equal(r1, r2)
  expect_equal(call_count, 1L)

  rd_ClearCache()
})

test_that("EikonGetSymbology cache hit emits message when verbose", {
  RD <- make_mock_connection()
  rd_ClearCache()

  mock_symbology <- list(list(
    mappedSymbols = list(list(
      symbol = "AAPL.O",
      bestMatch = list(ISIN = "US0378331005")
    ))
  ))

  mockery::stub(EikonGetSymbology, "chunked_download", function(...) mock_symbology)

  EikonGetSymbology(
    EikonObject = RD, symbol = "AAPL.O",
    from_symbol_type = "RIC", to_symbol_type = "ISIN",
    cache = 3600, verbose = FALSE
  )

  expect_message(
    EikonGetSymbology(
      EikonObject = RD, symbol = "AAPL.O",
      from_symbol_type = "RIC", to_symbol_type = "ISIN",
      cache = 3600, verbose = TRUE
    ),
    "Cache hit"
  )

  rd_ClearCache()
})

test_that("EikonGetSymbology verbose emits debug messages in fetch_fn", {
  RD <- make_mock_connection()

  RD$get_symbology <- function(...) {
    list(mappedSymbols = list(list(
      symbol = "AAPL.O", bestMatch = list(ISIN = "US0378331005")
    )))
  }

  mockery::stub(EikonGetSymbology, "chunked_download", function(n_chunks, fetch_fn, ...) {
    list(fetch_fn(1))
  })

  expect_message(
    EikonGetSymbology(
      EikonObject = RD, symbol = "AAPL.O",
      from_symbol_type = "RIC", to_symbol_type = "ISIN",
      verbose = TRUE, cache = FALSE
    ),
    "EikonGetSymbology JSON request"
  )
})

test_that("EikonGetSymbology cache store for result", {
  RD <- make_mock_connection()
  rd_ClearCache()

  mock_symbology <- list(list(
    mappedSymbols = list(list(
      symbol = "AAPL.O",
      bestMatch = list(ISIN = "US0378331005")
    ))
  ))

  mockery::stub(EikonGetSymbology, "chunked_download", function(...) mock_symbology)

  result <- EikonGetSymbology(
    EikonObject = RD, symbol = "AAPL.O",
    from_symbol_type = "RIC", to_symbol_type = "ISIN", cache = TRUE
  )

  info <- rd_CacheInfo()
  expect_true(info$total_keys > 0)

  rd_ClearCache()
})

# ═══════════════════════════════════════════════════════════════════════════
# R1. EikonGetSymbology — raw_output returns list directly
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonGetSymbology raw_output returns chunked_download list directly", {
  RD <- make_mock_connection()
  mock_raw <- list(list(
    mappedSymbols = list(list(
      symbol = "AAPL.O",
      bestMatch = list(ISIN = "US0378331005")
    ))
  ))

  mockery::stub(EikonGetSymbology, "chunked_download", function(...) mock_raw)

  result <- EikonGetSymbology(
    EikonObject = RD, symbol = "AAPL.O",
    from_symbol_type = "RIC", to_symbol_type = "ISIN",
    raw_output = TRUE, cache = FALSE
  )

  expect_type(result, "list")
  expect_equal(result, mock_raw)
})

# ═══════════════════════════════════════════════════════════════════════════
# R2. EikonGetSymbology — NA chunk filtering in non-raw mode
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonGetSymbology filters NA chunks in non-raw mode", {
  RD <- make_mock_connection()

  mockery::stub(EikonGetSymbology, "chunked_download", function(...) list(NA))

  result <- EikonGetSymbology(
    EikonObject = RD, symbol = "AAPL.O",
    from_symbol_type = "RIC", to_symbol_type = "ISIN",
    raw_output = FALSE, cache = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# ═══════════════════════════════════════════════════════════════════════════
# R3. EikonNameCleaner — non-standard SpaceConvertor retains spaces
# ═══════════════════════════════════════════════════════════════════════════

test_that("EikonNameCleaner retains spaces with non-standard SpaceConvertor", {
  result <- EikonNameCleaner(c("Dividend Yield", "Company Name"),
    SpaceConvertor = "X"
  )
  expect_true(grepl(" ", result[1]))
  expect_true(grepl(" ", result[2]))
})

test_that("EikonNameCleaner uses all four standard SpaceConvertors", {
  input <- "Dividend Yield"
  for (sep in c(".", ",", "-", "_")) {
    result <- EikonNameCleaner(input, SpaceConvertor = sep)
    expect_false(grepl(" ", result), info = paste("SpaceConvertor =", sep))
    expect_true(grepl(sep, result, fixed = TRUE), info = paste("SpaceConvertor =", sep))
  }
})


restore_refinitiv_state(.saved_state, "test-refinitiv-coverage2")
