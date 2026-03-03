library(testthat)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# ── Offline / mock tests ────────────────────────────────────────────────────

test_that("EikonGetSymbology delegates to chunked_download()", {
  local_refinitiv_state()
  withr::local_options(list(
    eikon_port = 9000L,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  ))
  refinitiv_vault_set("api_key", "dummy_key")
  RD <- RefinitivJsonConnect("dummy_key")

  # Capture the arguments passed to chunked_download
  captured_args <- NULL
  mockery::stub(EikonGetSymbology, "chunked_download", function(...) {
    captured_args <<- list(...)
    # Return a single-chunk result mimicking a raw symbology response
    list(list(
      mappedSymbols = list(list(
        symbol = "AAPL.O",
        bestMatch = list(ISIN = "US0378331005")
      ))
    ))
  })

  result <- EikonGetSymbology(
    EikonObject = RD,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    cache = FALSE
  )

  expect_false(is.null(captured_args))
  expect_equal(captured_args$n_chunks, 1L)
  expect_equal(captured_args$sleep, 0.5)
  expect_equal(captured_args$on_failure, "warning")
  expect_type(captured_args$fetch_fn, "closure")
  expect_s3_class(result, "data.frame")
})

test_that("EikonGetSymbology raw_output returns chunked_download list directly", {
  local_refinitiv_state()
  withr::local_options(list(
    eikon_port = 9000L,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  ))
  refinitiv_vault_set("api_key", "dummy_key")
  RD <- RefinitivJsonConnect("dummy_key")

  mock_raw <- list(mappedSymbols = list(list(symbol = "AAPL.O")))
  mockery::stub(EikonGetSymbology, "chunked_download", function(...) {
    list(mock_raw)
  })

  result <- EikonGetSymbology(
    EikonObject = RD,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    raw_output = TRUE,
    cache = FALSE
  )

  expect_type(result, "list")
  expect_equal(result, list(mock_raw))
})

test_that("EikonGetSymbology passes verbose to chunked_download", {
  local_refinitiv_state()
  withr::local_options(list(
    eikon_port = 9000L,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  ))
  refinitiv_vault_set("api_key", "dummy_key")
  RD <- RefinitivJsonConnect("dummy_key")

  captured_verbose <- NULL
  mockery::stub(EikonGetSymbology, "chunked_download", function(...) {
    args <- list(...)
    captured_verbose <<- args$verbose
    list(list(mappedSymbols = list(list(
      symbol = "AAPL.O",
      bestMatch = list(ISIN = "US0378331005")
    ))))
  })

  EikonGetSymbology(
    EikonObject = RD,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    verbose = TRUE,
    cache = FALSE
  )
  expect_true(captured_verbose)

  EikonGetSymbology(
    EikonObject = RD,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    verbose = FALSE,
    cache = FALSE
  )
  expect_false(captured_verbose)
})

test_that("EikonGetSymbology chunks multiple symbols", {
  local_refinitiv_state()
  withr::local_options(list(
    eikon_port = 9000L,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  ))
  refinitiv_vault_set("api_key", "dummy_key")
  RD <- RefinitivJsonConnect("dummy_key")

  # Generate enough symbols to trigger chunking (>300 RICs * 1 field = 300 data points,
  # but EikonChunker max is 12000 points and 300 RICs, so 301 symbols should produce 2 chunks)
  many_symbols <- paste0("SYM", seq_len(301), ".O")

  captured_n_chunks <- NULL
  mockery::stub(EikonGetSymbology, "chunked_download", function(...) {
    args <- list(...)
    captured_n_chunks <<- args$n_chunks
    # Return one NA-result per chunk (raw mode, so no ProcessSymbology)
    as.list(rep(NA, args$n_chunks))
  })

  result <- EikonGetSymbology(
    EikonObject = RD,
    symbol = many_symbols,
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    raw_output = TRUE,
    cache = FALSE
  )

  expect_true(!is.null(captured_n_chunks))
  expect_gt(captured_n_chunks, 1L)
})

test_that("EikonGetSymbology uses retry with max_attempts = 1L (A5 fix)", {
  local_refinitiv_state()
  withr::local_options(list(
    eikon_port = 9000L,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  ))
  refinitiv_vault_set("api_key", "dummy_key")
  RD <- RefinitivJsonConnect("dummy_key")

  captured_retry_args <- NULL
  # Stub retry inside EikonGetSymbology to capture its arguments
  mockery::stub(EikonGetSymbology, "retry", function(fn, ...) {
    captured_retry_args <<- list(...)
    # Return a mock symbology result
    list(mappedSymbols = list(list(
      symbol = "AAPL.O",
      bestMatch = list(ISIN = "US0378331005")
    )))
  })

  result <- EikonGetSymbology(
    EikonObject = RD,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    raw_output = TRUE,
    cache = FALSE
  )

  expect_false(is.null(captured_retry_args))
  expect_equal(captured_retry_args$max_attempts, 1L)
  expect_equal(captured_retry_args$on_failure, "NA")
})

# ── Live tests ──────────────────────────────────────────────────────────────
# These tests require a live Eikon/Workspace terminal connection.
# Offline coverage for EikonGetSymbology is provided by
# test-httptest2-integration.R using recorded fixtures.

test_that("EikonGetSymbology converts RIC to ISIN", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetSymbology(
    EikonObject = Eikon,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN"
  )

  expect_s3_class(result, "data.frame")
  expect_true("ISIN" %in% names(result) || "bestMatch" %in% names(result))
})

test_that("EikonGetSymbology converts ISIN to RIC", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetSymbology(
    EikonObject = Eikon,
    symbol = "US0378331005",
    from_symbol_type = "ISIN",
    to_symbol_type = "RIC"
  )

  expect_s3_class(result, "data.frame")
})

test_that("EikonGetSymbology handles non-existing symbols", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetSymbology(
    EikonObject = Eikon,
    symbol = c("AAPL.O", "NONEXISTENT"),
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN"
  )

  expect_s3_class(result, "data.frame")
})

test_that("EikonGetSymbology returns raw output", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetSymbology(
    EikonObject = Eikon,
    symbol = "AAPL.O",
    from_symbol_type = "RIC",
    to_symbol_type = "ISIN",
    raw_output = TRUE
  )

  expect_type(result, "list")
})


restore_refinitiv_state(.saved_state, "test-EikonGetSymbology")
