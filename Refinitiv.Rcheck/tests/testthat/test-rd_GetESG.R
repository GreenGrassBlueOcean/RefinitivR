# Tests for rd_GetESG()

# ── Mock response payloads ──

mock_esg_scores <- list(
  headers = list(
    list(name = "instrument",  title = "Instrument",    type = "String"),
    list(name = "period_end",  title = "Period End Date", type = "Date"),
    list(name = "esg_score",   title = "ESG Score",     type = "Double"),
    list(name = "env_pillar",  title = "Environment Pillar Score", type = "Double")
  ),
  data = list(
    list("AAPL.O", "2023-12-31", 82.5, 76.1),
    list("MSFT.O", "2023-12-31", 78.3, 80.2)
  )
)

mock_esg_basic <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "company",    title = "Company Name", type = "String")
  ),
  data = list(
    list("IBM.N", "International Business Machines Corp")
  )
)

mock_esg_empty <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String")
  ),
  data = list()
)


# ── Input validation ──

test_that("rd_GetESG errors when universe is missing", {
  expect_error(rd_GetESG(), "universe")
})

test_that("rd_GetESG errors when universe is NULL", {
  expect_error(rd_GetESG(universe = NULL), "universe")
})

test_that("rd_GetESG errors when universe is empty vector", {
  expect_error(rd_GetESG(universe = character(0)), "universe")
})

test_that("rd_GetESG errors on invalid view", {
  expect_error(
    rd_GetESG(universe = "AAPL.O", view = "nonexistent"),
    "arg"
  )
})

test_that("rd_GetESG errors on non-numeric start", {
  expect_error(
    rd_GetESG(universe = "AAPL.O", start = "abc"),
    "numeric year"
  )
})

test_that("rd_GetESG errors on non-numeric end", {
  expect_error(
    rd_GetESG(universe = "AAPL.O", end = "abc"),
    "numeric year"
  )
})


# ── Core functionality (mocked) ──

test_that("rd_GetESG returns data.frame for scores-full view", {
  mock_conn <- list(
    get_esg = function(...) mock_esg_scores
  )
  mockery::stub(rd_GetESG, "RefinitivJsonConnect", function(...) mock_conn)
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  result <- rd_GetESG(
    RDObject = mock_conn,
    universe = c("AAPL.O", "MSFT.O"),
    view     = "scores-full"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_true("ESG Score" %in% names(result))
  expect_type(result[["ESG Score"]], "double")
})

test_that("rd_GetESG returns data.frame for basic view", {
  mock_conn <- list(
    get_esg = function(...) mock_esg_basic
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  result <- rd_GetESG(
    RDObject = mock_conn,
    universe = "IBM.N",
    view     = "basic"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_equal(result[["Company Name"]], "International Business Machines Corp")
})

test_that("rd_GetESG returns empty data.frame when API returns no data", {
  mock_conn <- list(
    get_esg = function(...) mock_esg_empty
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  result <- rd_GetESG(
    RDObject = mock_conn,
    universe = "AAPL.O"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("rd_GetESG raw_output returns the list as-is", {
  mock_conn <- list(
    get_esg = function(...) mock_esg_scores
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  result <- rd_GetESG(
    RDObject   = mock_conn,
    universe   = "AAPL.O",
    raw_output = TRUE
  )
  expect_type(result, "list")
  expect_true("headers" %in% names(result))
  expect_true("data" %in% names(result))
})

test_that("rd_GetESG uses name headers when use_field_names_in_headers = FALSE", {
  mock_conn <- list(
    get_esg = function(...) mock_esg_scores
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  result <- rd_GetESG(
    RDObject = mock_conn,
    universe = "AAPL.O",
    use_field_names_in_headers = FALSE
  )
  expect_true("instrument" %in% names(result))
  expect_true("esg_score" %in% names(result))
})

test_that("rd_GetESG propagates API errors", {
  mock_conn <- list(
    get_esg = function(...) list(error = list(message = "No ESG data available"))
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  expect_error(
    rd_GetESG(RDObject = mock_conn, universe = "INVALID.RIC"),
    "LSEG API error"
  )
})

test_that("rd_GetESG passes start/end parameters correctly", {
  captured_args <- NULL
  mock_conn <- list(
    get_esg = function(...) {
      captured_args <<- list(...)
      mock_esg_scores
    }
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  rd_GetESG(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "scores-full",
    start    = 2020,
    end      = 2023
  )

  expect_equal(captured_args$start, 2020L)
  expect_equal(captured_args$end, 2023L)
})


# ── View matching ──

test_that("rd_GetESG accepts all valid views", {
  valid_views <- c("scores-full", "scores-standard", "measures-full",
                   "measures-standard", "basic", "universe")

  mock_conn <- list(
    get_esg = function(...) mock_esg_basic
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  for (v in valid_views) {
    result <- rd_GetESG(RDObject = mock_conn, universe = "AAPL.O", view = v)
    expect_s3_class(result, "data.frame")
  }
})


# ── Caching ──

test_that("rd_GetESG caches results when cache = TRUE", {
  call_count <- 0L
  mock_conn <- list(
    get_esg = function(...) {
      call_count <<- call_count + 1L
      mock_esg_scores
    }
  )
  mockery::stub(rd_GetESG, "retry", function(fn, ...) fn())

  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  # First call — miss

  result1 <- rd_GetESG(RDObject = mock_conn, universe = "AAPL.O", cache = TRUE)
  expect_equal(call_count, 1L)

  # Second call — hit
  result2 <- rd_GetESG(RDObject = mock_conn, universe = "AAPL.O", cache = TRUE)
  expect_equal(call_count, 1L)  # Not incremented
  expect_equal(result1, result2)
})


# ── Live tests ──

test_that("rd_GetESG works with live terminal (scores-full)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetESG(universe = "AAPL.O", view = "scores-full"),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message))
        skip("ESG API entitlement not available")
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  expect_true("Instrument" %in% names(result))
})

test_that("rd_GetESG works with live terminal (basic view)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetESG(universe = c("AAPL.O", "MSFT.O"), view = "basic"),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message))
        skip("ESG API entitlement not available")
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})
