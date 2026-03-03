# Tests for rd_GetEstimates()

# ── Mock response payloads ──

mock_estimates_summary <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "financial_year", title = "Financial Year", type = "String"),
    list(name = "revenue_mean", title = "Revenue - Mean", type = "Double"),
    list(name = "eps_mean", title = "EPS - Mean", type = "Double")
  ),
  data = list(
    list("AAPL.O", "FY2024", 385000000000, 6.55),
    list("AAPL.O", "FY2025", 410000000000, 7.12)
  )
)

mock_estimates_kpi <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "kpi_name", title = "KPI Name", type = "String"),
    list(name = "kpi_value", title = "KPI Value", type = "Double")
  ),
  data = list(
    list("TSLA.O", "Deliveries", 1800000)
  )
)

mock_estimates_recommendations <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "mean_rec", title = "Mean Recommendation", type = "Double"),
    list(name = "num_buys", title = "Number of Buys", type = "Int32")
  ),
  data = list(
    list("AAPL.O", 1.8, 30),
    list("MSFT.O", 1.5, 35)
  )
)


# ── Input validation ──

test_that("rd_GetEstimates errors when universe is missing", {
  expect_error(rd_GetEstimates(), "universe")
})

test_that("rd_GetEstimates errors on invalid view", {
  expect_error(
    rd_GetEstimates(universe = "AAPL.O", view = "nonexistent", package = "basic"),
    "arg"
  )
})

test_that("rd_GetEstimates errors when package is missing for non-KPI view", {
  mock_conn <- list(get_estimates = function(...) mock_estimates_summary)
  expect_error(
    rd_GetEstimates(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "view-summary/annual", package = NULL
    ),
    "package.*required"
  )
})

test_that("rd_GetEstimates errors on invalid package value", {
  mock_conn <- list(get_estimates = function(...) mock_estimates_summary)
  expect_error(
    rd_GetEstimates(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "view-summary/annual", package = "gold"
    ),
    "arg"
  )
})

test_that("rd_GetEstimates does NOT error when package is NULL for KPI view", {
  mock_conn <- list(
    get_estimates = function(...) mock_estimates_kpi
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  # Should not error — KPI views don't need package
  result <- rd_GetEstimates(
    RDObject = mock_conn,
    universe = "TSLA.O",
    view     = "view-actuals-kpi/annual",
    package  = NULL
  )
  expect_s3_class(result, "data.frame")
})


# ── Core functionality (mocked) ──

test_that("rd_GetEstimates returns data.frame for summary/annual", {
  mock_conn <- list(
    get_estimates = function(...) mock_estimates_summary
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  result <- rd_GetEstimates(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "view-summary/annual",
    package  = "basic"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_true("Revenue - Mean" %in% names(result))
  expect_type(result[["EPS - Mean"]], "double")
})

test_that("rd_GetEstimates returns data.frame for recommendations", {
  mock_conn <- list(
    get_estimates = function(...) mock_estimates_recommendations
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  result <- rd_GetEstimates(
    RDObject = mock_conn,
    universe = c("AAPL.O", "MSFT.O"),
    view     = "view-summary/recommendations",
    package  = "standard"
  )
  expect_equal(nrow(result), 2L)
  expect_type(result[["Number of Buys"]], "integer")
})

test_that("rd_GetEstimates passes correct package to connection method", {
  captured_args <- NULL
  mock_conn <- list(
    get_estimates = function(...) {
      captured_args <<- list(...)
      mock_estimates_summary
    }
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  rd_GetEstimates(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "view-actuals/annual",
    package  = "professional"
  )
  expect_equal(captured_args$package, "professional")
  expect_equal(captured_args$view, "view-actuals/annual")
})

test_that("rd_GetEstimates sends NULL package for KPI views", {
  captured_args <- NULL
  mock_conn <- list(
    get_estimates = function(...) {
      captured_args <<- list(...)
      mock_estimates_kpi
    }
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  rd_GetEstimates(
    RDObject = mock_conn,
    universe = "TSLA.O",
    view     = "view-summary-kpi/annual"
  )
  expect_null(captured_args$package)
})

test_that("rd_GetEstimates raw_output returns list", {
  mock_conn <- list(
    get_estimates = function(...) mock_estimates_summary
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  result <- rd_GetEstimates(
    RDObject   = mock_conn,
    universe   = "AAPL.O",
    view       = "view-summary/annual",
    package    = "basic",
    raw_output = TRUE
  )
  expect_type(result, "list")
  expect_true("headers" %in% names(result))
})

test_that("rd_GetEstimates propagates API errors", {
  mock_conn <- list(
    get_estimates = function(...) {
      list(error = list(message = "Insufficient entitlement for professional package"))
    }
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  expect_error(
    rd_GetEstimates(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "view-summary/annual", package = "professional"
    ),
    "LSEG API error.*Insufficient entitlement"
  )
})


# ── View matching ──

test_that("rd_GetEstimates accepts all valid views", {
  all_views <- c(
    "view-summary/annual", "view-summary/interim",
    "view-summary/recommendations", "view-summary/non-periodic-measures",
    "view-summary/historical-snapshots-periodic-measures-annual",
    "view-summary/historical-snapshots-periodic-measures-interim",
    "view-summary/historical-snapshots-non-periodic-measures",
    "view-summary/historical-snapshots-recommendations",
    "view-actuals/annual", "view-actuals/interim",
    "view-actuals-kpi/annual", "view-actuals-kpi/interim",
    "view-summary-kpi/annual", "view-summary-kpi/interim",
    "view-summary-kpi/historical-snapshots-kpi"
  )

  mock_conn <- list(
    get_estimates = function(...) mock_estimates_summary
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  for (v in all_views) {
    is_kpi <- grepl("kpi", v, fixed = TRUE)
    pkg <- if (is_kpi) NULL else "basic"
    result <- rd_GetEstimates(
      RDObject = mock_conn, universe = "AAPL.O",
      view = v, package = pkg
    )
    expect_true(is.data.frame(result), label = paste("view:", v))
  }
})


# ── Caching ──

test_that("rd_GetEstimates caches results", {
  call_count <- 0L
  mock_conn <- list(
    get_estimates = function(...) {
      call_count <<- call_count + 1L
      mock_estimates_summary
    }
  )
  mockery::stub(rd_GetEstimates, "retry", function(fn, ...) fn())

  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  result1 <- rd_GetEstimates(
    RDObject = mock_conn, universe = "AAPL.O",
    view = "view-summary/annual", package = "basic",
    cache = TRUE
  )
  result2 <- rd_GetEstimates(
    RDObject = mock_conn, universe = "AAPL.O",
    view = "view-summary/annual", package = "basic",
    cache = TRUE
  )
  expect_equal(call_count, 1L)
  expect_equal(result1, result2)
})


# ── Live tests ──

test_that("rd_GetEstimates works with live terminal (summary/annual)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetEstimates(
      universe = "AAPL.O",
      view = "view-summary/annual", package = "basic"
    ),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message)) {
        skip("Estimates API entitlement not available")
      }
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("rd_GetEstimates works with live terminal (KPI actuals)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetEstimates(
      universe = "AAPL.O",
      view = "view-actuals-kpi/annual"
    ),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message)) {
        skip("Estimates API entitlement not available")
      }
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})
