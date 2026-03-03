# Tests for rd_GetOwnership()

# ── Mock response payloads ──

mock_ownership_breakdown <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "investor_type", title = "Investor Type", type = "String"),
    list(name = "pct_held", title = "% Held", type = "Double"),
    list(name = "holder_count", title = "Holder Count", type = "Int32")
  ),
  data = list(
    list("AAPL.O", "Investment Advisor", 42.5, 1500),
    list("AAPL.O", "Hedge Fund Manager", 8.3, 350),
    list("AAPL.O", "Pension Fund", 6.1, 200)
  )
)

mock_ownership_org_info <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "org_name", title = "Organization Name", type = "String"),
    list(name = "total_value", title = "Total Value", type = "Double")
  ),
  data = list(
    list("IBM.N", "International Business Machines Corporation", 150000000000)
  )
)

# For pagination testing: 100-row page (simulates a full page)
make_mock_page <- function(n, offset = 0L) {
  rows <- lapply(seq_len(n), function(i) {
    list(paste0("INV_", offset + i), "Type A", 1.0, i + offset)
  })
  list(
    headers = list(
      list(name = "investor_id", title = "Investor ID", type = "String"),
      list(name = "investor_type", title = "Investor Type", type = "String"),
      list(name = "pct_held", title = "% Held", type = "Double"),
      list(name = "rank", title = "Rank", type = "Int32")
    ),
    data = rows
  )
}


# ── Input validation ──

test_that("rd_GetOwnership errors when universe is missing", {
  expect_error(rd_GetOwnership(), "universe")
})

test_that("rd_GetOwnership errors on invalid view", {
  expect_error(
    rd_GetOwnership(universe = "AAPL.O", view = "nonexistent"),
    "arg"
  )
})

test_that("rd_GetOwnership errors when stat_type missing for breakdown view", {
  mock_conn <- list(get_ownership = function(...) mock_ownership_breakdown)
  expect_error(
    rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "consolidated/breakdown"
    ),
    "stat_type.*required"
  )
})

test_that("rd_GetOwnership errors when stat_type out of range", {
  mock_conn <- list(get_ownership = function(...) mock_ownership_breakdown)
  expect_error(
    rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "consolidated/breakdown", stat_type = 0
    ),
    "between 1 and 8"
  )
  expect_error(
    rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "consolidated/breakdown", stat_type = 9
    ),
    "between 1 and 8"
  )
})

test_that("rd_GetOwnership errors on invalid sort_order", {
  mock_conn <- list(get_ownership = function(...) mock_ownership_breakdown)
  expect_error(
    rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "consolidated/investors", sort_order = "sideways"
    ),
    "arg"
  )
})

test_that("rd_GetOwnership errors on invalid frequency", {
  mock_conn <- list(get_ownership = function(...) mock_ownership_breakdown)
  expect_error(
    rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "consolidated/investors", frequency = "W"
    ),
    "arg"
  )
})

test_that("rd_GetOwnership does not require stat_type for non-breakdown views", {
  mock_conn <- list(
    get_ownership = function(...) mock_ownership_org_info
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "IBM.N",
    view     = "org-info"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
})


# ── Core functionality (mocked) ──

test_that("rd_GetOwnership returns data.frame for breakdown view", {
  mock_conn <- list(
    get_ownership = function(...) mock_ownership_breakdown
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject  = mock_conn,
    universe  = "AAPL.O",
    view      = "consolidated/breakdown",
    stat_type = 1
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  expect_true("Investor Type" %in% names(result))
  expect_type(result[["% Held"]], "double")
  expect_type(result[["Holder Count"]], "integer")
})

test_that("rd_GetOwnership returns data.frame for org-info view", {
  mock_conn <- list(
    get_ownership = function(...) mock_ownership_org_info
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "IBM.N",
    view     = "org-info"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(
    result[["Organization Name"]],
    "International Business Machines Corporation"
  )
})

test_that("rd_GetOwnership raw_output returns list (single page)", {
  mock_conn <- list(
    get_ownership = function(...) mock_ownership_breakdown
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  # Use explicit limit <= 100 to take single-page path
  result <- rd_GetOwnership(
    RDObject   = mock_conn,
    universe   = "AAPL.O",
    view       = "consolidated/breakdown",
    stat_type  = 1,
    limit      = 50,
    raw_output = TRUE
  )
  expect_type(result, "list")
  expect_true("headers" %in% names(result))
})

test_that("rd_GetOwnership raw_output returns list of pages (fetch-all)", {
  mock_conn <- list(
    get_ownership = function(...) mock_ownership_breakdown
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  # limit = NULL → fetch-all pagination; mock returns 3 rows → exhaustion
  result <- rd_GetOwnership(
    RDObject   = mock_conn,
    universe   = "AAPL.O",
    view       = "consolidated/breakdown",
    stat_type  = 1,
    limit      = NULL,
    raw_output = TRUE
  )
  expect_type(result, "list")
  expect_true(length(result) >= 1L)
})


# ── Pagination ──

test_that("rd_GetOwnership auto-paginates when limit > 100", {
  page_calls <- 0L
  mock_conn <- list(
    get_ownership = function(...) {
      args <- list(...)
      page_calls <<- page_calls + 1L
      n <- args$limit %||% 100L
      off <- args$offset %||% 0L
      make_mock_page(n, off)
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "consolidated/investors",
    limit    = 250
  )

  expect_s3_class(result, "data.frame")
  # 250 / 100 = 3 pages (100 + 100 + 50)
  expect_equal(page_calls, 3L)
  expect_equal(nrow(result), 250L)
})

test_that("rd_GetOwnership stops early when API returns fewer rows", {
  page_calls <- 0L
  mock_conn <- list(
    get_ownership = function(...) {
      args <- list(...)
      page_calls <<- page_calls + 1L
      off <- args$offset %||% 0L
      # First page: 100 rows, second page: only 30 (exhausted)
      if (page_calls == 1L) {
        make_mock_page(100, 0)
      } else {
        make_mock_page(30, 100)
      }
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "consolidated/investors",
    limit    = 500 # Requesting 500, but only 130 exist
  )

  expect_equal(page_calls, 2L) # Stopped after second page
  expect_equal(nrow(result), 130L)
})

test_that("rd_GetOwnership fetch-all (limit=NULL) paginates until exhaustion", {
  page_calls <- 0L
  mock_conn <- list(
    get_ownership = function(...) {
      page_calls <<- page_calls + 1L
      off <- (page_calls - 1L) * 100L
      if (page_calls <= 2L) {
        make_mock_page(100, off)
      } else {
        # Third page: only 20 rows → signals exhaustion
        make_mock_page(20, off)
      }
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "fund/investors",
    limit    = NULL # fetch all
  )

  expect_equal(page_calls, 3L)
  expect_equal(nrow(result), 220L) # 100 + 100 + 20
})

test_that("rd_GetOwnership fetch-all stops on empty page", {
  page_calls <- 0L
  mock_conn <- list(
    get_ownership = function(...) {
      page_calls <<- page_calls + 1L
      if (page_calls == 1L) {
        make_mock_page(100, 0)
      } else {
        # Empty second page
        list(
          headers = list(
            list(name = "investor_id", title = "Investor ID", type = "String"),
            list(name = "investor_type", title = "Investor Type", type = "String"),
            list(name = "pct_held", title = "% Held", type = "Double"),
            list(name = "rank", title = "Rank", type = "Int32")
          ),
          data = list()
        )
      }
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "fund/investors",
    limit    = NULL
  )

  expect_equal(page_calls, 2L)
  expect_equal(nrow(result), 100L)
})

test_that("rd_GetOwnership single page (limit <= 100) does not paginate", {
  page_calls <- 0L
  mock_conn <- list(
    get_ownership = function(...) {
      page_calls <<- page_calls + 1L
      make_mock_page(50, 0)
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject = mock_conn,
    universe = "AAPL.O",
    view     = "fund/investors",
    limit    = 50
  )

  expect_equal(page_calls, 1L)
  expect_equal(nrow(result), 50L)
})

test_that("rd_GetOwnership raw_output with pagination returns list of pages", {
  mock_conn <- list(
    get_ownership = function(...) {
      args <- list(...)
      n <- args$limit %||% 100L
      off <- args$offset %||% 0L
      make_mock_page(n, off)
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  result <- rd_GetOwnership(
    RDObject   = mock_conn,
    universe   = "AAPL.O",
    view       = "fund/investors",
    limit      = 200,
    raw_output = TRUE
  )

  expect_type(result, "list")
  expect_length(result, 2L) # 2 pages of 100
})


# ── View matching ──

test_that("rd_GetOwnership accepts all valid views", {
  all_views <- c(
    "consolidated/breakdown", "consolidated/concentration",
    "consolidated/top-n-concentration", "consolidated/investors",
    "consolidated/shareholders-report",
    "consolidated/shareholders-history-report",
    "consolidated/recent-activity",
    "fund/holdings", "fund/breakdown", "fund/concentration",
    "fund/top-n-concentration", "fund/investors",
    "fund/shareholders-report", "fund/shareholders-history-report",
    "fund/recent-activity",
    "insider/shareholders-report", "insider/transaction-report",
    "investor/holdings", "org-info"
  )

  mock_conn <- list(
    get_ownership = function(...) mock_ownership_org_info
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  for (v in all_views) {
    is_breakdown <- grepl("/breakdown$", v)
    st <- if (is_breakdown) 1 else NULL
    result <- rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = v, stat_type = st
    )
    expect_true(is.data.frame(result), label = paste("view:", v))
  }
})


# ── Caching ──

test_that("rd_GetOwnership caches results", {
  call_count <- 0L
  mock_conn <- list(
    get_ownership = function(...) {
      call_count <<- call_count + 1L
      mock_ownership_breakdown
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  withr::local_options(refinitiv_cache = FALSE)
  rd_ClearCache()

  result1 <- rd_GetOwnership(
    RDObject = mock_conn, universe = "AAPL.O",
    view = "consolidated/breakdown", stat_type = 1, cache = TRUE
  )
  result2 <- rd_GetOwnership(
    RDObject = mock_conn, universe = "AAPL.O",
    view = "consolidated/breakdown", stat_type = 1, cache = TRUE
  )
  expect_equal(call_count, 1L)
  expect_equal(result1, result2)
})

# ── API errors ──

test_that("rd_GetOwnership propagates API errors", {
  mock_conn <- list(
    get_ownership = function(...) {
      list(error = list(message = "Access denied — insufficient permissions"))
    }
  )
  mockery::stub(rd_GetOwnership, "retry", function(fn, ...) fn())

  expect_error(
    rd_GetOwnership(
      RDObject = mock_conn, universe = "AAPL.O",
      view = "consolidated/investors"
    ),
    "LSEG API error"
  )
})


# ── Live tests ──

test_that("rd_GetOwnership works with live terminal (consolidated/breakdown)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetOwnership(
      universe  = "AAPL.O",
      view      = "consolidated/breakdown",
      stat_type = 1
    ),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message)) {
        skip("Ownership API entitlement not available")
      }
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("rd_GetOwnership works with live terminal (org-info)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetOwnership(
      universe = "IBM.N",
      view     = "org-info"
    ),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message)) {
        skip("Ownership API entitlement not available")
      }
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("rd_GetOwnership works with live terminal (paginated fund/investors)", {
  skip_if_not(has_live_api(), "No live Eikon/Workspace terminal available")

  result <- tryCatch(
    rd_GetOwnership(
      universe = "AAPL.O",
      view     = "fund/investors",
      limit    = 150 # Should trigger 2 pages
    ),
    error = function(e) {
      if (grepl("access denied|Scopes required", e$message)) {
        skip("Ownership API entitlement not available")
      }
      stop(e)
    }
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})
