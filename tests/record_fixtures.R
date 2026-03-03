# Record httptest2 fixtures for offline testing.
#
# Run this script interactively while a Refinitiv Workspace / Eikon
# terminal is running. It will record HTTP request/response pairs
# into fixture directories under tests/testthat/.
#
# The package redactor (inst/httptest2/redact.R) normalises recorded
# URLs to use "lh:9000" as host:port, producing fixture subdirectories
# named "lh-9000/".  Mock playback in tests uses
# refinitiv_base_url = "http://lh" with eikon_port = 9000 to match.
#
# Usage (from project root, in R):
#   source("tests/record_fixtures.R")
#
# Prerequisites:
#   - Active Refinitiv Workspace or Eikon Desktop
#   - Valid API key configured

library(Refinitiv)
library(httptest2)

fixture_dir <- file.path("tests", "testthat")

# Clear any stale mock state from previous test runs
options(.EikonApiKey = NULL)

cat("Connecting via JSON...\n")
RD <- RDConnect()
Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

record <- function(name, expr) {
  dir <- file.path(fixture_dir, name)
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
    cat(sprintf("  [%s] cleaned old fixtures\n", name))
  }
  cat(sprintf("  [%s] recording... ", name))
  tryCatch(
    {
      # httptest2::capture_requests() writes to .mockPaths()[1]; temporarily
      # redirect that to our target directory.
      old_paths <- .mockPaths()
      .mockPaths(dir)
      on.exit(.mockPaths(old_paths), add = TRUE)
      capture_requests(expr)
      n <- length(list.files(dir, recursive = TRUE))
      cat(sprintf("OK (%d files)\n", n))
    },
    error = function(e) {
      cat(sprintf("FAILED: %s\n", conditionMessage(e)))
    }
  )
}

# --- Infrastructure ---
cat("\n=== Infrastructure ===\n")
record("chk-terminal", {
  options(eikon_port = NULL) # ensure probe is not short-circuited
  CheckTerminalType(force = TRUE)
})
record("handshake", rd_handshake(force = TRUE, debug = FALSE))

# --- rd_GetData ---
cat("\n=== rd_GetData ===\n")
record("getdata-1ric", {
  rd_GetData(
    RDObject = RD, rics = "AAPL.O",
    Eikonformulas = c(
      "TR.COMPANYNAME", "TR.INSTRUMENTTYPE",
      "TR.EXCHANGENAME", "TR.INSTRUMENTISACTIVE"
    )
  )
})
record("getdata-nric", {
  rd_GetData(
    RDObject = RD,
    rics = c("AAPL.O", "GOOG.O", "III.L", "MMM"),
    Eikonformulas = c(
      "TR.InstrumentType", "TR.ExchangeName",
      "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive"
    )
  )
})
record("getdata-params", {
  rd_GetData(
    RDObject = RD, rics = "AAPL.O",
    Eikonformulas = c(
      "TR.CAEffectiveDate", "TR.CAAdjustmentFactor",
      "TR.CAAdjustmentType"
    ),
    Parameters = list(
      CAEventType = "SSP",
      SDate = "2020-10-27", EDate = "2020-12-01"
    )
  )
})
record("getdata-bad", {
  rd_GetData(
    RDObject = RD, rics = "WRONGRIC",
    Eikonformulas = "TR.COMPANYNAME"
  )
})

# --- rd_GetHistoricalPricing ---
cat("\n=== rd_GetHistoricalPricing ===\n")
record("histpx-1d", {
  rd_GetHistoricalPricing(
    RDObject = RD, universe = "VOD.L",
    interval = "P1D", count = 20L
  )
})
record("histpx-nric", {
  rd_GetHistoricalPricing(
    RDObject = RD,
    universe = c("VOD.L", "AAPL.O"),
    interval = "P1D", count = 20L
  )
})

# --- rd_GetHistory ---
cat("\n=== rd_GetHistory ===\n")
record("hist-px", {
  rd_GetHistory(
    RD = RD, universe = "AAPL.O",
    start = "2020-01-02", end = "2020-01-10", interval = "P1D"
  )
})
record("hist-nric", {
  rd_GetHistory(
    RD = RD, universe = c("GOOG.O", "MSFT.O"),
    start = "2020-01-02", end = "2020-01-10", interval = "P1D"
  )
})
record("hist-fund", {
  rd_GetHistory(
    RD = RD, universe = c("GOOG.O", "MSFT.O"),
    fields = c("TR.Revenue", "TR.GrossProfit"),
    parameters = list(SDate = 0, EDate = -3, FRQ = "FY", Curn = "USD")
  )
})

# --- EikonGetData ---
cat("\n=== EikonGetData ===\n")
record("ekdata-1ric", {
  EikonGetData(
    EikonObject = Eikon, rics = "MMM",
    Eikonformulas = c("TR.CompanyName", "CF_EXCHNG", "RDN_EXCHD2", "CURRENCY")
  )
})
record("ekdata-nric", {
  EikonGetData(
    EikonObject = Eikon, rics = c("MMM", "III.L"),
    Eikonformulas = c("TR.CompanyName", "CF_EXCHNG", "RDN_EXCHD2", "CURRENCY")
  )
})

# --- EikonGetTimeseries ---
cat("\n=== EikonGetTimeseries ===\n")
record("ekts-daily", {
  EikonGetTimeseries(
    EikonObject = Eikon, rics = "MMM",
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    corax = "unadjusted"
  )
})
record("ekts-nric", {
  EikonGetTimeseries(
    EikonObject = Eikon, rics = c("MMM", "III.L"),
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    corax = "unadjusted"
  )
})

# --- EikonGetSymbology ---
cat("\n=== EikonGetSymbology ===\n")
record("eksym-r2i", {
  EikonGetSymbology(
    EikonObject = Eikon,
    symbol = c("AAPL.O", "MSFT"),
    from_symbol_type = "RIC", to_symbol_type = "ISIN"
  )
})
record("eksym-i2r", {
  EikonGetSymbology(
    EikonObject = Eikon,
    symbol = c("US0378331005", "GB00B03MLX29"),
    from_symbol_type = "ISIN", to_symbol_type = "RIC"
  )
})

# --- RDPsearch ---
cat("\n=== RDPsearch ===\n")
record("search-basic", {
  RDPsearch(RDP = RD, query = "AAPL.O", top = 5)
})
record("search-people", {
  RDPsearch(RDP = RD, query = "president", view = "People", top = 5)
})

# --- RDPget_search_metadata ---
cat("\n=== RDPget_search_metadata ===\n")
record("searchmeta-eq", {
  RDPget_search_metadata(RDP = RD, searchView = "EquityQuotes")
})

# --- News ---
cat("\n=== News ===\n")
record("news-hl-msft", {
  rd_get_news_headlines(RDObject = RD, query = "R:MSFT.O", limit = 2L)
})
record("news-hl-aapl", {
  rd_get_news_headlines(RDObject = RD, query = "R:AAPL.O", limit = 2L)
})
# The news-story fixture records two requests: one headlines call to get a
# fresh story ID, and the story call itself.  The integration test replays
# both requests from the same fixture directory.
record("news-story", {
  headlines <- rd_get_news_headlines(RDObject = RD, query = "R:MSFT.O", limit = 1L)
  if (nrow(headlines) > 0 && "storyId" %in% names(headlines)) {
    rd_get_news_story(RDObject = RD, story_id = headlines$storyId[1])
  }
})
record("news-top", {
  rd_get_top_news(RDObject = RD)
})

# --- ESG ---
cat("\n=== ESG ===\n")
record("esg-basic", {
  rd_GetESG(RDObject = RD, universe = "AAPL.O", view = "basic")
})
record("esg-scores", {
  rd_GetESG(
    RDObject = RD, universe = c("AAPL.O", "MSFT.O"),
    view = "scores-full"
  )
})

# --- Estimates ---
cat("\n=== Estimates ===\n")
record("est-summary", {
  rd_GetEstimates(
    RDObject = RD, universe = "AAPL.O",
    view = "view-summary/annual", package = "basic"
  )
})
record("est-actuals", {
  rd_GetEstimates(
    RDObject = RD, universe = "AAPL.O",
    view = "view-actuals/annual", package = "basic"
  )
})
record("est-kpi", {
  rd_GetEstimates(
    RDObject = RD, universe = "AAPL.O",
    view = "view-actuals-kpi/annual"
  )
})

# --- Ownership ---
cat("\n=== Ownership ===\n")
record("own-consol", {
  rd_GetOwnership(
    RDObject = RD, universe = "AAPL.O",
    view = "consolidated/breakdown", stat_type = 1
  )
})
record("own-fund", {
  rd_GetOwnership(
    RDObject = RD, universe = "AAPL.O",
    view = "fund/investors", limit = 10
  )
})
record("own-org", {
  rd_GetOwnership(
    RDObject = RD, universe = "IBM.N",
    view = "org-info"
  )
})

cat("\n=== Done ===\n")
cat("Fixtures recorded. Run tests with: devtools::test()\n")

# Post-recording validation
cat("\nValidation:\n")

# 1. Check no secrets leaked
cat("  Checking for leaked secrets... ")
leaked <- system2("grep", c(
  "-rl", "applicationid\\|Authorization",
  shQuote(file.path(fixture_dir, "*"))
),
stdout = TRUE, stderr = TRUE
)
if (length(leaked) == 0) {
  cat("OK\n")
} else {
  cat(sprintf("WARNING: found in %d files\n", length(leaked)))
  cat(paste("   ", leaked, collapse = "\n"), "\n")
}

# 2. Check fixture dirs use lh-9000 convention (not localhost-9000)
cat("  Checking host convention (lh-9000)... ")
bad_host <- list.dirs(fixture_dir, recursive = TRUE, full.names = FALSE)
bad_host <- bad_host[grepl("localhost-", bad_host)]
if (length(bad_host) == 0) {
  cat("OK\n")
} else {
  cat(sprintf("WARNING: %d dirs still use 'localhost-'\n", length(bad_host)))
  cat(paste("   ", bad_host, collapse = "\n"), "\n")
  cat("  Check inst/httptest2/redact.R is normalising host correctly.\n")
}
