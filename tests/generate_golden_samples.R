# Generate golden reference samples from current RefinitivR JSON path.
#
# Run this script interactively while a Refinitiv Workspace / Eikon
# terminal is running.  It exercises the JSON connection (no Python)
# and saves results as an RDS file for regression testing.
#
# Usage (from project root, in R):
#   source("tests/generate_golden_samples.R")
#
# Output:
#   tests/testthat/golden/golden_samples_r.rds

library(Refinitiv)

outdir <- file.path("tests", "testthat", "golden")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

cat("Connecting via JSON ...\n")
RD <- RDConnect(PythonModule = "JSON")

golden <- list()

safe_call <- function(label, expr) {
  cat(sprintf("  [%s] ... ", label))
  tryCatch(
    {
      result <- expr
      cat(sprintf("OK (%d rows x %d cols)\n", nrow(result), ncol(result)))
      result
    },
    error = function(e) {
      cat(sprintf("FAILED: %s\n", conditionMessage(e)))
      list(error = conditionMessage(e))
    }
  )
}

# ── rd_GetData ──────────────────────────────────────────────────────────────

cat("\n=== rd_GetData ===\n")

golden$get_data_multi_ric <- safe_call("get_data_multi_ric", {
  rd_GetData(
    RDObject = RD,
    rics = c("AAPL.O", "GOOG.O", "III.L", "MMM"),
    Eikonformulas = c(
      "TR.InstrumentType", "TR.ExchangeName",
      "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive"
    )
  )
})

golden$get_data_single_ric_pricing <- safe_call("get_data_single_ric_pricing", {
  rd_GetData(
    RDObject = RD,
    rics = "AAPL.O",
    Eikonformulas = c("TR.PriceClose.Currency", "CF_CURR", "CF_EXCHNG")
  )
})

golden$get_data_with_params <- safe_call("get_data_with_params", {
  rd_GetData(
    RDObject = RD,
    rics = "AAPL.O",
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

# ── rd_GetHistory (pricing) ────────────────────────────────────────────────

cat("\n=== rd_GetHistory ===\n")

golden$get_history_default_fields <- safe_call("get_history_default_fields", {
  rd_GetHistory(
    RD = RD,
    universe = "AAPL.O",
    start = "2020-01-02",
    end = "2020-01-10",
    interval = "P1D"
  )
})

golden$get_history_multi_ric <- safe_call("get_history_multi_ric", {
  rd_GetHistory(
    RD = RD,
    universe = c("GOOG.O", "MSFT.O"),
    start = "2020-01-02",
    end = "2020-01-10",
    interval = "P1D"
  )
})

# ── rd_GetData (fundamental / revenue) ─────────────────────────────────────

cat("\n=== rd_GetData (fundamental) ===\n")

golden$get_data_revenue <- safe_call("get_data_revenue", {
  rd_GetData(
    RDObject = RD,
    rics = c("GOOG.O", "MSFT.O"),
    Eikonformulas = c("TR.Revenue.date", "TR.Revenue", "TR.GrossProfit"),
    Parameters = list(SDate = 0, EDate = -3, FRQ = "FY", Curn = "USD")
  )
})

# ── Symbology ──────────────────────────────────────────────────────────────

cat("\n=== Symbology ===\n")

golden$symbology_ric_to_isin <- safe_call("symbology_ric_to_isin", {
  rd_GetData(
    RDObject = RD,
    rics = "AAPL.O",
    Eikonformulas = "TR.ISINCode"
  )
})

# ── Historical Pricing ─────────────────────────────────────────────────────

cat("\n=== Historical Pricing ===\n")

golden$historical_pricing_interday <- safe_call("historical_pricing_interday", {
  rd_GetHistoricalPricing(
    RDObject = RD,
    universe = "VOD.L",
    interval = "P1D",
    count = 20L
  )
})

golden$historical_pricing_multi_ric <- safe_call("historical_pricing_multi_ric", {
  rd_GetHistoricalPricing(
    RDObject = RD,
    universe = c("VOD.L", "AAPL.O"),
    interval = "P1D",
    count = 20L
  )
})

# ── Search ─────────────────────────────────────────────────────────────────

cat("\n=== Search ===\n")

golden$search_basic <- safe_call("search_basic", {
  RDPsearch(RDP = RD, query = "AAPL.O", top = 5)
})

# ── News ───────────────────────────────────────────────────────────────────

cat("\n=== News Headlines ===\n")

golden$news_headlines_msft <- safe_call("news_headlines_msft", {
  rd_get_news_headlines(RDObject = RD, query = "R:MSFT.O", limit = 2)
})

# ── Save ───────────────────────────────────────────────────────────────────

outfile <- file.path(outdir, "golden_samples_r.rds")
saveRDS(golden, outfile)
cat(sprintf(
  "\nGolden samples saved to %s\n  %d scenarios recorded\n",
  outfile,
  length(golden)
))
