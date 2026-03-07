# Coverage tests for rd_GetHistory.R
# Targets missed lines: cache paths (126, 131-134, 346),
# GetData-only branch (315), NAVALUE handling (283-284),
# Universe rename (288-291), custom instruments (199),
# .check_date_granularity early return (363)
library(testthat)
library(mockery)
library(data.table)

# ── Cache lookup, hit, and store (lines 126, 131-134, 345-346) ──────────────

test_that("rd_GetHistory cache store and hit work", {
  withr::local_options(
    refinitiv_cache = TRUE,
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = "BID"
  )
  rd_ClearCache()

  dummy_RD <- list()
  call_count <- 0L
  dummy_hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) {
    call_count <<- call_count + 1L
    data.table::copy(dummy_hp)
  })

  # First call: cache miss → stores result
  res1 <- rd_GetHistory(
    RD = dummy_RD, universe = "AAPL.O", fields = "BID",
    start = "2020-01-01", end = "2020-01-03", cache = TRUE
  )
  expect_equal(call_count, 1L)
  expect_true(is.data.frame(res1))

  # Second call: cache hit → no extra API call
  res2 <- rd_GetHistory(
    RD = dummy_RD, universe = "AAPL.O", fields = "BID",
    start = "2020-01-01", end = "2020-01-03", cache = TRUE
  )
  expect_equal(call_count, 1L)
  expect_equal(res1, res2)

  rd_ClearCache()
})

test_that("rd_GetHistory cache hit emits debug message", {
  withr::local_options(
    refinitiv_cache = TRUE,
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = "BID"
  )
  rd_ClearCache()

  dummy_RD <- list()
  dummy_hp <- data.table(
    Date = as.Date("2020-01-01"),
    Instrument = "AAPL.O",
    BID = 100
  )

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) data.table::copy(dummy_hp))

  # Prime the cache
  rd_GetHistory(
    RD = dummy_RD, universe = "AAPL.O", fields = "BID",
    start = "2020-01-01", end = "2020-01-01", cache = TRUE
  )

  # Cache hit with debug = TRUE should emit message
  msgs <- capture_messages(
    rd_GetHistory(
      RD = dummy_RD, universe = "AAPL.O", fields = "BID",
      start = "2020-01-01", end = "2020-01-01", cache = TRUE, debug = TRUE
    )
  )
  expect_true(any(grepl("Cache hit", msgs)))

  rd_ClearCache()
})

# ── GetData-only path: fundamental fields only (line 315) ────────────────────

test_that("rd_GetHistory handles fundamental-only fields (no HistoricalPricing)", {
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = c("BID", "ASK", "TRDPRC_1")
  )

  dummy_RD <- list()
  dummy_gd <- data.table(
    Date = as.Date(c("2020-03-31", "2020-06-30")),
    Instrument = rep("AAPL.O", 2),
    TR.REVENUE = c(1000, 1100)
  )

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetData", function(...) data.table::copy(dummy_gd))

  # Only TR.Revenue → all fields go to GetData, none to HistoricalPricing
  res <- rd_GetHistory(
    RD = dummy_RD, universe = "AAPL.O", fields = "TR.Revenue",
    start = "2020-01-01", end = "2020-12-31"
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2L)
  expect_null(attr(res, "merge_info"))
})

# ── NAVALUE handling (lines 283-284) ─────────────────────────────────────────

test_that("rd_GetHistory converts NAVALUE FALSE to NA", {
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = c("BID", "NAVALUE")
  )

  dummy_RD <- list()
  dummy_hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102),
    NAVALUE = c(TRUE, FALSE, TRUE)
  )

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) data.table::copy(dummy_hp))

  res <- rd_GetHistory(
    RD = dummy_RD, universe = "AAPL.O",
    fields = c("BID", "NAVALUE"),
    start = "2020-01-01", end = "2020-01-03"
  )

  expect_true("NAVALUE" %in% names(res))
  # FALSE values should have been converted to NA
  expect_true(is.na(res$NAVALUE[2]))
  # TRUE values stay TRUE
  expect_true(res$NAVALUE[1])
  expect_true(res$NAVALUE[3])
})

# ── Custom instrument check (line 199) ───────────────────────────────────────

test_that("rd_GetHistory errors for custom instruments", {
  withr::local_options(.RefinitivUUID = "TEST-UUID-123")

  expect_error(
    rd_GetHistory(
      RD = list(), universe = "S)myInstrument.TEST-UUID-123",
      fields = "BID", start = "2020-01-01", end = "2020-01-03"
    ),
    "Custom Instruments are currently not supported"
  )
})

# ── .check_date_granularity early return for disjoint instruments (line 363) ─

test_that("rd_GetHistory merge with disjoint instruments triggers granularity early return", {
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = "BID"
  )

  dummy_RD <- list()
  # Pricing returns AAPL.O, fundamentals return MSFT.O — no overlap
  dummy_hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )
  dummy_gd <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("MSFT.O", 3),
    ASK = c(200, 201, 202)
  )

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetData", function(...) data.table::copy(dummy_gd))
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) data.table::copy(dummy_hp))

  # No shared instruments → .check_date_granularity returns early, no warning
  expect_no_warning(
    res <- rd_GetHistory(
      RD = dummy_RD, universe = c("AAPL.O", "MSFT.O"),
      fields = c("BID", "ASK"),
      start = "2020-01-01", end = "2020-01-03"
    )
  )
  expect_true(is.data.frame(res))
  # Both instruments present from the full outer join
  expect_true(all(c("AAPL.O", "MSFT.O") %in% res$Instrument))
})

# ── Universe → Instrument rename (lines 288-291) ────────────────────────────

test_that("rd_GetHistory renames Universe to Instrument", {
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = "BID"
  )

  dummy_RD <- list()
  # Simulate rd_GetHistoricalPricing returning "Universe" instead of "Instrument"
  dummy_hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Universe = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )

  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) data.table::copy(dummy_hp))

  res <- rd_GetHistory(
    RD = dummy_RD, universe = "AAPL.O", fields = "BID",
    start = "2020-01-01", end = "2020-01-03"
  )

  expect_true("Instrument" %in% names(res))
  expect_false("Universe" %in% names(res))
  expect_equal(unique(res$Instrument), "AAPL.O")
})
