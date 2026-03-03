library(testthat)
library(httptest2)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
local_refinitiv_state(teardown_env())

# =============================================================================
# httptest2 Integration Tests for RefinitivR
#
# These tests use pre-recorded HTTP fixtures to test API functions offline.
# Fixtures are stored in subdirectories of tests/testthat/.
#
# To re-record fixtures, run tests/record_fixtures.R with a live terminal.
# =============================================================================

# Helper: set up mock environment
mock_env <- function() {
  options(
    .EikonApiKey    = "dummy_key",
    eikon_port      = 9000L,
    refinitiv_base_url = "http://localhost",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI   = "JSON"
  )
  RefinitivJsonConnect("dummy_key")
}

# Helper: check if a fixture directory has the expected file structure
fixture_ready <- function(dir_name) {
  dir_path <- test_path(dir_name)
  dir.exists(dir_path) && length(list.files(dir_path, recursive = TRUE)) > 0
}

# =============================================================================
# CheckTerminalType
# =============================================================================

test_that("CheckTerminalType returns terminal info from fixture", {
  skip_if(!fixture_ready("chk-terminal"), "Fixture not available")
  options(eikon_port = 9000L)

  with_mock_dir("chk-terminal", {
    result <- CheckTerminalType()
  })

  # CheckTerminalType sets options and returns invisibly
  expect_equal(getOption("eikon_port"), 9000)
})

# =============================================================================
# rd_GetData
# =============================================================================

test_that("rd_GetData works with single RIC from fixture", {
  skip_if(!fixture_ready("getdata-1ric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("getdata-1ric", {
    result <- rd_GetData(RDObject = RD, rics = "AAPL.O",
                         Eikonformulas = c("TR.COMPANYNAME", "TR.INSTRUMENTTYPE",
                                           "TR.EXCHANGENAME", "TR.INSTRUMENTISACTIVE"))
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("Instrument" %in% names(result))
})

test_that("rd_GetData works with multiple RICs from fixture", {
  skip_if(!fixture_ready("getdata-nric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("getdata-nric", {
    result <- rd_GetData(RDObject = RD,
                         rics = c("AAPL.O", "GOOG.O", "III.L", "MMM"),
                         Eikonformulas = c("TR.InstrumentType", "TR.ExchangeName",
                                           "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive"))
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})

test_that("rd_GetData works with parameters from fixture", {
  skip_if(!fixture_ready("getdata-params"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("getdata-params", {
    result <- rd_GetData(RDObject = RD, rics = "AAPL.O",
                         Eikonformulas = c("TR.CAEffectiveDate", "TR.CAAdjustmentFactor",
                                           "TR.CAAdjustmentType"),
                         Parameters = list(CAEventType = "SSP",
                                           SDate = "2020-10-27", EDate = "2020-12-01"))
  })

  expect_s3_class(result, "data.frame")
})

test_that("rd_GetData handles wrong RIC from fixture", {
  skip_if(!fixture_ready("getdata-bad"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("getdata-bad", {
    result <- suppressWarnings(
      rd_GetData(RDObject = RD, rics = "WRONGRIC",
                 Eikonformulas = "TR.COMPANYNAME"),
      classes = "simpleWarning"
    )
  })

  expect_s3_class(result, "data.frame")
})

# =============================================================================
# rd_GetHistoricalPricing
# =============================================================================

test_that("rd_GetHistoricalPricing interday from fixture", {
  skip_if(!fixture_ready("histpx-1d"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("histpx-1d", {
    result <- rd_GetHistoricalPricing(RDObject = RD, universe = "VOD.L",
                                       interval = "P1D", count = 20L)
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_GetHistoricalPricing multi-RIC from fixture", {
  skip_if(!fixture_ready("histpx-nric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("histpx-nric", {
    result <- rd_GetHistoricalPricing(RDObject = RD,
                                       universe = c("VOD.L", "AAPL.O"),
                                       interval = "P1D", count = 20L)
  })

  expect_s3_class(result, "data.frame")
  # Column is named "Universe" (capital U) in the API response
  universe_col <- intersect(c("Universe", "universe", "Security"), names(result))[1]
  expect_true(all(c("VOD.L", "AAPL.O") %in% result[[universe_col]]))
})

# =============================================================================
# rd_GetHistory
# =============================================================================

test_that("rd_GetHistory pricing from fixture", {
  skip_if(!fixture_ready("hist-px"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("hist-px", {
    result <- rd_GetHistory(RD = RD, universe = "AAPL.O",
                            start = "2020-01-02", end = "2020-01-10",
                            interval = "P1D")
  })

  expect_s3_class(result, "data.frame")
  expect_true("Date" %in% names(result) || "DATE" %in% names(result))
})

test_that("rd_GetHistory multi-RIC from fixture", {
  skip_if(!fixture_ready("hist-nric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("hist-nric", {
    result <- rd_GetHistory(RD = RD,
                            universe = c("GOOG.O", "MSFT.O"),
                            start = "2020-01-02", end = "2020-01-10",
                            interval = "P1D")
  })

  expect_s3_class(result, "data.frame")
})

test_that("rd_GetHistory fundamental from fixture", {
  skip_if(!fixture_ready("hist-fund"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("hist-fund", {
    result <- suppressWarnings(
      rd_GetHistory(RD = RD,
                    universe = c("GOOG.O", "MSFT.O"),
                    fields = c("TR.Revenue", "TR.GrossProfit"),
                    parameters = list(SDate = 0, EDate = -3,
                                      FRQ = "FY", Curn = "USD")),
      classes = "simpleWarning"
    )
  })

  expect_s3_class(result, "data.frame")
})

# =============================================================================
# EikonGetData
# =============================================================================

test_that("EikonGetData single RIC from fixture", {
  skip_if(!fixture_ready("ekdata-1ric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("ekdata-1ric", {
    result <- EikonGetData(EikonObject = RD, rics = "MMM",
                           Eikonformulas = c("TR.CompanyName", "CF_EXCHNG",
                                             "RDN_EXCHD2", "CURRENCY"))
  })

  expect_type(result, "list")
  expect_s3_class(result$PostProcessedEikonGetData, "data.frame")
})

test_that("EikonGetData multi RIC from fixture", {
  skip_if(!fixture_ready("ekdata-nric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("ekdata-nric", {
    result <- EikonGetData(EikonObject = RD, rics = c("MMM", "III.L"),
                           Eikonformulas = c("TR.CompanyName", "CF_EXCHNG",
                                             "RDN_EXCHD2", "CURRENCY"))
  })

  expect_type(result, "list")
  expect_equal(nrow(result$PostProcessedEikonGetData), 2)
})

# =============================================================================
# EikonGetTimeseries
# =============================================================================

test_that("EikonGetTimeseries daily from fixture", {
  skip_if(!fixture_ready("ekts-daily"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("ekts-daily", {
    result <- EikonGetTimeseries(EikonObject = RD, rics = "MMM",
                                 start_date = "2020-01-01T01:00:00",
                                 end_date = "2020-01-10T01:00:00",
                                 corax = "unadjusted")
  })

  expect_s3_class(result, "data.frame")
  expect_true("CLOSE" %in% names(result))
})

test_that("EikonGetTimeseries multi-RIC from fixture", {
  skip_if(!fixture_ready("ekts-nric"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("ekts-nric", {
    result <- EikonGetTimeseries(EikonObject = RD, rics = c("MMM", "III.L"),
                                 start_date = "2020-01-01T01:00:00",
                                 end_date = "2020-01-10T01:00:00",
                                 corax = "unadjusted")
  })

  expect_s3_class(result, "data.frame")
  expect_true(all(c("MMM", "III.L") %in% result$Security))
})

# =============================================================================
# EikonGetSymbology
# =============================================================================

test_that("EikonGetSymbology RIC to ISIN from fixture", {
  skip_if(!fixture_ready("eksym-r2i"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("eksym-r2i", {
    result <- EikonGetSymbology(EikonObject = RD,
                                symbol = c("AAPL.O", "MSFT"),
                                from_symbol_type = "RIC",
                                to_symbol_type = "ISIN")
  })

  expect_s3_class(result, "data.frame")
})

test_that("EikonGetSymbology ISIN to RIC from fixture", {
  skip_if(!fixture_ready("eksym-i2r"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("eksym-i2r", {
    result <- EikonGetSymbology(EikonObject = RD,
                                symbol = c("US0378331005", "GB00B03MLX29"),
                                from_symbol_type = "ISIN",
                                to_symbol_type = "RIC")
  })

  expect_s3_class(result, "data.frame")
})

# =============================================================================
# RDPsearch
# =============================================================================

test_that("RDPsearch basic from fixture", {
  skip_if(!fixture_ready("search-basic"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("search-basic", {
    result <- RDPsearch(RDP = RD, query = "AAPL.O", top = 5)
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("RDPsearch people view from fixture", {
  skip_if(!fixture_ready("search-people"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("search-people", {
    result <- RDPsearch(RDP = RD, query = "president", view = "People", top = 5)
  })

  expect_s3_class(result, "data.frame")
})

# =============================================================================
# RDPget_search_metadata
# =============================================================================

test_that("RDPget_search_metadata equity from fixture", {
  skip_if(!fixture_ready("searchmeta-eq"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("searchmeta-eq", {
    result <- RDPget_search_metadata(RDP = RD, searchView = "EquityQuotes")
  })

  expect_s3_class(result, "data.frame")
})

# =============================================================================
# News
# =============================================================================

test_that("rd_get_news_headlines MSFT from fixture", {
  skip_if(!fixture_ready("news-hl-msft"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("news-hl-msft", {
    result <- rd_get_news_headlines(RDObject = RD, query = "R:MSFT.O", limit = 2L)
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_get_news_headlines AAPL from fixture", {
  skip_if(!fixture_ready("news-hl-aapl"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("news-hl-aapl", {
    result <- rd_get_news_headlines(RDObject = RD, query = "R:AAPL.O", limit = 2L)
  })

  expect_s3_class(result, "data.frame")
})

test_that("rd_get_news_story from fixture", {
  skip_if(!fixture_ready("news-story"), "Fixture not available")
  RD <- mock_env()

  # The fixture records both the headlines lookup and the story fetch, so we
  # replay the same two-step sequence to obtain a valid story ID.
  with_mock_dir("news-story", {
    headlines <- rd_get_news_headlines(RDObject = RD, query = "R:MSFT.O", limit = 1L)
    result    <- rd_get_news_story(RDObject = RD, story_id = headlines$storyId[1])
  })

  expect_type(result, "list")
})

test_that("rd_get_top_news from fixture", {
  skip_if(!fixture_ready("news-top"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("news-top", {
    result <- rd_get_top_news(RDObject = RD)
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# =============================================================================
# rd_handshake
# =============================================================================

test_that("rd_handshake from fixture", {
  skip_if(!fixture_ready("handshake"), "Fixture not available")
  options(eikon_port = 9000L)

  with_mock_dir("handshake", {
    rd_handshake(force = TRUE, debug = FALSE)
  })

  expect_true(!is.null(refinitiv_vault_get("access_token")))
})

# =============================================================================
# ESG
# =============================================================================

test_that("rd_GetESG basic from fixture", {
  skip_if(!fixture_ready("esg-basic"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("esg-basic", {
    result <- rd_GetESG(RDObject = RD, universe = "AAPL.O", view = "basic")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_GetESG scores-full from fixture", {
  skip_if(!fixture_ready("esg-scores"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("esg-scores", {
    result <- rd_GetESG(RDObject = RD,
                        universe = c("AAPL.O", "MSFT.O"),
                        view = "scores-full")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# =============================================================================
# Estimates
# =============================================================================

test_that("rd_GetEstimates summary annual from fixture", {
  skip_if(!fixture_ready("est-summary"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("est-summary", {
    result <- rd_GetEstimates(RDObject = RD, universe = "AAPL.O",
                              view = "view-summary/annual", package = "basic")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_GetEstimates actuals annual from fixture", {
  skip_if(!fixture_ready("est-actuals"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("est-actuals", {
    result <- rd_GetEstimates(RDObject = RD, universe = "AAPL.O",
                              view = "view-actuals/annual", package = "basic")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_GetEstimates KPI from fixture", {
  skip_if(!fixture_ready("est-kpi"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("est-kpi", {
    result <- rd_GetEstimates(RDObject = RD, universe = "AAPL.O",
                              view = "view-actuals-kpi/annual")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# =============================================================================
# Ownership
# =============================================================================

test_that("rd_GetOwnership consolidated breakdown from fixture", {
  skip_if(!fixture_ready("own-consol"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("own-consol", {
    result <- rd_GetOwnership(RDObject = RD, universe = "AAPL.O",
                              view = "consolidated/breakdown", stat_type = 1)
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_GetOwnership fund investors from fixture", {
  skip_if(!fixture_ready("own-fund"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("own-fund", {
    result <- rd_GetOwnership(RDObject = RD, universe = "AAPL.O",
                              view = "fund/investors", limit = 10)
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("rd_GetOwnership org-info from fixture", {
  skip_if(!fixture_ready("own-org"), "Fixture not available")
  RD <- mock_env()

  with_mock_dir("own-org", {
    result <- rd_GetOwnership(RDObject = RD, universe = "IBM.N",
                              view = "org-info")
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

dump_refinitiv_options("test-zzz-httptest2-integration")
