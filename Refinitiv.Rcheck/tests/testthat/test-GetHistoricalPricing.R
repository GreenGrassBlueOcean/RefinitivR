library(testthat)
library(mockery)
library(data.table)

test_that("warns when it has the wrong eikonobject", {
  # With Python support removed, passing NULL as RDObject no longer triggers

  # the old module-type error.  Instead the retry loop exhausts itself and
  # the function returns with a warning about download failures.
  expect_warning(
    rd_GetHistoricalPricing(RDObject = NULL, universe = "AAPL.O"),
    "downloading data failed"
  )
})


test_that("stops when it receives the wrong interval", {

  expect_error( rd_GetHistoricalPricing(universe = "AAPL.O", interval = "daily")
                , "Interval is daily but can only be one of 'PT1M', 'PT5M', 'PT10M', 'PT30M', 'PT60M', 'PT1H', 'P1D', 'P7D', 'P1W', 'P1M', 'P3M', 'P12M', 'P1Y'"
               , fixed = TRUE)
})



test_that("historical pricing delivers identical results", {

  skip_if(!has_live_api(), "No live API available")

  Vodafone <- rd_GetHistoricalPricing( universe = "VOD.L", interval = "P1D"
                                      , count = 500L, RDObject = RDConnect()
                                      , fields = c("universe", "DATE", "TRDPRC_1", "MKT_HIGH", "MKT_LOW", "ACVOL_UNS",
                                          "MKT_OPEN", "BID", "ASK", "TRNOVR_UNS", "VWAP", "MID_PRICE",
                                          "PERATIO", "ORDBK_VOL", "NUM_MOVES", "IND_AUCVOL", "OFFBK_VOL",
                                          "HIGH_1", "ORDBK_VWAP", "IND_AUC", "OPEN_PRC", "LOW_1", "OFF_CLOSE",
                                          "CLS_AUCVOL", "OPN_AUCVOL", "OPN_AUC", "CLS_AUC", "TRD_STATUS",
                                          "INT_AUC", "INT_AUCVOL", "EX_VOL_UNS", "ALL_C_MOVE", "ELG_NUMMOV",
                                          "NAVALUE")
                                      )

  expect_is(Vodafone, 'data.frame')
  expect_equal(nrow(Vodafone), 500)


  # The proxy may return integer-valued fields as either "integer" or "numeric"
  # depending on the Workspace/Eikon version — accept both.
  numeric_cols <- c("ACVOL_UNS", "ALL_C_MOVE", "ASK", "BID", "CLS_AUC", "CLS_AUCVOL",
                    "ELG_NUMMOV", "EX_VOL_UNS", "HIGH_1", "IND_AUC", "IND_AUCVOL",
                    "INT_AUC", "INT_AUCVOL", "LOW_1", "MID_PRICE", "MKT_HIGH", "MKT_LOW",
                    "MKT_OPEN", "NUM_MOVES", "OFF_CLOSE", "OFFBK_VOL", "OPEN_PRC",
                    "OPN_AUC", "OPN_AUCVOL", "ORDBK_VOL", "ORDBK_VWAP", "PERATIO",
                    "TRD_STATUS", "TRDPRC_1", "TRNOVR_UNS", "VWAP")
  for (col in numeric_cols) {
    expect_true(class(Vodafone[[col]]) %in% c("integer", "numeric"),
                info = paste("column", col, "should be integer or numeric, got:", class(Vodafone[[col]])))
  }
  expect_s3_class(Vodafone$Date, "Date")
  expect_type(Vodafone$Universe, "character")
  expect_type(Vodafone$NAVALUE, "logical")

  expect_equal(unique(Vodafone$Universe), "VOD.L")

  Vodafone_json <- rd_GetHistoricalPricing( universe = "VOD.L", interval = "P1D"
                                       , count = 500L, RDObject = RefinitivJsonConnect()
                                       , fields = c("universe", "DATE", "TRDPRC_1", "MKT_HIGH", "MKT_LOW", "ACVOL_UNS",
                                                    "MKT_OPEN", "BID", "ASK", "TRNOVR_UNS", "VWAP", "MID_PRICE",
                                                    "PERATIO", "ORDBK_VOL", "NUM_MOVES", "IND_AUCVOL", "OFFBK_VOL",
                                                    "HIGH_1", "ORDBK_VWAP", "IND_AUC", "OPEN_PRC", "LOW_1", "OFF_CLOSE",
                                                    "CLS_AUCVOL", "OPN_AUCVOL", "OPN_AUC", "CLS_AUC", "TRD_STATUS",
                                                    "INT_AUC", "INT_AUCVOL", "EX_VOL_UNS", "ALL_C_MOVE", "ELG_NUMMOV",
                                                    "NAVALUE")
  )

  expect_equal(Vodafone, Vodafone_json)

})


test_that("historical pricing delivers identical intraday results", {

  skip_if(!has_live_api(), "No live API available")

  intraday <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                     , interval = "PT1M", count = 20L
                                     , sessions= c("pre","normal","post")
                                     , fields = c("HIGH_1", "LOW_1", "OPEN_PRC", "TRDPRC_1", "NUM_MOVES", "ACVOL_UNS",
                                                    "HIGH_YLD", "LOW_YLD", "OPEN_YLD", "YIELD", "BID_HIGH_1", "BID_LOW_1",
                                                    "OPEN_BID", "BID", "BID_NUMMOV", "ASK_HIGH_1", "ASK_LOW_1", "OPEN_ASK",
                                                    "ASK", "ASK_NUMMOV", "MID_HIGH", "MID_LOW", "MID_OPEN", "MID_PRICE")
                                     , RDObject = RDConnect())

  intraday_json <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                       , interval = "PT1M", count = 20L
                                       , sessions= c("pre","normal","post")
                                       , RDObject = RefinitivJsonConnect()
                                       , fields = c("HIGH_1", "LOW_1", "OPEN_PRC", "TRDPRC_1", "NUM_MOVES", "ACVOL_UNS",
                                                    "HIGH_YLD", "LOW_YLD", "OPEN_YLD", "YIELD", "BID_HIGH_1", "BID_LOW_1",
                                                    "OPEN_BID", "BID", "BID_NUMMOV", "ASK_HIGH_1", "ASK_LOW_1", "OPEN_ASK",
                                                    "ASK", "ASK_NUMMOV", "MID_HIGH", "MID_LOW", "MID_OPEN", "MID_PRICE")
                                       )

  #expect_equal(intraday, intraday_json) # can never be the same so not tested
  expected <- list(Universe = "character", DATE_TIME = "character", HIGH_1 = "numeric",
                 LOW_1 = "numeric", OPEN_PRC = "numeric", TRDPRC_1 = "numeric",
                 NUM_MOVES = "integer", ACVOL_UNS = "integer", HIGH_YLD = "logical",
                 LOW_YLD = "logical", OPEN_YLD = "logical", YIELD = "logical",
                 BID_HIGH_1 = "numeric", BID_LOW_1 = "numeric", OPEN_BID = "numeric",
                 BID = "numeric", BID_NUMMOV = "integer", ASK_HIGH_1 = "numeric",
                 ASK_LOW_1 = "numeric", OPEN_ASK = "numeric", ASK = "numeric",
                 ASK_NUMMOV = "integer", MID_HIGH = "numeric", MID_LOW = "numeric",
                 MID_OPEN = "numeric", MID_PRICE = "numeric")
  expected <- expected[order(names(expected))]


  for(i in list(intraday, intraday_json)){

    expect_is(i, 'data.frame')
    expect_equal(nrow(i), 40L)

    actual <- lapply(i,class)
    actual <- actual[order(names(actual))]

    expect_equal(names(actual),names(expected))
    expect_equal(sort(unique(i$Universe)), c("AAPL.O", "VOD.L"))
  }

})


test_that("historical pricing delivers identical interday results", {

  skip_if(!has_live_api(), "No live API available")

  fields <- c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS")

  interday <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L, RDObject = RDConnect()
                                     , fields = fields )

  interday_json <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L
                                           , fields = fields
                                           , RDObject = RefinitivJsonConnect())


  interday_reorder <- interday[,match(c("Universe", "Date", fields), colnames(interday))]

  expect_equal(interday_reorder, interday_json)

  expected <- c("Universe", "Date",  fields)
  expected <- expected[order(expected)]

  for(i in list(interday, interday_json)){

  expect_is(i, 'data.frame')
  expect_equal(nrow(i), 40L)

  actual <- names(i)
  actual <- actual[order(actual)]

  expect_equal(expected, actual)
  expect_equal(sort(unique(i$Universe)), c("AAPL.O", "VOD.L"))
}

})

test_that("historical pricing delivers identical results for non exisiting ric", {

  skip_if(!has_live_api(), "No live API available")

  fields <- c("BID","ASK")

  NotExistingOption_python <- rd_GetHistoricalPricing(universe = c("AAPLA212216000.U^22")
                                     , interval = "P1D", count = 20L, RDObject = RDConnect()
                                     , fields = fields ) |> suppressWarnings()



  NotExistingOption_json <- rd_GetHistoricalPricing(universe = c("AAPLA212216000.U^22")
                                               , interval = "P1D", count = 20L, RDObject = RDConnect()
                                               , fields = fields ) |> suppressWarnings()

  expect_equal(NotExistingOption_python, NotExistingOption_json)

})



#####
# Stub test for rd_GetHistoricalPricing (JSON branch)
#####
test_that("rd_GetHistoricalPricing (JSON branch) processes stubbed response correctly", {
  # Force JSON branch.
  withr::local_options(.RefinitivPyModuleName = "JSON")

  # Stub CheckifCustomInstrument to always return FALSE.
  stub(rd_GetHistoricalPricing, "CheckifCustomInstrument", function(x, ...) FALSE)

  # Create a dummy RD object with a stubbed get_historical_pricing method.
  dummy_RD <- list()
  dummy_request <- list(
    get_data = function() {
      # Simulate that get_data() returns a list with data$raw containing our JSON.
      list(data = list(raw = '{"Date":["2020-01-01","2020-01-02"], "Universe":["AAPL.O","AAPL.O"], "BID":[100,101]}'))
    }
  )
  dummy_RD$get_historical_pricing <- function(universe, interval, start, end, adjustments, count, fields, sessions) {
    list(dummy_request)
  }

  # Stub rd_OutputProcesser to convert our dummy response into a fixed data frame.
  stub(rd_GetHistoricalPricing, "rd_OutputProcesser", function(x, use_field_names_in_headers) {
    data.frame(
      Date = as.Date(c("2020-01-01", "2020-01-02")),
      Universe = rep("AAPL.O", 2),
      BID = c(100, 101),
      stringsAsFactors = FALSE
    )
  })

  # Call rd_GetHistoricalPricing with our dummy RD object.
  res <- rd_GetHistoricalPricing(
    RDObject = dummy_RD,
    universe = "AAPL.O",
    interval = "P1D",
    count = 2L,
    fields = "BID",
    debug = TRUE
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_equal(unique(res$Universe), "AAPL.O")
  expect_equal(res$BID, c(100, 101))

  # withr::local_options() auto-restores .RefinitivPyModuleName at end of test.
})

# NOTE: The "refinitiv.data branch" stub test was removed in 0.2.0.
# It tested a Python/reticulate code path (setting .RefinitivPyModuleName = "refinitiv.data"
# and using $content$historical_pricing) that no longer exists. The JSON path is now the
# only path — the option is ignored and the dummy Python-style object is never traversed.

dump_refinitiv_options("test-GetHistoricalPricing")
