test_that("stops when it has the wrong eikonobject", {

  OldModuleName <- getOption(".RefinitivPyModuleName")
  options(.RefinitivPyModuleName = "wrong object")

    expect_error( rd_GetHistoricalPricing(RDObject = NULL)
                , "historical pricing is only available when JSON --> RefinitivJsonConnect() or Python Refinitiv data --> RDConnect() is used as RDObject"
                , fixed = TRUE)

  options(.RefinitivPyModuleName = OldModuleName)
})


test_that("historical pricing delivers identical results", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  Vodafone <- rd_GetHistoricalPricing( universe = "VOD.L", interval = "P1D"
                                      , count = 20L, RDObject = RDConnect(PythonModule = "RD")
                                      , fields = c("universe", "DATE", "TRDPRC_1", "MKT_HIGH", "MKT_LOW", "ACVOL_UNS",
                                          "MKT_OPEN", "BID", "ASK", "TRNOVR_UNS", "VWAP", "MID_PRICE",
                                          "PERATIO", "ORDBK_VOL", "NUM_MOVES", "IND_AUCVOL", "OFFBK_VOL",
                                          "HIGH_1", "ORDBK_VWAP", "IND_AUC", "OPEN_PRC", "LOW_1", "OFF_CLOSE",
                                          "CLS_AUCVOL", "OPN_AUCVOL", "OPN_AUC", "CLS_AUC", "TRD_STATUS",
                                          "INT_AUC", "INT_AUCVOL", "EX_VOL_UNS", "ALL_C_MOVE", "ELG_NUMMOV",
                                          "NAVALUE")
                                      )

  expect_is(Vodafone, 'data.frame')
  expect_equal(nrow(Vodafone), 20L)


  expected <- list(ACVOL_UNS = "integer", ALL_C_MOVE = "integer", ASK = "numeric",
                   BID = "numeric", CLS_AUC = "numeric", CLS_AUCVOL = "integer",
                   Date = "Date", ELG_NUMMOV = "integer", EX_VOL_UNS = "integer",
                   HIGH_1 = "numeric", IND_AUC = "numeric", IND_AUCVOL = "integer",
                   INT_AUC = "numeric", INT_AUCVOL = "integer", LOW_1 = "numeric",
                   MID_PRICE = "numeric", MKT_HIGH = "numeric", MKT_LOW = "numeric",
                   MKT_OPEN = "numeric", NAVALUE = "logical", NUM_MOVES = "integer",
                   OFF_CLOSE = "numeric", OFFBK_VOL = "integer", OPEN_PRC = "numeric",
                   OPN_AUC = "numeric", OPN_AUCVOL = "integer", ORDBK_VOL = "integer",
                   ORDBK_VWAP = "numeric", PERATIO = "numeric", TRD_STATUS = "integer",
                   TRDPRC_1 = "numeric", TRNOVR_UNS = "numeric", Universe = "character",
                   VWAP = "numeric")
  expected <- expected[order(names(expected))]

  actual <- lapply(Vodafone,class)
  actual <- actual[order(names(actual))]

  expect_equal(actual, expected)

  expect_equal(unique(Vodafone$Universe), "VOD.L")

  Vodafone_json <- rd_GetHistoricalPricing( universe = "VOD.L", interval = "P1D"
                                       , count = 20L, RDObject = RefinitivJsonConnect()
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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  intraday <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                     , interval = "PT1M", count = 20L
                                     , sessions= c("pre","normal","post")
                                     , fields = c("HIGH_1", "LOW_1", "OPEN_PRC", "TRDPRC_1", "NUM_MOVES", "ACVOL_UNS",
                                                    "HIGH_YLD", "LOW_YLD", "OPEN_YLD", "YIELD", "BID_HIGH_1", "BID_LOW_1",
                                                    "OPEN_BID", "BID", "BID_NUMMOV", "ASK_HIGH_1", "ASK_LOW_1", "OPEN_ASK",
                                                    "ASK", "ASK_NUMMOV", "MID_HIGH", "MID_LOW", "MID_OPEN", "MID_PRICE")
                                     , RDObject = RDConnect(PythonModule = "RD"))

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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  fields <- c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS")

  interday <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L, RDObject = RDConnect(PythonModule = "RD")
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
