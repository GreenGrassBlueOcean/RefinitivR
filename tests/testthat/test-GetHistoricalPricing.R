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
                                      , count = 20L, RDObject = RDConnect()
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


  expected <- list(universe = "character", DATE = "character", TRDPRC_1 = "numeric",
                   MKT_HIGH = "numeric", MKT_LOW = "numeric", ACVOL_UNS = "integer",
                   MKT_OPEN = "numeric", BID = "numeric", ASK = "numeric", TRNOVR_UNS = "numeric",
                   VWAP = "numeric", MID_PRICE = "numeric", PERATIO = "numeric",
                   ORDBK_VOL = "integer", NUM_MOVES = "integer", IND_AUCVOL = "integer",
                   OFFBK_VOL = "integer", HIGH_1 = "numeric", ORDBK_VWAP = "numeric",
                   IND_AUC = "numeric", OPEN_PRC = "numeric", LOW_1 = "numeric",
                   OFF_CLOSE = "numeric", CLS_AUCVOL = "integer", OPN_AUCVOL = "integer",
                   OPN_AUC = "numeric", CLS_AUC = "numeric", TRD_STATUS = "integer",
                   INT_AUC = "numeric", INT_AUCVOL = "integer", EX_VOL_UNS = "integer",
                   ALL_C_MOVE = "integer", ELG_NUMMOV = "integer", NAVALUE = "logical")
  expected <- expected[order(names(expected))]

  actual <- lapply(Vodafone,class)
  actual <- actual[order(names(actual))]

  expect_equal(actual, expected)

  expect_equal(unique(Vodafone$universe), "VOD.L")

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

  for(i in list(intraday, intraday_json)){

    expect_is(i, 'data.frame')
    expect_equal(nrow(i), 40L)

    expect_equal(lapply(i,class)
                 ,list(universe = "character", DATE_TIME = "character", HIGH_1 = "numeric",
                       LOW_1 = "numeric", OPEN_PRC = "numeric", TRDPRC_1 = "numeric",
                       NUM_MOVES = "integer", ACVOL_UNS = "integer", HIGH_YLD = "logical",
                       LOW_YLD = "logical", OPEN_YLD = "logical", YIELD = "logical",
                       BID_HIGH_1 = "numeric", BID_LOW_1 = "numeric", OPEN_BID = "numeric",
                       BID = "numeric", BID_NUMMOV = "integer", ASK_HIGH_1 = "numeric",
                       ASK_LOW_1 = "numeric", OPEN_ASK = "numeric", ASK = "numeric",
                       ASK_NUMMOV = "integer", MID_HIGH = "numeric", MID_LOW = "numeric",
                       MID_OPEN = "numeric", MID_PRICE = "numeric"))



    expect_equal(sort(unique(i$universe)), c("AAPL.O", "VOD.L"))
  }

})


test_that("historical pricing delivers identical interday results", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  fields <- c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS")

  interday <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L, RDObject = RDConnect()
                                     , fields = fields )

  interday_json <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L
                                           , fields = fields
                                           , RDObject = RefinitivJsonConnect())


  interday_reorder <- interday[,match(c("universe", "DATE", fields), colnames(interday))]

  expect_equal(interday_reorder, interday_json)

  for(i in list(intraday, intraday_json)){

  expect_is(i, 'data.frame')
  expect_equal(nrow(i), 40L)

  expect_equal(lapply(i,class)
               ,list(universe = "character", DATE_TIME = "character", HIGH_1 = "numeric",
                     LOW_1 = "numeric", OPEN_PRC = "numeric", TRDPRC_1 = "numeric",
                     NUM_MOVES = "integer", ACVOL_UNS = "integer", HIGH_YLD = "logical",
                     LOW_YLD = "logical", OPEN_YLD = "logical", YIELD = "logical",
                     BID_HIGH_1 = "numeric", BID_LOW_1 = "numeric", OPEN_BID = "numeric",
                     BID = "numeric", BID_NUMMOV = "integer", ASK_HIGH_1 = "numeric",
                     ASK_LOW_1 = "numeric", OPEN_ASK = "numeric", ASK = "numeric",
                     ASK_NUMMOV = "integer", MID_HIGH = "numeric", MID_LOW = "numeric",
                     MID_OPEN = "numeric", MID_PRICE = "numeric"))



  expect_equal(sort(unique(i$universe)), c("AAPL.O", "VOD.L"))
}

})
