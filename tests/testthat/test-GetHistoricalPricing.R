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
                                      , count = 20L, RDObject = RDConnect())

  expect_is(Vodafone, 'data.frame')
  expect_equal(nrow(Vodafone), 20L)

  expect_equal(lapply(Vodafone,class)
              , list(universe = "character", DATE = "Date", TRDPRC_1 = "numeric",
                     MKT_HIGH = "numeric", MKT_LOW = "numeric", ACVOL_UNS = "numeric",
                     MKT_OPEN = "numeric", BID = "numeric", ASK = "numeric",
                     TRNOVR_UNS = "numeric", VWAP = "numeric", MID_PRICE = "numeric",
                     PERATIO = "numeric", ORDBK_VOL = "numeric", NUM_MOVES = "numeric",
                     IND_AUCVOL = "numeric", OFFBK_VOL = "numeric", HIGH_1 = "numeric",
                     ORDBK_VWAP = "numeric", IND_AUC = "numeric", OPEN_PRC = "numeric",
                     LOW_1 = "numeric", OFF_CLOSE = "numeric", CLS_AUCVOL = "numeric",
                     OPN_AUCVOL = "numeric", OPN_AUC = "numeric", CLS_AUC = "numeric",
                     TRD_STATUS = "numeric", INT_AUC = "numeric", INT_AUCVOL = "numeric",
                     EX_VOL_UNS = "numeric", ALL_C_MOVE = "numeric", ELG_NUMMOV = "numeric",
                     NAVALUE = "numeric"))



  expect_equal(unique(Vodafone$universe), "VOD.L")

  Vodafone_json <- rd_GetHistoricalPricing( universe = "VOD.L", interval = "P1D"
                                       , count = 20L, RDObject = RefinitivJsonConnect())

  expect_equal(Vodafone, Vodafone_json)

})


test_that("historical pricing delivers identical intraday results", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  intraday <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                     , interval = "PT1M", count = 20L
                                     , sessions= c("pre","normal","post")
                                     , RDObject = RDConnect())

  intraday_json <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                       , interval = "PT1M", count = 20L
                                       , sessions= c("pre","normal","post")
                                       , RDObject = RefinitivJsonConnect())

  expect_equal(intraday, intraday_json)

  expect_is(intraday, 'data.frame')
  expect_equal(nrow(intraday), 40L)

  expect_equal(lapply(intraday,class)
               ,list(universe = "character", DATE_TIME = "character", HIGH_1 = "numeric",
                     LOW_1 = "numeric", OPEN_PRC = "numeric", TRDPRC_1 = "numeric",
                     NUM_MOVES = "integer", ACVOL_UNS = "integer", HIGH_YLD = "logical",
                     LOW_YLD = "logical", OPEN_YLD = "logical", YIELD = "logical",
                     BID_HIGH_1 = "numeric", BID_LOW_1 = "numeric", OPEN_BID = "numeric",
                     BID = "numeric", BID_NUMMOV = "integer", ASK_HIGH_1 = "numeric",
                     ASK_LOW_1 = "numeric", OPEN_ASK = "numeric", ASK = "numeric",
                     ASK_NUMMOV = "integer", MID_HIGH = "numeric", MID_LOW = "numeric",
                     MID_OPEN = "numeric", MID_PRICE = "numeric"))



  expect_equal(sort(unique(intraday$universe)), c("AAPL.O", "VOD.L"))

})


test_that("historical pricing delivers identical interday results", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))


  interday <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L
                                     , fields =c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS") )

  interday_json <- rd_GetHistoricalPricing(universe = c("VOD.L", "AAPL.O"), interval = "P1D", count = 20L
                                           , fields =c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS")
                                           , RDObject = RefinitivJsonConnect())


  expect_equal(interday, interday_json)

  expect_is(interday, 'data.frame')
  expect_equal(nrow(interday), 40L)

  expect_equal(lapply(interday,class)
               ,list(universe = "character", DATE_TIME = "character", HIGH_1 = "numeric",
                     LOW_1 = "numeric", OPEN_PRC = "numeric", TRDPRC_1 = "numeric",
                     NUM_MOVES = "integer", ACVOL_UNS = "integer", HIGH_YLD = "logical",
                     LOW_YLD = "logical", OPEN_YLD = "logical", YIELD = "logical",
                     BID_HIGH_1 = "numeric", BID_LOW_1 = "numeric", OPEN_BID = "numeric",
                     BID = "numeric", BID_NUMMOV = "integer", ASK_HIGH_1 = "numeric",
                     ASK_LOW_1 = "numeric", OPEN_ASK = "numeric", ASK = "numeric",
                     ASK_NUMMOV = "integer", MID_HIGH = "numeric", MID_LOW = "numeric",
                     MID_OPEN = "numeric", MID_PRICE = "numeric"))



  expect_equal(sort(unique(interday$universe)), c("AAPL.O", "VOD.L"))

})
