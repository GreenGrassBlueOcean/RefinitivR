test_that("stops when it has the wrong eikonobject", {

  OldModuleName <- getOption(".RefinitivPyModuleName")
  options(.RefinitivPyModuleName = "wrong object")

    expect_error( rd_GetHistoricalPricing(RDObject = NULL)
                , "historical pricing is only available when JSON --> RefinitivJsonConnect() or Python Refinitiv data --> RDConnect() is used as EikonObject"
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
