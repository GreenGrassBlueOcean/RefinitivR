test_that("rd_GetHistory fails when it should", {

  expect_error(rd_GetHistory(RD = NULL, universe="GOOG.O",fields = c("BID", "ASK"),interval="tick",count=-1)
              , "count should be integer > 0")

  expect_error(rd_GetHistory(RD = NULL, fields = c("BID", "ASK"))
               , "Parameter universe should be supplied and is not")

})


test_that("rd_GetHistory can handle most simple request", {

  TestRD <- check_Eikonapi(ExecutionMode = "RD")

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  test <- rd_GetHistory(RD = TestRD,  universe="AAPL.O")

  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
              , list(Date = c("POSIXct", "POSIXt"), Instrument = "character"
                    , variable = "character", value = "numeric")
  )

  expect_equal(unique(test$Instrument), "AAPL.O")

})

test_that("rd_GetHistory can handle request with simple fields", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  TestRD <- check_Eikonapi(ExecutionMode = "RD")

  test <- try(rd_GetHistory(RD = TestRD, universe = c("GOOG.O","MSFT.O"),fields = c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
                        ,parameters = list("Scale" = 6,"SDate" = 0,"EDate" = -3,"FRQ" = "FY", "Curn" = "EUR")))

  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class),
               list(Date = c("POSIXct", "POSIXt"), Instrument = "character",
                    variable = "character", value = "character"))


  expect_equal(sort(unique(test$Instrument)), c("GOOG.O", "MSFT.O"))
  expect_equal(sort(unique(test$variable)), c("Date", "Gross Profit", "Revenue"))
})

test_that("rd_GetHistory can handle request with explicit date", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory( universe = c("GOOG.O","MSFT.O")
                       , start = "2020-01-01T01:00:00"
                       , end = "2020-01-10T01:00:00"
                       ))

  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               , list(Date = c("POSIXct", "POSIXt"), Instrument = "character",
                      variable = "character", value = "numeric")
               )

  expect_equal(sort(unique(test$Instrument)), c("GOOG.O", "MSFT.O"))
  expect_equal(sort(unique(test$Date))
              , structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600,
                            1578528000, 1578614400), class = c("POSIXct", "POSIXt"), tzone = "UTC"))
})


test_that("rd_GetHistory can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory(universe= "AAPL.O"
                      , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
                                  ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")
                      , parameters = list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01")))


  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               ,list(Date = c("POSIXct", "POSIXt"), Instrument = "character"
                     , variable = "character",value = "numeric")
  )


  expect_equal(sort(unique(test$Date))
               , structure(c(1601510400, 1603756800, 1603843200, 1603929600, 1604016000,
                             1604188800, 1604275200, 1604361600, 1604448000, 1604534400, 1604620800,
                             1604880000, 1604966400, 1605052800, 1605139200, 1605225600, 1605484800,
                             1605571200, 1605657600, 1605744000, 1605830400, 1606089600, 1606176000,
                             1606262400, 1606435200, 1606694400, 1606780800),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"))

  expect_equal(sort(unique(test$Instrument)), c("AAPL.O"))

})


test_that("rd_GetHistory can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory(universe= "AAPL.O"
                        , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
                                     ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")
                        , parameters = list("Curn" = "USD")
                        , start = "2020-10-27"
                        , end = "2020-12-01"
                        ))


  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               ,list(Date = c("POSIXct", "POSIXt"), Instrument = "character"
                     , variable = "character",value = "numeric")
  )


  expect_equal(sort(unique(test$Date))
               ,structure(c(1601510400, 1603756800, 1603843200, 1603929600, 1604016000,
                            1604188800, 1604275200, 1604361600, 1604448000, 1604534400, 1604620800,
                            1604880000, 1604966400, 1605052800, 1605139200, 1605225600, 1605484800,
                            1605571200, 1605657600, 1605744000, 1605830400, 1606089600, 1606176000,
                            1606262400, 1606435200, 1606694400, 1606780800)
                          , class = c("POSIXct", "POSIXt"), tzone = "UTC")
               )

  expect_equal(sort(unique(test$Instrument)), c("AAPL.O"))

})


test_that("rd_GetHistory can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  # Refinitiv::RDConnect()

  DownloadRics <- c("AALB.AS", "ABNd.AS", "AEGN.AS", "AD.AS", "AKZO.AS", "ATCA.AS^A21",
                    "MT.AS", "ASML.AS", "BOSN.AS^K22", "DSMN.AS^E23", "GLPG.AS",
                    "GTO.AS^E19", "HEIN.AS", "INGA.AS", "KPN.AS", "NN.AS", "PHG.AS",
                    "RAND.AS", "RELN.AS^I18", "SHEL.AS", "SBMO.AS", "URW.PA", "UNc.AS^G19",
                    "VOPA.AS", "WLSNc.AS")

  adjustments <- NULL

  end_date <- structure(17563, class = "Date")
  start_date <- structure(17527, class = "Date")
  parameters <- list()
  Eikonformulas <- "TR.DividendYield"

  
  GetData_StockData <- Refinitiv::rd_GetHistory( universe = DownloadRics
                                                 , fields = Eikonformulas
                                                 , parameters = parameters
                                                 , start = start_date
                                                 , end = end_date
                                                 , adjustments = adjustments
  )










  test <- try(rd_GetHistory(universe= "AAPL.O"
                            , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
                                         ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")
                            , parameters = list("Curn" = "USD")
                            , start = "2020-10-27"
                            , end = "2020-12-01"
  ))


  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               ,list(Date = c("POSIXct", "POSIXt"), Instrument = "character"
                     , variable = "character",value = "numeric")
  )


  expect_equal(sort(unique(test$Date))
               ,structure(c(1601510400, 1603756800, 1603843200, 1603929600, 1604016000,
                            1604188800, 1604275200, 1604361600, 1604448000, 1604534400, 1604620800,
                            1604880000, 1604966400, 1605052800, 1605139200, 1605225600, 1605484800,
                            1605571200, 1605657600, 1605744000, 1605830400, 1606089600, 1606176000,
                            1606262400, 1606435200, 1606694400, 1606780800)
                          , class = c("POSIXct", "POSIXt"), tzone = "UTC")
  )

  expect_equal(sort(unique(test$Instrument)), c("AAPL.O"))

})



