test_that("rd_GetData satisfies basic request", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  RICS = c("AAPL.O", "GOOG.O", "III.L", "MMM")
  Eikonformulas = c("TR.PriceClose.Currency", "TR.InstrumentType", "TR.ExchangeName"
                   , "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive","TR.ISINCode")

  # not working in RD "CF_CURR", "CF_EXCHNG"

  test3_RD <- rd_GetData(rics =  RICS, Eikonformulas = Eikonformulas
                      , RD = RDConnect(PythonModule = "RD") )

  test3_json <- rd_GetData(rics =  RICS, Eikonformulas = Eikonformulas
                           , RD = RDConnect(PythonModule = "JSON") )

  test3_json <- data.table::as.data.table(test3_json)
  test3_RD <- data.table::as.data.table(test3_RD)

  data.table::setcolorder( test3_json, neworder = names(test3_RD))


  expect_equal(test3_RD, test3_json)
  expect_equal(sort(names(test3_RD)), sort(c("Instrument", "Currency", "Instrument Type", "Exchange Name",
                                        "Exchange Market Identifier Code", "Instrument Is Active Flag",
                                        "ISIN Code")))

  expect_equal(sort(unique(test3_RD$Instrument))
              , sort(RICS))

})



test_that("rd_GetData can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- c("AAPL.O")
  fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
              ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")

  parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01", "Fill" ="None")

  test_json <- rd_GetData(RD = RDConnect(PythonModule = "JSON"), rics =  rics
                             , Eikonformulas =  fields
                             , Parameters = parameters
                          , use_field_names_in_headers = TRUE
                          , SyncFields = FALSE)

  test_python <-  rd_GetData(RD = RDConnect(PythonModule = "RD"),  rics= rics
                                , Eikonformulas = fields
                               , Parameters = parameters
                             , use_field_names_in_headers = TRUE)

  expect_equal(class(test_json), "data.frame")


  test_json <- data.table::as.data.table(test_json)
  test_python <- data.table::as.data.table(test_python)

  data.table::setcolorder( test_json, neworder = names(test_python))


  expect_equal(sort(unique(test_python$Instrument)), c("AAPL.O"))
  expect_equal(test_json, test_python)

})
#



test_that("rd_GetData can handle previous problem case", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))


asset <- "AAPL.O"

# Retrieve corporate events
corp_event_rd <- rd_GetData( SpaceConvertor = ".",  RD = RDConnect(PythonModule = "JSON"),
                        , rics = asset
                        , Eikonformulas = c("TR.CAEffectiveDate", "TR.CAAdjustmentFactor", "TR.CAAdjustmentType")
                        , Parameters = list("CAEventType" = "SSP"
                                           ,"SDate" = as.character(format(Sys.Date() - lubridate::dyears(50), "%Y-%m-%d"))
                                           , "EDate" = as.character(Sys.Date())
                                           )
                                          #, use_field_names_in_headers = TRUE
                                          #, SyncFields = TRUE
                                          #,
                                          )
corp_event_rd$Capital.Change.Effective.Date <- as.character(corp_event_rd$Capital.Change.Effective.Date)

corp_event_eikon <- EikonGetData(EikonObject = EikonConnect()
                          , rics = asset
                          , Eikonformulas = c("TR.CAEffectiveDate", "TR.CAAdjustmentFactor", "TR.CAAdjustmentType")
                          , Parameters = list("CAEventType" = "SSP"
                                             ,"SDate" = as.character(format(Sys.Date() - lubridate::dyears(50), "%Y-%m-%d"))
                                             , "EDate" = as.character(Sys.Date()))

                                            #, use_field_names_in_headers = TRUE
                                            #, SyncFields = TRUE

                          )

testthat::expect_equal(corp_event_rd, corp_event_eikon$PostProcessedEikonGetData)

})
