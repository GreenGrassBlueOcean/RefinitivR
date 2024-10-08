test_that("rd_GetHistory fails when it should", {

  expect_error(rd_GetHistory(RD = NULL, fields = c("BID", "ASK"))
               , "Parameter universe should be supplied and is not")

})

test_that("rd_GetHistory can handle requests with no fields", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python <- rd_GetHistory(RD = TestRDObject,  universe="AAPL.O")

  TestJSONObject <- RefinitivJsonConnect()
  test_json <- rd_GetHistory(RD = TestJSONObject,  universe="AAPL.O")

  expect_equal(class(test_python), "data.frame")
  expect_equal(unique(test_python$Instrument), "AAPL.O")

  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)


  data.table::setcolorder( test_json
                           , neworder = names(test_python)
  )


  data.table::setorderv(test_python, keycol)
  data.table::setorderv(test_json, keycol)

  expect_equal(test_python, test_json)

})


test_that("rd_GetHistory can handle requests with only timeseries fields", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))


  Fields <- c("TRDPRC_1", "HIGH_1", "LOW_1", "ACVOL_UNS",
              "OPEN_PRC", "BID", "ASK", "TRNOVR_UNS", "VWAP", "BLKCOUNT", "BLKVOLUM",
              "NUM_MOVES", "TRD_STATUS", "SALTIM", "NAVALUE" )

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python <- rd_GetHistory(RD = TestRDObject,  universe="AAPL.O"
                              , fields =Fields)

  TestJSONObject <- RefinitivJsonConnect()
  test_json <- rd_GetHistory(RD = TestJSONObject,  universe="AAPL.O", fields =Fields)

  expect_equal(class(test_python), "data.frame")

  expect_equal(lapply(test_python, class)
              , list(Date = "Date", Instrument = "character", TRDPRC_1 = "numeric",
                     HIGH_1 = "numeric", LOW_1 = "numeric", ACVOL_UNS = "integer",
                     OPEN_PRC = "numeric", BID = "numeric", ASK = "numeric", TRNOVR_UNS = "numeric",
                     VWAP = "numeric", BLKCOUNT = "integer", BLKVOLUM = "integer",
                     NUM_MOVES = "integer", TRD_STATUS = "integer", SALTIM = "integer",
                     NAVALUE = "logical")
  )

  expect_equal(unique(test_python$Instrument), "AAPL.O")

  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)

  data.table::setcolorder( test_python
                          , neworder = c(keycol,  sort(Fields))
  )

  data.table::setcolorder( test_json
                           , neworder = c(keycol,  sort(Fields))
  )


  data.table::setorderv(test_python, keycol)
  data.table::setorderv(test_json, keycol)

  expect_equal(test_python, test_json)

})

test_that("rd_GetHistory can handle request with simple fields", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  # TestRD <- check_Eikonapi(ExecutionMode = "RD")


  Universe = c("GOOG.O","MSFT.O")
  Fields =  c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
  Parameters = list("SDate" = 0,"EDate" = -3
                    ,"FRQ" = "FY", "Curn" = "EUR") #"Scale" = 6

  test_json <- suppressWarnings(rd_GetHistory( RD = RDConnect(PythonModule = "JSON")
                                               , universe = Universe
                                               , fields = Fields
                                               , parameters = Parameters
                                               , use_field_names_in_headers = TRUE))

  Universe2 <- Universe

  test_python <- suppressWarnings(rd_GetHistory( RD = RDConnect(PythonModule = "RD")
                                               , universe = Universe2
                                               , fields = Fields
                                               , parameters = Parameters
                                               , use_field_names_in_headers = TRUE)
                                  )


  expect_equal(class(test_python), "data.frame")

  actual <- lapply(test_python, class)
  actual <- actual[order(names(actual))]

  expected <- list(Date = "Date", Instrument = "character"
                   , TR.GROSSPROFIT = "numeric",
                   TR.REVENUE = "numeric", TR.REVENUE.DATE ="Date")
  expected <- expected[order(names(expected))]

  expect_equal(actual, expected)


  expect_equal(sort(unique(test_python$Instrument)), c("GOOG.O", "MSFT.O"))


  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)
  #
  data.table::setorderv(test_python, keycol, order = -1)
  data.table::setorderv(test_json, keycol, order = -1)

  data.table::setcolorder( test_json, neworder = names(test_python))

  expect_equal(test_python, test_json)
})

test_that("rd_GetHistory can handle request with explicit date", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  Universe = c("GOOG.O","MSFT.O")
  Start = "2020-01-01T01:00:00"
  End = "2020-01-10T01:00:00"

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python <- rd_GetHistory( RD = TestRDObject
                       , universe = Universe
                       , start = Start
                       , end = End
                       )

  TestRDObject <- RDConnect(PythonModule = "JSON")
  test_json <- rd_GetHistory( RD = TestRDObject
                                , universe = Universe
                                , start = Start
                                , end = End
  )

  expect_equal(class(test_python), "data.frame")

  test_json <- data.table::as.data.table(test_json)
  test_python <- data.table::as.data.table(test_python)

  data.table::setcolorder( test_json, neworder = names(test_python))

  expect_equal(test_json, test_python)

})


test_that("rd_GetHistory can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  Universe <- "AAPL.O"
  Fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)"
             ,"TR.FreeFloatPct()/100/*FreefloatWeight*/"
             ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
             ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")

  Parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01")

  test_json <- rd_GetHistory( RD = RDConnect(PythonModule = "JSON"), universe= Universe
                            , fields = Fields
                            , parameters = Parameters)

  test_python <-  rd_GetHistory(RD = RDConnect(PythonModule = "RD"),  universe= Universe
                               , fields = Fields, parameters = Parameters)

  expect_equal(class(test_json), "data.frame")
  expect_equal(sort(unique(test_json$Date))
              , structure(c(18536, 18562, 18563, 18564, 18565, 18567, 18568,
                            18569, 18570, 18571, 18572, 18575, 18576, 18577, 18578, 18579,
                            18582, 18583, 18584, 18585, 18586, 18589, 18590, 18591, 18593,
                            18596, 18597), class = "Date"))

  expect_equal(sort(unique(test_json$Instrument)), c("AAPL.O"))

  test_json <- data.table::as.data.table(test_json)
  test_python <- data.table::as.data.table(test_python)

  data.table::setcolorder( test_json, neworder = names(test_python))

  expect_equal(test_json, test_python)

})


test_that("rd_GetHistory will not handle requests ", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  universe = c("GOOG.O","MSFT.O")
  fields =  c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
  parameters = list("Scale" = 6,"SDate" = 0,"EDate" = -3
                    ,"FRQ" = "FY", "Curn" = "EUR")


  expect_error(rd_GetHistory( RD = NULL, universe = universe
                            , fields = fields
                            , parameters = parameters
                            , use_field_names_in_headers = FALSE),
               "Fields should not contain Tr.Eikonformula.date  and use_field_names_in_headers = FALSE for use in GetHistory function removing TR.Revenue.date you will receive than multiple columns that have the same name (Date) and is therefore forbidden"
               , fixed = TRUE)
})



test_that("rd_GetHistory will handle requests with multiple instruments and one field",{

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- c("AAPL.O", "NVDA.O")
  fields <- "TR.ClosePrice"
  StartDate <- "2020-01-02"
  EndDate <- "2020-01-10"

  timeseries1_python <-  rd_GetHistory(universe= rics
                                      , fields = fields
                                      , RD = RDConnect(PythonModule = "RD")
                                      , start = StartDate, end = EndDate)


  timeseries1_JSON <-  rd_GetHistory(universe= rics
                                       , fields = fields
                                       , RD = RDConnect(PythonModule = "JSON")
                                       , start = StartDate, end = EndDate)


  timeseries1_python <- timeseries1_python[order(names(timeseries1_python))]
  timeseries1_JSON <- timeseries1_JSON[order(names(timeseries1_JSON))]


  expect_equal(timeseries1_python, timeseries1_JSON)



})


test_that("rd_GetHistory will handle requests with one instruments and multiple fields",{

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- c("AAPL.O")
  Fields <- c("TR.ClosePrice", "TR.OpenPrice")
  StartDate <- "2020-01-02"
  EndDate <- "2020-01-10"

  timeseries2_python <- rd_GetHistory(universe=rics
                                     , fields = Fields
                                     , RD = RDConnect(PythonModule = "RD")
                                     , start = StartDate, end = EndDate)

  timeseries2_JSON <-  rd_GetHistory(universe=rics
                                    , fields = Fields
                                    , RD = RDConnect(PythonModule = "JSON")
                                    , start = StartDate,  end = EndDate)

  expect_equal(timeseries2_python, timeseries2_JSON)

})

test_that("rd_GetHistory can handle timedates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- "AAPL.O"
  Eikonformulas <- "TR.IssueMarketCap(Scale=6,ShType=FFL,Curn=USD)"

  test_python <- Refinitiv::rd_GetHistory( universe = rics, RD = RDConnect(PythonModule = "RD")
                                       , fields = Eikonformulas
                                       , start = "2020-10-27T01:00:00"
                                       , end = "2020-12-01T01:00:00"
                                       , parameters = NULL, adjustments = NULL
                                       )

  test_json <- Refinitiv::rd_GetHistory( universe = rics, RD = RDConnect(PythonModule = "JSON")
                                           , fields = Eikonformulas
                                           , start = "2020-10-27T01:00:00"
                                           , end = "2020-12-01T01:00:00"
                                           , parameters = NULL, adjustments = NULL
  )


  test_python <- test_python[order(names(test_python))]
  test_json <- test_json[order(names(test_json))]

  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)


  data.table::setcolorder( test_json
                           , neworder = names(test_python)
  )


  data.table::setorderv(test_python, keycol)
  data.table::setorderv(test_json, keycol)





  expect_equal(test_python, test_json)

})


test_that("rd_GetHistory can handle problematic fields",{

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  fields <- c('BID', 'ASK', 'TRDPRC_1', 'SETTLE')
  universe <- c("AAPL.O", "NVDA.O")

  timeseries2_python <- suppressMessages(suppressWarnings(rd_GetHistory(universe=universe
                                      , fields = fields
                                      , RD = RDConnect(PythonModule = "RD")
                                      )))
  timeseries2_JSON <- suppressMessages(suppressWarnings(rd_GetHistory(universe=universe
                                  , fields = fields
                                  , RD = RDConnect(PythonModule = "JSON")
                                  )))
  timeseries2_python <- timeseries2_python[order(names(timeseries2_python))]
  timeseries2_JSON <- timeseries2_JSON[order(names(timeseries2_JSON))]


  keycol <- c("Date", "Instrument")
  timeseries2_python <- data.table::as.data.table(timeseries2_python)
  timeseries2_JSON <- data.table::as.data.table(timeseries2_JSON)


  data.table::setcolorder( timeseries2_JSON
                           , neworder = names(timeseries2_python)
  )


  data.table::setorderv(timeseries2_python, keycol)
  data.table::setorderv(timeseries2_JSON, keycol)



  expect_equal(timeseries2_python, timeseries2_JSON)



})


test_that("rd_GetHistory can handle non existing rics",{

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  fields <- c('BID', 'ASK', 'TRDPRC_1', "TR.IssueMarketCap(Scale=6,ShType=FFL,Curn=USD)")


  NotExistingOption_json <- rd_GetHistory( universe = c("AAPLA212216000.U^22"),
                                         , interval = "P1D", count = 20L, RD = RDConnect(PythonModule = "JSON")
                                         , fields = fields ) |> suppressWarnings()

  CorrectOutcome <- structure(list( Instrument = character(0)
                                  , Date = structure(numeric(0), class = "Date")
                                  , `TR.ISSUEMARKETCAP(SCALE=6,SHTYPE=FFL,CURN=USD)` = logical(0))
                              , row.names = integer(0), class = "data.frame")

  NotExistingOption_json <- NotExistingOption_json[order(names(NotExistingOption_json))]
  CorrectOutcome <- CorrectOutcome[order(names(CorrectOutcome))]

  expect_equal(NotExistingOption_json, CorrectOutcome)

})
