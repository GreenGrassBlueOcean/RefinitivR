test_that("rd_OutputProcesser works", {



  response <- list(links = list(count = 46L), variability = "", universe = list(
    list(Instrument = "GOOG.O", `Company Common Name` = "Alphabet Inc",
         `Organization PermID` = "5030853586", `Reporting Currency` = "USD"),
    list(Instrument = "NVDA.O", `Company Common Name` = "NVIDIA Corp",
         `Organization PermID` = "4295914405", `Reporting Currency` = "USD")),
    data = list( list("GOOG.O", "2022-10-05T00:00:00", 102.22, 100.69)
               , list("GOOG.O", "2022-10-06T00:00:00", 102.24, 101.5)
               , list("GOOG.O", "2022-10-07T00:00:00", 99.57, 100.65)
               , list("GOOG.O", "2022-10-10T00:00:00", 98.71, 99.85)
               , list("GOOG.O", "2022-10-11T00:00:00", 98.05, 98.25)
               , list("GOOG.O", "2022-10-12T00:00:00", 98.3, 98.27)
               , list("GOOG.O", "2022-10-13T00:00:00", 99.71, 95.93)
               , list("GOOG.O", "2022-10-14T00:00:00", 97.18, 100.625)
               , list("GOOG.O", "2022-10-17T00:00:00", 100.78, 99.52)
               , list("GOOG.O", "2022-10-18T00:00:00", 101.39, 103.94)
               , list("GOOG.O", "2022-10-19T00:00:00", 100.29, 100.7)
               , list("GOOG.O", "2022-10-20T00:00:00", 100.53, 100.82)
               , list("GOOG.O", "2022-10-21T00:00:00", 101.48, 98.46)
               , list("GOOG.O", "2022-10-24T00:00:00", 102.97, 102.09)
               , list("GOOG.O", "2022-10-25T00:00:00", 104.93, 103.3)
               , list("GOOG.O", "2022-10-26T00:00:00", 94.82, 96.76)
               , list("GOOG.O", "2022-10-27T00:00:00", 92.6, 94.31)
               , list("GOOG.O", "2022-10-28T00:00:00", 96.58, 92.53)
               , list("GOOG.O", "2022-10-31T00:00:00", 94.66, 95.78)
               , list("GOOG.O", "2022-11-01T00:00:00", 90.5, 95.59)
               , list("GOOG.O", "2022-11-02T00:00:00", 87.07, 90.91)
               , list("GOOG.O", "2022-11-03T00:00:00", 83.49, 86.345)
               , list("GOOG.O", "2022-11-04T00:00:00", 86.7, 85.51)
               , list("NVDA.O", "2022-10-05T00:00:00", 132.09, 129.107)
               , list("NVDA.O", "2022-10-06T00:00:00", 131.3, 132.2)
               , list("NVDA.O", "2022-10-07T00:00:00", 120.76, 125.05)
               , list("NVDA.O", "2022-10-10T00:00:00", 116.7, 120.37)
               , list("NVDA.O", "2022-10-11T00:00:00", 115.86, 115.61)
               , list("NVDA.O", "2022-10-12T00:00:00", 115L, 115.79)
               , list("NVDA.O", "2022-10-13T00:00:00", 119.6, 109.71)
               , list("NVDA.O", "2022-10-14T00:00:00", 112.27, 120.57)
               , list("NVDA.O", "2022-10-17T00:00:00", 118.88, 115.18)
               , list("NVDA.O", "2022-10-18T00:00:00", 119.67, 123.44)
               , list("NVDA.O", "2022-10-19T00:00:00", 120.51, 118.79)
               , list("NVDA.O", "2022-10-20T00:00:00", 121.94, 121.13)
               , list("NVDA.O", "2022-10-21T00:00:00", 124.66, 120.982)
               , list("NVDA.O", "2022-10-24T00:00:00", 125.99, 125.08)
               , list("NVDA.O", "2022-10-25T00:00:00", 132.61, 126.935)
               , list("NVDA.O", "2022-10-26T00:00:00", 128.96, 128.69)
               , list("NVDA.O", "2022-10-27T00:00:00", 131.76, 136.3)
               , list("NVDA.O", "2022-10-28T00:00:00", 138.34, 131.04)
               , list("NVDA.O", "2022-10-31T00:00:00", 134.97, 137.78)
               , list("NVDA.O", "2022-11-01T00:00:00", 135.43, 138.11)
               , list("NVDA.O", "2022-11-02T00:00:00", 132.19, 138.5)
               , list("NVDA.O", "2022-11-03T00:00:00", 134.21, 130.43)
               , list("NVDA.O", "2022-11-04T00:00:00", 141.56, 139.86)),
    messages = list(codes = list(list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L)
                                , list(-1L, -1L, -1L, -1L))
                    , descriptions = list(list(code = -1L, description = "ok"))), headers = list(
                                          list(name = "instrument", title = "Instrument", type = "string",
                                                 description = "The requested Instrument as defined by the user."),
                                          list(name = "date", title = "Date", type = "datetime",
                                                 description = "Date associated with the returned data."),
                                          list(name = "TR.Close", title = "Price Close", type = "number",
                                                 decimalChar = ".", description = "The latest available closing price. If there are no trades for the most recent completed tradable day, the most recent prior tradable day with trading activity is used, provided the last tradable day for the instrument is within 378 completed calendar days (54 weeks)."),
                                          list(name = "TR.Open", title = "Price Open", type = "number",
                                                 decimalChar = ".", description = "Open price for the latest trading day")))
  actual <- rd_OutputProcesser(response, use_field_names_in_headers = TRUE )

  expected <- structure(list(Instrument = c("GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O"
  ), Date = structure(c(19270, 19271, 19272, 19275, 19276, 19277,
                        19278, 19279, 19282, 19283, 19284, 19285, 19286, 19289, 19290,
                        19291, 19292, 19293, 19296, 19297, 19298, 19299, 19300, 19270,
                        19271, 19272, 19275, 19276, 19277, 19278, 19279, 19282, 19283,
                        19284, 19285, 19286, 19289, 19290, 19291, 19292, 19293, 19296,
                        19297, 19298, 19299, 19300), class = "Date")
  , TR.Close = c(102.22, 102.24, 99.57, 98.71, 98.05, 98.3, 99.71, 97.18, 100.78, 101.39,
                 100.29, 100.53, 101.48, 102.97, 104.93, 94.82, 92.6, 96.58, 94.66,
                 90.5, 87.07, 83.49, 86.7, 132.09, 131.3, 120.76, 116.7, 115.86,
                 115, 119.6, 112.27, 118.88, 119.67, 120.51, 121.94, 124.66, 125.99,
                 132.61, 128.96, 131.76, 138.34, 134.97, 135.43, 132.19, 134.21,
                 141.56)
  , TR.Open = c(100.69, 101.5, 100.65, 99.85, 98.25, 98.27, 95.93, 100.625
               , 99.52, 103.94, 100.7, 100.82, 98.46, 102.09,
               103.3, 96.76, 94.31, 92.53, 95.78, 95.59, 90.91, 86.345, 85.51,
               129.107, 132.2, 125.05, 120.37, 115.61, 115.79, 109.71, 120.57,
               115.18, 123.44, 118.79, 121.13, 120.982, 125.08, 126.935, 128.69,
               136.3, 131.04, 137.78, 138.11, 138.5, 130.43, 139.86))
  , row.names = c(NA, -46L), class = c("data.table", "data.frame"))

  expect_equal(actual, expected)

  actual_2 <- rd_OutputProcesser(response, use_field_names_in_headers = FALSE )

  expected_2 <- structure(list(Instrument = c("GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                              "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                              "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                              "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "NVDA.O", "NVDA.O",
                                              "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                              "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                              "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O")
                               , Date = structure(c(19270, 19271, 19272, 19275, 19276, 19277,
                                                    19278, 19279, 19282, 19283, 19284, 19285, 19286, 19289, 19290,
                                                    19291, 19292, 19293, 19296, 19297, 19298, 19299, 19300, 19270,
                                                    19271, 19272, 19275, 19276, 19277, 19278, 19279, 19282, 19283,
                                                    19284, 19285, 19286, 19289, 19290, 19291, 19292, 19293, 19296,
                                                    19297, 19298, 19299, 19300), class = "Date")
                               , `Price Close` = c(102.22, 102.24, 99.57, 98.71, 98.05, 98.3, 99.71, 97.18, 100.78, 101.39,
                                                   100.29, 100.53, 101.48, 102.97, 104.93, 94.82, 92.6, 96.58, 94.66,
                                                   90.5, 87.07, 83.49, 86.7, 132.09, 131.3, 120.76, 116.7, 115.86,
                                                   115, 119.6, 112.27, 118.88, 119.67, 120.51, 121.94, 124.66, 125.99,
                                                   132.61, 128.96, 131.76, 138.34, 134.97, 135.43, 132.19, 134.21,
                                                   141.56)
                               , `Price Open` = c(100.69, 101.5, 100.65, 99.85, 98.25, 98.27, 95.93, 100.625, 99.52, 103.94, 100.7, 100.82, 98.46, 102.09,
                                                  103.3, 96.76, 94.31, 92.53, 95.78, 95.59, 90.91, 86.345, 85.51,
                                                  129.107, 132.2, 125.05, 120.37, 115.61, 115.79, 109.71, 120.57,
                                                  115.18, 123.44, 118.79, 121.13, 120.982, 125.08, 126.935, 128.69,
                                                  136.3, 131.04, 137.78, 138.11, 138.5, 130.43, 139.86))
                          , row.names = c(NA, -46L), class = c("data.table", "data.frame"))

  expect_equal(actual_2, expected_2)

})



test_that("rd_OutputProcesser works case RDConnect get_data with names", {

  input <- list(columnHeadersCount = 1L, data = list(list("MMM", "", "3M Co"),
                                                     list("III.L", 4.23463019323009, "3i Group PLC")), headerOrientation = "horizontal",
                headers = list(list(list(displayName = "Instrument"), list(
                  displayName = "P/E (Daily Time Series Ratio)", field = "TR.PE(SDATE=0D)/*P/E (LTM) - DILUTED EXCL*/"),
                  list(displayName = "Company Name", field = "TR.COMPANYNAME"))),
                rowHeadersCount = 1L, totalColumnsCount = 3L, totalRowsCount = 3L)

  use_field_names_in_headers <- FALSE

  actual <- rd_OutputProcesser(input, use_field_names_in_headers)


  expected <- structure(list( Instrument = c("MMM", "III.L")
                            , `P/E (Daily Time Series Ratio)` = c(0, 4.23463019323009), `Company Name` = c("3M Co", "3i Group PLC"))
                        , row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  expect_equal(actual, expected)
})

test_that("rd_OutputProcesser works case RDConnect get_data with fieldnames", {

  input <- list(columnHeadersCount = 1L, data = list(list("MMM", ""), list(
    "III.L", 4.6545003199969)), headerOrientation = "horizontal",
    headers = list(list(list(displayName = "Instrument"), list(
      displayName = "P/E (Daily Time Series Ratio)", field = "TR.PE(SDATE=0D)/*P/E (LTM) - DILUTED EXCL*/"))),
    rowHeadersCount = 1L, totalColumnsCount = 2L, totalRowsCount = 3L)


  actual <- rd_OutputProcesser(x = input, use_field_names_in_headers = TRUE, NA_cleaning = FALSE)


  expected <- structure(list(Instrument = c("MMM", "III.L")
                             , `TR.PE(SDATE=0D)/*P/E (LTM) - DILUTED EXCL*/` = c(NA, 4.6545003199969))
                        , row.names = c(NA, -2L), class = c("data.table", "data.frame"))
  expect_equal(actual, expected)


  actual <- rd_OutputProcesser(x = input, use_field_names_in_headers = FALSE, NA_cleaning = FALSE)


  expected <- structure(list(Instrument = c("MMM", "III.L")
                             , `P/E (Daily Time Series Ratio)` = c(NA,4.6545003199969))
                        , row.names = c(NA, -2L), class = c("data.table", "data.frame"))


})


test_that("rd_OutputProcesser works case 3 json connect get_data with names", {

  input <- list(links = list(count = 3L), variability = "", universe = list(
    list(Instrument = "MMM", `Company Common Name` = "3M Co",
         `Organization PermID` = "5000072036", `Reporting Currency` = "USD"),
    list(Instrument = "III.L", `Company Common Name` = "3i Group PLC",
         `Organization PermID` = "4295895807", `Reporting Currency` = "GBP")),
    data = list(list("MMM", NULL, NULL, "3M Co"), list("III.L", NULL, NULL, "3i Group PLC")
                , list("III.L", "2023-10-20T00:00:00", 4.23463019323009, NULL))
    , messages = list(codes = list(list(-1L, -2L, -2L, -1L), list(-1L, -2L, -2L, -1L)
                                   , list(-1L, -1L, -1L, -2L)), descriptions = list(list(code = -2L, description = "empty")
                                                                                    , list(code = -1L, description = "ok"))),
    headers = list(list(name = "instrument", title = "Instrument",
                        type = "string", description = "The requested Instrument as defined by the user."),
                   list(name = "date", title = "Date", type = "datetime",
                        description = "Date associated with the returned data."),
                   list(name = "TR.PE", title = "P/E (Daily Time Series Ratio)",
                        type = "number", decimalChar = ".", description = "A valuation ratio of a company's current share price relative to its EPS. EPS is LTM Earnings per Share From Continuing Operations. PE is not calculated when LTM EPS is less than or equal to Zero."),
                   list(name = "Company Name", title = "Company Name", type = "string",
                        description = "Company Name.")))

  use_field_names_in_headers <- FALSE

  actual <- rd_OutputProcesser(input, use_field_names_in_headers,NA_cleaning = FALSE)


  expected <- structure(list(Instrument = c("MMM", "III.L", "III.L")
                             , Date = structure(c(NA, NA, 19650), class = "Date")
                             , `P/E (Daily Time Series Ratio)` = c(NA, NA, 4.23463019323009)
                             , `Company Name` = c("3M Co", "3i Group PLC", NA))
                        , row.names = c(NA, -3L), class = c("data.table", "data.frame"))
  expect_equal(actual, expected)
})


test_that("rd_OutputProcesser works case 3 json connect get_data with fields", {

  input <- list(links = list(count = 3L), variability = "", universe = list(
    list(Instrument = "MMM", `Company Common Name` = "3M Co",
         `Organization PermID` = "5000072036", `Reporting Currency` = "USD"),
    list(Instrument = "III.L", `Company Common Name` = "3i Group PLC",
         `Organization PermID` = "4295895807", `Reporting Currency` = "GBP")),
    data = list(list("MMM", NULL, NULL, "3M Co"), list("III.L", NULL, NULL, "3i Group PLC")
                , list("III.L", "2023-10-20T00:00:00", 4.23463019323009, NULL))
    , messages = list(codes = list(list(-1L, -2L, -2L, -1L), list(-1L, -2L, -2L, -1L)
                                   , list(-1L, -1L, -1L, -2L)), descriptions = list(list(code = -2L, description = "empty")
                                                                                    , list(code = -1L, description = "ok"))),
    headers = list(list(name = "instrument", title = "Instrument",
                        type = "string", description = "The requested Instrument as defined by the user."),
                   list(name = "date", title = "Date", type = "datetime",
                        description = "Date associated with the returned data."),
                   list(name = "TR.PE", title = "P/E (Daily Time Series Ratio)",
                        type = "number", decimalChar = ".", description = "A valuation ratio of a company's current share price relative to its EPS. EPS is LTM Earnings per Share From Continuing Operations. PE is not calculated when LTM EPS is less than or equal to Zero."),
                   list(name = "Company Name", title = "Company Name", type = "string",
                        description = "Company Name.")))


  actual <- rd_OutputProcesser(input, use_field_names_in_headers = TRUE, NA_cleaning = TRUE)


  expected <- structure(list(Instrument = c("MMM", "III.L", "III.L")
                             , Date = structure(c(0, 0, 19650), class = "Date")
                             , TR.PE = c(0, 0, 4.23463019323009)
                             , `Company Name` = c("3M Co", "3i Group PLC", "FALSE"))
                        , row.names = c(NA,-3L), class = c("data.table", "data.frame"))
  expect_equal(actual, expected)
})


test_that("rd_OutputProcesser can work with empty and failed requests", {

  #failed request
  x <- list(universe = list(ric = "someRic")
           , status = list(code = "some error",
                           message = "some message"))
  suppressMessages(suppressWarnings(expect_equal(rd_OutputProcesser(x  , use_field_names_in_headers = TRUE, NA_cleaning = TRUE, SpaceConvertor = NULL)
              , data.table::data.table()
              )))

  #empty request returned
  x <- list(universe = list(ric = "someRic")
            , data = list())
  suppressMessages(suppressWarnings(expect_equal(rd_OutputProcesser(x  , use_field_names_in_headers = TRUE, NA_cleaning = TRUE, SpaceConvertor = NULL)
               , data.table::data.table()
  )))


  })




test_that("rd_OutputProcesser can work with complete NA responses ", {


 x <- list(columnHeadersCount = 1L, data = list(list("NVDA.O", NULL), list("ASMI.AS", NULL))
           , error = list(list(code = 218L, col = 1L, message = "The formula must contain at least one field or function.",row = 0L)
                        , list(code = 218L, col = 1L, message = "The formula must contain at least one field or function.", row = 1L))
           , headerOrientation = "horizontal", headers = list(list(displayName = NULL)
                                                             , list(displayName = "TR.IVPRICETOLNTRINSICVALUEGLOBALRANK"
                                                                    , field = "TR.IVPRICETOLNTRINSICVALUEGLOBALRANK"))
           , rowHeadersCount = 1L, totalColumnsCount = 2L, totalRowsCount = 3L)


 test <- suppressWarnings(suppressMessages(expect_warning(rd_OutputProcesser(x  , use_field_names_in_headers = TRUE, NA_cleaning = FALSE, SpaceConvertor = NULL))))

 CorrectOutcome <- structure(list(V1 = c("NVDA.O", "ASMI.AS"), TR.IVPRICETOLNTRINSICVALUEGLOBALRANK = c(NA,NA)), row.names = c(NA, -2L)
                            , class = c("data.table", "data.frame"))


 expect_equal(CorrectOutcome, test)
})



test_that("rd_OutputProcesser can work with partial NA responses ", {


input  <- list(universe = list(ric = "AAPL.O"), interval = "P1D", summaryTimestampLabel = "endPeriod",
     adjustments = list("exchangeCorrection", "manualCorrection",
                        "CCH", "CRE", "RTS", "RPO"), defaultPricingField = "TRDPRC_1",
     headers = list(list(name = "DATE", type = "string"), list(
       name = "TRDPRC_1", type = "number", decimalChar = "."),
       list(name = "HIGH_1", type = "number", decimalChar = "."),
       list(name = "LOW_1", type = "number", decimalChar = "."),
       list(name = "ACVOL_UNS", type = "number", decimalChar = "."),
       list(name = "OPEN_PRC", type = "number", decimalChar = "."),
       list(name = "BID", type = "number", decimalChar = "."),
       list(name = "ASK", type = "number", decimalChar = "."),
       list(name = "TRNOVR_UNS", type = "number", decimalChar = "."),
       list(name = "VWAP", type = "number", decimalChar = "."),
       list(name = "BLKCOUNT", type = "number", decimalChar = "."),
       list(name = "BLKVOLUM", type = "number", decimalChar = "."),
       list(name = "NUM_MOVES", type = "number", decimalChar = "."),
       list(name = "TRD_STATUS", type = "number", decimalChar = "."),
       list(name = "SALTIM", type = "number", decimalChar = "."),
       list(name = "NAVALUE", type = "number", decimalChar = "."),
       list(name = "VWAP_VOL", type = "number", decimalChar = ".")),
     data = list(list("2024-08-27", 228.03, 228.85, 224.89, 35934559,
                      225.995, 228.14, 228.15, 8185629556, 227.7513, 51, 11416370,
                      491852, 1, 72000, NULL, 28153632)
                 , list("2024-08-26",227.18, 227.28, 223.8905, 30602208, 226.76, 227.22, 227.26,
                        6918040853, 225.9286, 41, 7628281, 565404, 1, 72000,
                        NULL, 25868715)
                 , list("2024-08-23", 226.84, 228.22, 224.33,
                        38677250, 225.6589, 226.73, 226.79, 8763518551, 226.5552,
                        47, 11158110, 555154, 1, 72000, NULL, 33599651)
                 , list("2024-08-22", 224.53, 228.34, 223.9, 43695321, 227.79,
                        224.53, 224.54, 9852330095, 225.6868, 54, 14384685, 586190,
                        1, 72900, NULL, 33958811)
                 , list("2024-08-21", 226.4,227.98, 225.05, 34765480, 226.52, 226.35, 226.39, 7872506188,
                        226.4466, 69, 10364824, 506656, 1, 72000, NULL, 28963442)
                 , list("2024-08-20", 226.51, 227.17, 225.45, 30299033, 225.77, 226.5, 226.52, 6863636658, 226.5423, 46,
                      7844015, 498797, 1, 72000, NULL, 25490741)
                 , list("2024-08-19", 225.89, 225.99, 223.04, 40687813, 225.72,
                        225.89, 225.92, 9148975381, 224.7369, 68, 13105076,
                        606363, 1, 72000, NULL, 35908387)
                 , list("2024-08-16",226.05, 226.8271, 223.6501, 44340240, 223.92, 226.07,
                        226.1, 9995644943, 225.4064, 75, 17560109, 562638,
                        1, 72000, NULL, NULL)
                 , list("2024-08-15", 224.72,225.35, 222.76, 46414013, 224.6, 224.71, 224.72,
                        10422279814, 224.5537, 63, 16142449, 590575, 1, 72000,
                        NULL, NULL)
                 , list("2024-08-14", 221.72, 223.03, 219.7,
                        41960574, 220.57, 221.59, 221.62, 9294719678, 221.495,
                        79, 12287469, 568519, 1, 72000, NULL, NULL)
                 , list("2024-08-13", 221.27, 221.89, 219.01, 44155331, 219.01,
                        221.26, 221.27, 9752841920, 220.991, 77, 15242348,
                        553265, 1, 72900, NULL, NULL)
                 , list("2024-08-12",217.53, 219.5099, 215.6, 38028092, 216.07, 217.53,
                        217.6, 8272879342, 217.5629, 47, 8831416, 602530,
                        1, 72000, NULL, NULL)
                 , list("2024-08-09", 216.24, 216.78, 211.97, 42201646, 212.1, 216.3, 216.31, 9081788822,
                        215.1614, 57, 10196037, 589469, 1, 72000, NULL, NULL)
                 , list("2024-08-08", 213.31, 214.2, 208.83, 47161149, 213.11,
                        213.34, 213.39, 10027425280, 212.5627, 84, 12751082,
                        628773, 1, 72000, NULL, NULL)
                 , list("2024-08-07", 209.82, 213.64, 206.39, 63516417, 206.9, 209.81,
                        209.87, 13382167507, 211.0548, 118, 19200973, 741724,
                        1, 72900, NULL, NULL)
                 , list("2024-08-06", 207.23,209.99, 201.07, 69660488, 205.3, 207.11, 207.14,
                        14401602171, 206.5738, 99, 15948484, 921953, 1, 72000,
                        NULL, NULL)
                 , list("2024-08-05", 209.27, 213.5, 196,
                        119548589, 199.09, 209.17, 209.3, 24842394979, 208.1691,
                        133, 21300770, 1661550, 1, 72000, NULL, NULL)
                 , list("2024-08-02", 219.86, 225.6, 217.71, 105568560, 219.15,
                        219.8, 219.85, 23425004933, 222.3303, 122, 27568046,
                        1132553, 1, 72000, NULL, NULL)
                 , list("2024-08-01", 218.36, 224.48, 217.02, 62500996, 224.37, 218.26,
                        218.3, 13716031825, 219.5743, 102, 12925524, 876129,
                        1, 72000, NULL, NULL)
                 , list("2024-07-31", 222.08,223.82, 220.63, 50036262, 221.44, 221.99, 222.02,
                        11124841898, 222.394, 76, 14235610, 668901, 1, 72000,
                        NULL, NULL))
     , meta = list(blendingEntry = list(headers = list(list(name = "DATE", type = "string")
                                                       , list(name = "TRDPRC_1",type = "number", decimalChar = ".")
                                                       , list(name = "HIGH_1",type = "number", decimalChar = ".")
                                                       , list(name = "LOW_1",type = "number", decimalChar = ".")
                                                       , list(name = "ACVOL_UNS", type = "number", decimalChar = ".")
                                                       , list(name = "OPEN_PRC", type = "number", decimalChar = ".")
                                                       , list(name = "BID", type = "number", decimalChar = ".")
                                                       , list(name = "ASK", type = "number", decimalChar = ".")
                                                       , list(name = "TRNOVR_UNS", type = "number", decimalChar = ".")
                                                       , list(name = "VWAP", type = "number", decimalChar = ".")
                                                       , list(name = "BLKCOUNT", type = "number", decimalChar = ".")
                                                       , list(name = "BLKVOLUM", type = "number", decimalChar = ".")
                                                       , list(name = "NUM_MOVES", type = "number", decimalChar = ".")
                                                       , list(name = "TRD_STATUS", type = "number", decimalChar = ".")
                                                       , list(name = "SALTIM", type = "number", decimalChar = ".")
                                                       , list(name = "NAVALUE", type = "number", decimalChar = ".")
                                                       , list(name = "VWAP_VOL", type = "number", decimalChar = "."))
                                        , data = list(list("2024-08-27", 228.03, 228.85, 224.89, 35934559,
                                                           225.995, 228.14, 228.15, 8185629556, 227.7513, 51,
                                                           11416370, 491852, 1, 72000, NULL, 28153632))))
     , qos = list(timeliness = "delayed"))

    test <- rd_OutputProcesser(input  , use_field_names_in_headers = TRUE, NA_cleaning = FALSE, SpaceConvertor = NULL)

    CorrectOutcome <- structure(list(universe = c("AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                  "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                  "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                  "AAPL.O", "AAPL.O")
                                     , Date = structure(c(19962, 19961, 19958,
                                                           19957, 19956, 19955, 19954, 19951, 19950, 19949, 19948, 19947,
                                                           19944, 19943, 19942, 19941, 19940, 19937, 19936, 19935), class = "Date"),
                                     TRDPRC_1 = c(228.03, 227.18, 226.84, 224.53, 226.4, 226.51,
                                                  225.89, 226.05, 224.72, 221.72, 221.27, 217.53, 216.24, 213.31,
                                                  209.82, 207.23, 209.27, 219.86, 218.36, 222.08)
                                     , HIGH_1 = c(228.85,227.28, 228.22, 228.34, 227.98, 227.17, 225.99, 226.8271,
                                                  225.35, 223.03, 221.89, 219.5099, 216.78, 214.2, 213.64,
                                                  209.99, 213.5, 225.6, 224.48, 223.82)
                                     , LOW_1 = c(224.89, 223.8905, 224.33, 223.9, 225.05, 225.45, 223.04, 223.6501,
                                                222.76, 219.7, 219.01, 215.6, 211.97, 208.83, 206.39, 201.07,
                                                196, 217.71, 217.02, 220.63)
                                     , ACVOL_UNS = c(35934559, 30602208,38677250, 43695321, 34765480, 30299033, 40687813, 44340240,
                                                     46414013, 41960574, 44155331, 38028092, 42201646, 47161149,
                                                     63516417, 69660488, 119548589, 105568560, 62500996, 50036262
                                                     )
                                     , OPEN_PRC = c(225.995, 226.76, 225.6589, 227.79, 226.52,
                                                    225.77, 225.72, 223.92, 224.6, 220.57, 219.01, 216.07, 212.1,
                                                    213.11, 206.9, 205.3, 199.09, 219.15, 224.37, 221.44)
                                     , BID = c(228.14,227.22, 226.73, 224.53, 226.35, 226.5, 225.89, 226.07, 224.71,
                                              221.59, 221.26, 217.53, 216.3, 213.34, 209.81, 207.11, 209.17,
                                              219.8, 218.26, 221.99)
                                     , ASK = c(228.15, 227.26, 226.79, 224.54, 226.39, 226.52, 225.92, 226.1, 224.72, 221.62, 221.27, 217.6,
                                              216.31, 213.39, 209.87, 207.14, 209.3, 219.85, 218.3, 222.02)
                                     , TRNOVR_UNS = c(8185629556, 6918040853, 8763518551, 9852330095,
                                                     7872506188, 6863636658, 9148975381, 9995644943, 10422279814,
                                                     9294719678, 9752841920, 8272879342, 9081788822, 10027425280,
                                                     13382167507, 14401602171, 24842394979, 23425004933, 13716031825,
                                                     11124841898)
                                     , VWAP = c(227.7513, 225.9286, 226.5552, 225.6868,
                                                226.4466, 226.5423, 224.7369, 225.4064, 224.5537, 221.495,
                                                220.991, 217.5629, 215.1614, 212.5627, 211.0548, 206.5738,
                                                208.1691, 222.3303, 219.5743, 222.394)
                                     , BLKCOUNT = c(51,41, 47, 54, 69, 46, 68, 75, 63, 79, 77, 47, 57, 84, 118,
                                                    99, 133, 122, 102, 76)
                                     , BLKVOLUM = c(11416370, 7628281, 11158110,
                                                    14384685, 10364824, 7844015, 13105076, 17560109, 16142449,
                                                    12287469, 15242348, 8831416, 10196037, 12751082, 19200973,
                                                    15948484, 21300770, 27568046, 12925524, 14235610)
                                     , NUM_MOVES = c(491852,
                                                    565404, 555154, 586190, 506656, 498797, 606363, 562638, 590575,
                                                    568519, 553265, 602530, 589469, 628773, 741724, 921953, 1661550,
                                                    1132553, 876129, 668901)
                                     , TRD_STATUS = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
                                     , SALTIM = c(72000, 72000, 72000, 72900, 72000, 72000, 72000, 72000, 72000, 72000,
                                                  72900, 72000, 72000, 72000, 72900, 72000, 72000, 72000, 72000,
                                                  72000)
                                     , NAVALUE = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                     , VWAP_VOL = c(28153632, 25868715, 33599651, 33958811, 28963442, 25490741, 35908387,
                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
                                , row.names = c(NA, -20L), class = c("data.table", "data.frame"))

    expect_equal(CorrectOutcome, test)
})

