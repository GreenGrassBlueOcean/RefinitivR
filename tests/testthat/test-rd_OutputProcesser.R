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

  expected <- structure(list(instrument = c("GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O")
                             , date = c("2022-10-05T00:00:00", "2022-10-06T00:00:00", "2022-10-07T00:00:00",
                                        "2022-10-10T00:00:00", "2022-10-11T00:00:00", "2022-10-12T00:00:00",
                                        "2022-10-13T00:00:00", "2022-10-14T00:00:00", "2022-10-17T00:00:00",
                                        "2022-10-18T00:00:00", "2022-10-19T00:00:00", "2022-10-20T00:00:00",
                                        "2022-10-21T00:00:00", "2022-10-24T00:00:00", "2022-10-25T00:00:00",
                                        "2022-10-26T00:00:00", "2022-10-27T00:00:00", "2022-10-28T00:00:00",
                                        "2022-10-31T00:00:00", "2022-11-01T00:00:00", "2022-11-02T00:00:00",
                                        "2022-11-03T00:00:00", "2022-11-04T00:00:00", "2022-10-05T00:00:00",
                                        "2022-10-06T00:00:00", "2022-10-07T00:00:00", "2022-10-10T00:00:00",
                                        "2022-10-11T00:00:00", "2022-10-12T00:00:00", "2022-10-13T00:00:00",
                                        "2022-10-14T00:00:00", "2022-10-17T00:00:00", "2022-10-18T00:00:00",
                                        "2022-10-19T00:00:00", "2022-10-20T00:00:00", "2022-10-21T00:00:00",
                                        "2022-10-24T00:00:00", "2022-10-25T00:00:00", "2022-10-26T00:00:00",
                                        "2022-10-27T00:00:00", "2022-10-28T00:00:00", "2022-10-31T00:00:00",
                                        "2022-11-01T00:00:00", "2022-11-02T00:00:00", "2022-11-03T00:00:00",
                                        "2022-11-04T00:00:00")
                             , TR.Close = c(102.22, 102.24, 99.57, 98.71,
                                            98.05, 98.3, 99.71, 97.18, 100.78, 101.39, 100.29, 100.53, 101.48,
                                            102.97, 104.93, 94.82, 92.6, 96.58, 94.66, 90.5, 87.07, 83.49,
                                            86.7, 132.09, 131.3, 120.76, 116.7, 115.86, 115, 119.6, 112.27,
                                            118.88, 119.67, 120.51, 121.94, 124.66, 125.99, 132.61, 128.96,
                                            131.76, 138.34, 134.97, 135.43, 132.19, 134.21, 141.56)
                             , TR.Open = c(100.69, 101.5, 100.65, 99.85, 98.25, 98.27, 95.93, 100.625, 99.52, 103.94,
                                           100.7, 100.82, 98.46, 102.09, 103.3, 96.76, 94.31, 92.53, 95.78,
                                           95.59, 90.91, 86.345, 85.51, 129.107, 132.2, 125.05, 120.37,
                                           115.61, 115.79, 109.71, 120.57, 115.18, 123.44, 118.79, 121.13,
                                           120.982, 125.08, 126.935, 128.69, 136.3, 131.04, 137.78, 138.11,
                                           138.5, 130.43, 139.86))
                        , row.names = c(NA, -46L), class = c("data.table", "data.frame"))

  expect_equal(actual, expected)

  actual_2 <- rd_OutputProcesser(response, use_field_names_in_headers = FALSE )

  expected_2 <- structure(list(Instrument = c("GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O",
                                            "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "GOOG.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O",
                                            "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O", "NVDA.O"
  ), Date = c("2022-10-05T00:00:00", "2022-10-06T00:00:00", "2022-10-07T00:00:00",
              "2022-10-10T00:00:00", "2022-10-11T00:00:00", "2022-10-12T00:00:00",
              "2022-10-13T00:00:00", "2022-10-14T00:00:00", "2022-10-17T00:00:00",
              "2022-10-18T00:00:00", "2022-10-19T00:00:00", "2022-10-20T00:00:00",
              "2022-10-21T00:00:00", "2022-10-24T00:00:00", "2022-10-25T00:00:00",
              "2022-10-26T00:00:00", "2022-10-27T00:00:00", "2022-10-28T00:00:00",
              "2022-10-31T00:00:00", "2022-11-01T00:00:00", "2022-11-02T00:00:00",
              "2022-11-03T00:00:00", "2022-11-04T00:00:00", "2022-10-05T00:00:00",
              "2022-10-06T00:00:00", "2022-10-07T00:00:00", "2022-10-10T00:00:00",
              "2022-10-11T00:00:00", "2022-10-12T00:00:00", "2022-10-13T00:00:00",
              "2022-10-14T00:00:00", "2022-10-17T00:00:00", "2022-10-18T00:00:00",
              "2022-10-19T00:00:00", "2022-10-20T00:00:00", "2022-10-21T00:00:00",
              "2022-10-24T00:00:00", "2022-10-25T00:00:00", "2022-10-26T00:00:00",
              "2022-10-27T00:00:00", "2022-10-28T00:00:00", "2022-10-31T00:00:00",
              "2022-11-01T00:00:00", "2022-11-02T00:00:00", "2022-11-03T00:00:00",
              "2022-11-04T00:00:00")
  , `Price Close` = c(102.22, 102.24, 99.57,
                      98.71, 98.05, 98.3, 99.71, 97.18, 100.78, 101.39, 100.29, 100.53,
                      101.48, 102.97, 104.93, 94.82, 92.6, 96.58, 94.66, 90.5, 87.07,
                      83.49, 86.7, 132.09, 131.3, 120.76, 116.7, 115.86, 115, 119.6,
                      112.27, 118.88, 119.67, 120.51, 121.94, 124.66, 125.99, 132.61,
                      128.96, 131.76, 138.34, 134.97, 135.43, 132.19, 134.21, 141.56)
  , `Price Open` = c(100.69, 101.5, 100.65, 99.85, 98.25, 98.27,
                     95.93, 100.625, 99.52, 103.94, 100.7, 100.82, 98.46, 102.09,
                     103.3, 96.76, 94.31, 92.53, 95.78, 95.59, 90.91, 86.345, 85.51,
                     129.107, 132.2, 125.05, 120.37, 115.61, 115.79, 109.71, 120.57,
                     115.18, 123.44, 118.79, 121.13, 120.982, 125.08, 126.935, 128.69,
                     136.3, 131.04, 137.78, 138.11, 138.5, 130.43, 139.86)
  ), row.names = c(NA, -46L), class = c("data.table", "data.frame"))

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

  input <- list(columnHeadersCount = 1L, data = list(list("MMM", "", "3M Co"),
                                                     list("III.L", 4.23463019323009, "3i Group PLC")), headerOrientation = "horizontal",
                headers = list(list(list(displayName = "Instrument"), list(
                  displayName = "P/E (Daily Time Series Ratio)", field = "TR.PE(SDATE=0D)/*P/E (LTM) - DILUTED EXCL*/"),
                  list(displayName = "Company Name", field = "TR.COMPANYNAME"))),
                rowHeadersCount = 1L, totalColumnsCount = 3L, totalRowsCount = 3L)

  use_field_names_in_headers <- TRUE

  actual <- rd_OutputProcesser(input, use_field_names_in_headers)


  expected <- structure(list( Instrument = c("MMM", "III.L")
                            , `TR.PE(SDATE=0D)/*P/E (LTM) - DILUTED EXCL*/` = c(0, 4.23463019323009)
                            , TR.COMPANYNAME = c("3M Co", "3i Group PLC"))
                        , row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  expect_equal(actual, expected)
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


  expected <- structure(list( Instrument = c("MMM", "III.L", "III.L")
                            , Date = c(NA, NA, "2023-10-20T00:00:00")
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

  use_field_names_in_headers <- TRUE

  actual <- rd_OutputProcesser(input, use_field_names_in_headers,NA_cleaning = FALSE)


  expected <-structure(list(instrument = c("MMM", "III.L", "III.L")
                            , date = c(NA, NA, "2023-10-20T00:00:00")
                            , TR.PE = c(NA, NA, 4.23463019323009)
                            , `Company Name` = c("3M Co", "3i Group PLC", NA))
                       , row.names = c(NA, -3L), class = c("data.table", "data.frame"))

  expect_equal(actual, expected)
})


