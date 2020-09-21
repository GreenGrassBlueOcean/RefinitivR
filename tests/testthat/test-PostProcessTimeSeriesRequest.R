test_that("PostProcessTimeSeriesRequest works", {

  RawTimeSeries <- list(list(timeseriesData = list(list(dataPoints = list(list(180L, "2020-01-02T00:00:00Z"), list(178.45, "2020-01-03T00:00:00Z"),
                                                                          list(178.62, "2020-01-06T00:00:00Z"), list(177.9, "2020-01-07T00:00:00Z"),
                                                                          list(180.63, "2020-01-08T00:00:00Z"), list(181.2, "2020-01-09T00:00:00Z"),
                                                                          list(180.47, "2020-01-10T00:00:00Z"))
                                                        , fields = list(list(name = "CLOSE", type = "Double")
                                                                        , list(name = "TIMESTAMP", type = "DateTime")), ric = "MMM", statusCode = "Normal"))))

  expected_outcome <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                      , class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                     , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                     , CLOSE = c(180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47))
                                , row.names = c(NA, -7L), class = "data.frame")


  expect_identical(PostProcessTimeSeriesRequest(RawTimeSeries), expected_outcome)


})


test_that("PostProcessTimeSeriesRequest works with empty result", {

  expect_identical( PostProcessTimeSeriesRequest(RawTimeSeriesRequest = list(NA))
                  , structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame")
                  )

})


test_that("PostProcessTimeSeriesRequest works with one wrong RIC", {


  RawTimeSeries <- list(list(timeseriesData = list(list( dataPoints = NULL
                                    , errorCode = "TSIError"
                                    , errorMessage = "Error: TSIUnknownInstrument, ErrorCode: TA-TSIUnknownInstrument, Fault: TSIError, Description: Invalid RIC"
                                    , ric = "wrongRic"
                                    , statusCode = "Error"
                                    )
                                , list(dataPoints = list( list("2020-07-22T00:00:00Z", 1899582L, 159.93, 157.02, 157.43, 158.71)
                                                        , list("2020-07-23T00:00:00Z", 2695122L, 159.65, 158.36, 159L, 159.29)
                                                        , list("2020-07-24T00:00:00Z", 2524854L, 161.6, 158.7249, 160.05, 159.84)
                                                        , list("2020-07-27T00:00:00Z", 3198457L, 163.38, 159.33, 159.53, 163.24)
                                                        , list("2020-07-28T00:00:00Z", 7738796L, 157.48, 153.8, 155.47, 155.33))
                                       , fields = list( list(name = "TIMESTAMP", type = "DateTime")
                                                      , list(name = "VOLUME", type = "Double")
                                                      , list(name = "HIGH", type = "Double")
                                                      , list(name = "LOW", type = "Double")
                                                      , list(name = "OPEN", type = "Double")
                                                      , list(name = "CLOSE", type = "Double"))
                                       , ric = "MMM"
                                       , statusCode = "Normal"))
          ))

  expected_outcome <- structure(list( Date = structure(c(1595376000, 1595462400, 1595548800, 1595808000, 1595894400), class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                    , Security = c("MMM", "MMM", "MMM", "MMM", "MMM")
                                    , CLOSE = c(158.71, 159.29, 159.84, 163.24, 155.33)
                                    , HIGH = c(159.93, 159.65, 161.6, 163.38, 157.48)
                                    , LOW = c(157.02, 158.36, 158.7249, 159.33, 153.8)
                                    , OPEN = c(157.43, 159, 160.05, 159.53, 155.47)
                                    , VOLUME = c(1899582L, 2695122L, 2524854L, 3198457L, 7738796L))
                                , row.names = c(NA, -5L), class = "data.frame")


  expect_identical(PostProcessTimeSeriesRequest(RawTimeSeries), expected_outcome)

})
