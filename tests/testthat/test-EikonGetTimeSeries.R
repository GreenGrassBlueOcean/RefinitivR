test_that( "",{ expect_equal(1,1)
})

testthat::test_that("Check EikonGetTimeseries can handle fields = NULL"
                    , {
                      Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM", "III.L"),
                                                                 fields = NULL,
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00"))

                      GoodCheckEikonTimeSeries <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800,
                                                                                    1578355200, 1578441600, 1578528000,
                                                                                    1578614400, 1577923200, 1578009600,
                                                                                    1578268800, 1578355200, 1578441600,
                                                                                    1578528000, 1578614400)
                                                                                  , class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                                                 , Security = c("III.L", "III.L", "III.L", "III.L", "III.L", "III.L",
                                                                                "III.L", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM"))
                                                            , row.names = c(NA, -14L), class = "data.frame")


                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-1)}
)




testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with multiple rics"
                    , {
                      Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM", "III.L"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00"))

                      GoodCheckEikonTimeSeries <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800,
                                                                                    1578355200, 1578441600, 1578528000, 1578614400, 1577923200, 1578009600,
                                                                                    1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                  , class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                                                 , Security = c("III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                 , CLOSE = c(1116.5, 1108, 1088.5, 1086.5, 1088, 1094, 1088.5, 180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47)
                                                                 , HIGH = c(1123, 1110.5, 1102.5, 1101, 1089.5, 1096, 1100, 180.01, 178.66, 178.71, 178.51, 181.5, 181.59, 182.18)
                                                                 , LOW = c(1107.5, 1090.5, 1074, 1086.5, 1075, 1083, 1085, 177.1356, 175.63, 176.35, 176.82, 177.65, 179.76, 180.14)
                                                                 , OPEN = c(1108, 1110.5, 1101, 1096.5, 1079.5, 1090.5, 1100, 177.68, 177.02, 177.15, 178.28, 178, 181.51, 181.61)
                                                                 , VOLUME = c(723692, 717234, 813990, 1163565, 1451744, 975325, 2377611, 3448335, 2467310, 1997981, 2176615, 2758339, 2746346, 2103818))
                                                            , row.names =  c(NA, -14L), class = "data.frame")


                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-1)}
)

testthat::test_that("Check EikonGetTimeseries returns also raw data when requested"
                    , {
                      Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM", "III.L"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00",
                                                                 raw_output = TRUE
                                                                 ))

                      GoodCheckEikonTimeSeries <- list(list(timeseriesData = list(list(dataPoints = list(list("2020-01-02T00:00:00Z", 3448335L, 180.01, 177.1356, 177.68, 180L),
                                                                                                         list("2020-01-03T00:00:00Z", 2467310L, 178.66, 175.63, 177.02, 178.45),
                                                                                                         list("2020-01-06T00:00:00Z", 1997981L, 178.71, 176.35, 177.15, 178.62),
                                                                                                         list("2020-01-07T00:00:00Z", 2176615L, 178.51, 176.82, 178.28, 177.9),
                                                                                                         list("2020-01-08T00:00:00Z", 2758339L, 181.5, 177.65, 178L, 180.63),
                                                                                                         list("2020-01-09T00:00:00Z", 2746346L, 181.59, 179.76, 181.51, 181.2),
                                                                                                         list("2020-01-10T00:00:00Z", 2103818L, 182.18, 180.14, 181.61, 180.47))
                                                                                       , fields = list(list(name = "TIMESTAMP", type = "DateTime"),
                                                                                                       list(name = "VOLUME", type = "Double"),
                                                                                                       list(name = "HIGH", type = "Double"),
                                                                                                       list(name = "LOW", type = "Double"),
                                                                                                       list(name = "OPEN",type = "Double"),
                                                                                                       list(name = "CLOSE", type = "Double")),
                                                                                       ric = "MMM", statusCode = "Normal"),
                                                                                  list(dataPoints = list(list("2020-01-02T00:00:00Z", 723692L, 1123L, 1107.5, 1108L, 1116.5)
                                                                                                         , list("2020-01-03T00:00:00Z", 717234L, 1110.5, 1090.5, 1110.5, 1108L)
                                                                                                         , list("2020-01-06T00:00:00Z", 813990L, 1102.5, 1074L, 1101L, 1088.5),
                                                                                                         list("2020-01-07T00:00:00Z", 1163565L, 1101L, 1086.5, 1096.5, 1086.5),
                                                                                                         list("2020-01-08T00:00:00Z", 1451744L, 1089.5, 1075L, 1079.5, 1088L),
                                                                                                         list("2020-01-09T00:00:00Z", 975325L, 1096L, 1083L, 1090.5, 1094L),
                                                                                                         list("2020-01-10T00:00:00Z", 2377611L, 1100L, 1085L, 1100L, 1088.5)),
                                                                                       fields = list(list(name = "TIMESTAMP", type = "DateTime"),
                                                                                                     list(name = "VOLUME", type = "Double"),
                                                                                                     list(name = "HIGH", type = "Double"),
                                                                                                     list(name = "LOW", type = "Double"),
                                                                                                     list(name = "OPEN", type = "Double"), list(name = "CLOSE", type = "Double")),
                                                                                         ric = "III.L", statusCode = "Normal"))))


                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-1)}
)





testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only one ric"
                    , {Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM"),
                                                                 verbose = TRUE,
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00"))

                      GoodCheckEikonTimeSeries <-structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                 , class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                                                , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                , CLOSE = c(180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47)
                                                                , HIGH = c(180.01, 178.66, 178.71, 178.51, 181.5, 181.59, 182.18)
                                                                , LOW = c(177.1356, 175.63, 176.35, 176.82, 177.65, 179.76, 180.14)
                                                                , OPEN = c(177.68, 177.02, 177.15, 178.28, 178, 181.51, 181.61)
                                                                , VOLUME = c(3448335, 2467310, 1997981, 2176615, 2758339, 2746346, 2103818))
                                                           , row.names =  c(NA, -7L), class = "data.frame")

                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-2)
                      }
)



testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only one ric and one field"
                    , {Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00",
                                                                 fields = "CLOSE")
                      )

                      GoodCheckEikonTimeSeries <-structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                 , class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                                                , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                , CLOSE = c(180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47))
                                                           , row.names = c(NA, 7L), class = "data.frame")

                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 5e-2)}
)

testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only 2 rics and one field"
                    , {Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM", "III.L"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00",
                                                                 fields = "CLOSE")
                      )

                      GoodCheckEikonTimeSeries <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400, 1577923200, 1578009600,
                                                                                    1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                  , class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                                                 , Security = c("III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                 , CLOSE = c(1116.5, 1108, 1088.5, 1086.5, 1088, 1094, 1088.5, 180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47))
                                                            , row.names = c(NA, -14L), class = "data.frame")
                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-1)}
)


testthat::test_that("Check EikonGetTimeseries returns previously downloaded long timeseries"
                    , {Eikon <- check_Eikonapi()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("AAPL.O"),
                                                                 start_date = "2000-07-28T01:00:00",
                                                                 end_date = "2010-07-28T23:59:00",
                                                                 corax = "adjusted"
                      )
                      )


                      testthat::expect_identical(lapply(CheckTimeSeries, class),list(Date = c("POSIXct", "POSIXt"), Security = "character", CLOSE = "numeric",
                                                                                     HIGH = "numeric", LOW = "numeric", OPEN = "numeric", VOLUME = "numeric") )
                      testthat::expect_identical(nrow(CheckTimeSeries), 2513L)
                      testthat::expect_identical(min(CheckTimeSeries$Date), structure(965001600, class = c("POSIXct", "POSIXt"), tzone = "GMT"))
                      testthat::expect_identical(max(CheckTimeSeries$Date), structure(1280275200, class = c("POSIXct", "POSIXt"), tzone = "GMT"))
                      testthat::expect_equal(min(CheckTimeSeries$CLOSE), 0.2342855, tolerance = 1e-6)
                      testthat::expect_equal(max(CheckTimeSeries$CLOSE), 9.788347, tolerance = 1e-6)
                      testthat::expect_equal(min(CheckTimeSeries$VOLUME), 39466711, tolerance = 1e-6)
                      testthat::expect_equal(max(CheckTimeSeries$VOLUME), 7425523426, tolerance = 1e-6)
                      testthat::expect_equal(min(CheckTimeSeries$HIGH), 0.2355355, tolerance = 1e-6)
                      testthat::expect_equal(max(CheckTimeSeries$HIGH),  9.964633, tolerance = 1e-6)
                      testthat::expect_equal(min(CheckTimeSeries$LOW), 0.2271426, tolerance = 1e-6)
                      testthat::expect_equal(max(CheckTimeSeries$LOW), 9.696419, tolerance = 1e-6)
                      testthat::expect_equal(min(CheckTimeSeries$OPEN), 0.2319641, tolerance = 1e-6)
                      testthat::expect_equal(max(CheckTimeSeries$OPEN), 9.91749, tolerance = 1e-6)
                      testthat::expect_equal(unique(CheckTimeSeries$Security), "AAPL.O")
                    }
)







testthat::test_that("Check EikonGetTimeseries fails with empty ric list"
                    , {Eikon <- check_Eikonapi()

                      expect_warning({ checkEikonTS <- Refinitiv::EikonGetTimeseries( EikonObject = Eikon
                                                                                      , rics = c()
                                                                                      , interval= "daily"
                                                                                      , calender = "tradingdays"
                                                                                      , fields = c("TIMESTAMP","VOLUME","HIGH","LOW","OPEN","CLOSE")
                                                                                      , start_date =  "2020-07-21T01:00:00"
                                                                                      , end_date =  "2020-07-28T01:00:00")})

                      Expected_outcome <- structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame")
                      expect_equal(checkEikonTS, Expected_outcome)

                    })



testthat::test_that("Check EikonGetTimeseries works with wrong ric in list"
                    , {Eikon <- check_Eikonapi()

                      Correct_timeseries <- structure(list(Date = structure(c(1595376000, 1595462400, 1595548800, 1595808000, 1595894400), class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                                           , Security = c("MMM", "MMM", "MMM", "MMM", "MMM")
                                                           , CLOSE = c(158.71, 159.29, 159.84, 163.24, 155.68)
                                                           , HIGH = c(159.93, 159.65, 161.6, 163.38, 157.48)
                                                           , LOW = c(157.02, 158.36, 158.7249, 159.33, 153.8)
                                                           , OPEN = c(157.43, 159, 160.05, 159.53, 155.47)
                                                           , VOLUME = c(1899582, 2695122, 2524854, 3198457, 5362971))
                                                      , row.names = c(NA, 5L), class = "data.frame")

                      testtimeseries <- Refinitiv::EikonGetTimeseries( EikonObject = Eikon
                                                                       , rics = c("wrongRic", "MMM")
                                                                       , interval= "daily"
                                                                       , calender = "tradingdays"
                                                                       , fields = c("TIMESTAMP","VOLUME","HIGH","LOW","OPEN","CLOSE")
                                                                       , start_date =  "2020-07-21T01:00:00"
                                                                       , end_date =  "2020-07-28T01:00:00")

                      expect_equivalent(testtimeseries, Correct_timeseries, tolerance = 1e0)

                    })





testthat::test_that("EikonGetTimeseries satisfies corax conditions and can switch", {

  Eikon <- check_Eikonapi()
  AdjustedTS <- Refinitiv::EikonGetTimeseries(EikonObject = Eikon
                                , rics = c("AIRP.PA")
                                , start_date = as.Date("2019-01-01")
                                , end_date = as.Date("2019-01-10")
                                , corax = "adjusted"
                                )

  Correct_AdjustedTS <- structure(list( Date = structure(c(1546387200, 1546473600, 1546560000, 1546819200, 1546905600, 1546992000, 1547078400), class = c("POSIXct","POSIXt"), tzone = "GMT")
                                      , Security = c("AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA")
                                      , CLOSE = c(87.6859679504141,86.0743973801661, 88.471092074381, 87.9752242066124, 88.0991911735546,86.9008438264472, 85.950430413224)
                                      , HIGH = c(88.1818358181827,87.3140670495876, 88.7603483305794, 89.0909269090918, 89.214893876034,88.429769752067, 86.2810089917364)
                                      , LOW = c(87.0661331157033, 86.0743973801661, 86.5289429256207, 87.3967116942157, 87.7272902727282, 86.9008438264472, 85.2479509338851)
                                      , OPEN = c(88.1405134958686, 87.0248107933893, 86.8595215041331, 88.8016706528934, 87.7272902727282, 88.1405134958686, 86.1157197024802)
                                      , VOLUME = c(823047.885390415,940611.461877698, 942497.85150042, 725342.824931428, 927841.124431766, 1275431.70491365, 945878.590824272))
                                  , row.names = c(NA, -7L), class = "data.frame")
  testthat::expect_equivalent(AdjustedTS, Correct_AdjustedTS, tolerance = 1e-4)

  UnAdjustedTS <- Refinitiv::EikonGetTimeseries(EikonObject = Eikon
                                               , rics = c("AIRP.PA")
                                               , start_date = as.Date("2019-01-01")
                                               , end_date = as.Date("2019-01-10")
                                               , corax = "unadjusted"
                                               )

  Correct_UnAdjustedTS <- structure(list( Date = structure(c(1546387200, 1546473600, 1546560000, 1546819200, 1546905600, 1546992000, 1547078400), class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                        , Security = c("AIRP.PA", "AIRP.PA","AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA")
                                        , CLOSE = c(106.1, 104.15, 107.05, 106.45, 106.6, 105.15, 104)
                                        , HIGH = c(106.7, 105.65, 107.4, 107.8, 107.95, 107, 104.4)
                                        , LOW = c(105.35, 104.15, 104.7, 105.75, 106.15, 105.15, 103.15)
                                        , OPEN = c(106.65, 105.3,105.1, 107.45, 106.15, 106.65, 104.2)
                                        , VOLUME = c(680205L, 777365L,778924L, 599457L, 766811L, 1054076L, 781718L)), row.names = c(NA, -7L), class = "data.frame")
  testthat::expect_equivalent(UnAdjustedTS, Correct_UnAdjustedTS, tolerance = 1e-4)

 })


## TEST CASE PROBLEM ---

test_that( "empty downloaded data.frame can be processed", {

  Eikon <- check_Eikonapi()
  EikonTimeseries <- EikonGetTimeseries(EikonObject = Eikon
                                        , rics = list("ATM.NZ")
                                        , start_date = "2006-02-01T01:00:00"
                                        , end_date = "2006-02-20T01:00:00"
  )


  expect_equivalent(EikonTimeseries,structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame") )

})


test_that( "empty downloaded data.frame can be processed", {

  Eikon <- check_Eikonapi()
  EikonTimeseries <- EikonGetTimeseries(EikonObject = Eikon
                                        , rics = list("ATM.NZ", "MMM")
                                        , start_date = "2006-02-01T01:00:00"
                                        , end_date = "2006-02-20T01:00:00"
  )

  EikonTimeseriesCorrect <- structure(list( Date = structure(c(1138838400, 1138924800, 1139184000, 1139270400, 1139356800, 1139443200, 1139529600, 1139788800
                                                               , 1139875200, 1139961600, 1140048000, 1140134400), class = c("POSIXct", "POSIXt"), tzone = "GMT")
                                          , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                          , CLOSE = c(72.15, 71.1, 71.17, 70.65, 71, 72.12, 72.61, 72.91, 73.7, 73.13, 73.05, 73.77)
                                          , HIGH = c(73.68, 72.09, 71.3, 71.29, 71.28, 72.88, 72.99, 73.42, 74.12, 74.18, 73.35, 74.15)
                                          , LOW = c(72.05, 71, 70.99, 70.57, 70.3, 71.25, 71.5, 72.64, 73.41, 73.09, 72.5, 73.08)
                                          , OPEN = c(73.16, 71.75, 71, 71.16, 70.65, 71.36, 72.29, 72.69, 73.5, 73.71, 73.2, 73.15)
                                          , VOLUME = c(3207100, 3258300, 2551500, 3133200, 2846100, 3798100, 2932400, 2310200, 3859400, 2672900, 2923000, 2338600))
                                      , row.names = c(NA, -12L), class = "data.frame")


  expect_equivalent(EikonTimeseries,EikonTimeseriesCorrect )

})


test_that("one wrong ric does not blow it for the rest in EikonGetTimeseries", {

  #"FBHS.K" does not exist for this timerange
  Eikon <- check_Eikonapi()
  requireNamespace("lubridate", quietly = TRUE)
  suppressWarnings(test_problem_ts <- EikonGetTimeseries( start_date = paste0(Sys.Date()-lubridate::years(20), "T01:00:00")
                                                        , end_date =  paste0(Sys.Date()-lubridate::years(10), "T23:59:00")
                                                        , rics = c("FORTUM.HE", "WrongRIC", "0656.HK")
                                                        , EikonObject = Eikon
                                                        , raw_output = FALSE
  ))

  expect_equal(lapply(test_problem_ts, class), list(Date = c("POSIXct", "POSIXt"), Security = "character", CLOSE = "numeric",HIGH = "numeric", LOW = "numeric", OPEN = "numeric", VOLUME = "numeric") )
  expect_false("WrongRIC" %in% test_problem_ts$Security)


})


test_that("monthly economic timeseries can be downloaded", {

  requireNamespace("lubridate", quietly = TRUE)
  Eikon <- check_Eikonapi()

  testEconSeries <- EikonGetTimeseries(EikonObject = Eikon, rics = "USCPI=ECI", start_date = paste0(Sys.Date()-lubridate::years(5), "T01:00:00")
                                       , interval = "monthly", fields = c())
  expect_equal(lapply(testEconSeries, class), list(Date = c("POSIXct", "POSIXt"), Security = "character", VALUE = "numeric") )
  expect_true("USCPI=ECI" == unique(testEconSeries$Security))


  }

  )


test_that("works fields = NULL", {
  requireNamespace("lubridate", quietly = TRUE)
  Eikon <- check_Eikonapi()

  testEconSeries <- EikonGetTimeseries(EikonObject = Eikon, rics = "USCPI=ECI", start_date = paste0(Sys.Date()-lubridate::years(5), "T01:00:00")
                                       , interval = "monthly", fields = NULL)
  expect_equal(lapply(testEconSeries, class), list(Date = c("POSIXct", "POSIXt"), Security = "character", VALUE = "numeric") )
  expect_true("USCPI=ECI" == unique(testEconSeries$Security))


}

)

