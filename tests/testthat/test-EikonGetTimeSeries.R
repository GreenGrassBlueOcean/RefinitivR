test_that( "",{ expect_equal(1,1)
})

check_Eikonapi <- function() {
  if (is.null(getOption(".EikonApiKey"))) {
    skip("API not available")
  }
  print("Eikon API available performing test")
}


testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with multiple rics"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
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


                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-2)}
)



testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only one ric"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00"))

                      GoodCheckEikonTimeSeries <-structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                 , class = c("POSIXct", "POSIXt"))
                                                                , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                , CLOSE = c(180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47)
                                                                , HIGH = c(180.01, 178.66, 178.71, 178.51, 181.5, 181.59, 182.18)
                                                                , LOW = c(177.1356, 175.63, 176.35, 176.82, 177.65, 179.76, 180.14)
                                                                , OPEN = c(177.68, 177.02, 177.15, 178.28, 178, 181.51, 181.61)
                                                                , VOLUME = c(3448335, 2467310, 1997981, 2176615, 2758339, 2746346, 2103818))
                                                           , row.names =  c(NA, -7L), class = "data.frame")

                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-2)}
)



testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only one ric and one field"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00",
                                                                 fields = "CLOSE")
                      )

                      GoodCheckEikonTimeSeries <-structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                 , class = c("POSIXct", "POSIXt"))
                                                                , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                , CLOSE = c(180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47))
                                                           , row.names = c(NA, 7L), class = "data.frame")

                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-2)}
)

testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only 2 rics and one field"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM", "III.L"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00",
                                                                 fields = "CLOSE")
                      )

                      GoodCheckEikonTimeSeries <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400, 1577923200, 1578009600,
                                                                                    1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                  , class = c("POSIXct", "POSIXt"))
                                                                 , Security = c("III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                 , CLOSE = c(1116.5, 1108, 1088.5, 1086.5, 1088, 1094, 1088.5, 180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47))
                                                            , row.names = c(NA, -14L), class = "data.frame")
                      testthat::expect_equal(CheckTimeSeries, GoodCheckEikonTimeSeries, tolerance = 1e-2)}
)


testthat::test_that("Check EikonGetTimeseries returns previously downloaded long timeseries"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
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
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

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
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      Correct_timeseries <- structure(list(Date = structure(c(1595376000, 1595462400, 1595548800, 1595808000, 1595894400), class = c("POSIXct", "POSIXt"))
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

  check_Eikonapi()
  Eikon <- Refinitiv::EikonConnect()
  AdjustedTS <- Refinitiv::EikonGetTimeseries(EikonObject = Eikon
                                , rics = c("AIRP.PA")
                                , start_date = as.Date("2019-01-01")
                                , end_date = as.Date("2019-01-10")
                                , corax = "adjusted"
                                )

  Correct_AdjustedTS <- structure(list(Date = structure(c(1546387200, 1546473600, 1546560000, 1546819200, 1546905600, 1546992000, 1547078400)
                                                         , class = c("POSIXct","POSIXt"), tzone = "GMT")
                                        , Security = c("AIRP.PA", "AIRP.PA","AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA")
                                        , CLOSE = c(96.4545551,94.68182765, 97.31819155, 96.77273695, 96.9091006, 95.59091865, 94.545464)
                                        , HIGH = c(97.0000097, 96.04546415, 97.6363734, 98.0000098, 98.13637345, 97.272737, 94.9091004)
                                        , LOW = c(95.77273685, 94.68182765, 95.1818277, 96.13637325, 96.50000965, 95.59091865, 93.77273665)
                                        , OPEN = c(96.95455515, 95.7272823, 95.5454641, 97.68182795, 96.50000965, 96.95455515, 94.7272822)
                                        , VOLUME = c(748225.425177458,855101.414489859, 856816.314318369, 659402.634059737, 843492.015650798, 1159483.48405165, 859889.714011029))
                                   , row.names = c(NA, -7L), class = "data.frame")
  testthat::expect_equivalent(AdjustedTS, Correct_AdjustedTS, tolerance = 1e-4)

  UnAdjustedTS <- Refinitiv::EikonGetTimeseries(EikonObject = Eikon
                                               , rics = c("AIRP.PA")
                                               , start_date = as.Date("2019-01-01")
                                               , end_date = as.Date("2019-01-10")
                                               , corax = "unadjusted"
                                               )

  Correct_UnAdjustedTS <- structure(list( Date = structure(c(1546387200, 1546473600, 1546560000, 1546819200, 1546905600, 1546992000, 1547078400), class = c("POSIXct",  "POSIXt"), tzone = "GMT")
                                        , Security = c("AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA", "AIRP.PA")
                                        , CLOSE = c(106.1, 104.15, 107.05, 106.45, 106.6, 105.15, 104)
                                        , HIGH = c(106.7,105.65, 107.4, 107.8, 107.95, 107, 104.4)
                                        , LOW = c(105.35, 104.15, 104.7, 105.75, 106.15, 105.15, 103.15)
                                        , OPEN = c(106.65, 105.3, 105.1, 107.45, 106.15, 106.65, 104.2)
                                        , VOLUME = c(680205L, 777365L, 778924L, 599457L, 766811L, 1054076L, 781718L))
                                    , row.names = c(NA, -7L), class = "data.frame")

  testthat::expect_equivalent(UnAdjustedTS, Correct_UnAdjustedTS, tolerance = 1e-4)

  AdjustedTS <- Refinitiv::EikonGetTimeseries(EikonObject = Eikon
                                              , rics = c("AIRP.PA")
                                              , start_date = as.Date("2019-01-01")
                                              , end_date = as.Date("2019-01-10")
                                              , corax = "adjusted"
  )

  testthat::expect_equivalent(AdjustedTS, Correct_AdjustedTS, tolerance = 1e-4)

  })


## TEST CASE PROBLEM ---

test_that( "empty downloaded data.frame can be processed", {

  check_Eikonapi()
  Eikon <- Refinitiv::EikonConnect()
  EikonTimeseries <- EikonGetTimeseries(EikonObject = Eikon
                                        , rics = list("ATM.NZ")
                                        , start_date = "2006-02-01T01:00:00"
                                        , end_date = "2006-02-20T01:00:00"
  )


  expect_equivalent(EikonTimeseries,structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame") )

})


test_that( "empty downloaded data.frame can be processed", {

  check_Eikonapi()
  Eikon <- Refinitiv::EikonConnect()
  EikonTimeseries <- EikonGetTimeseries(EikonObject = Eikon
                                        , rics = list("ATM.NZ", "MMM")
                                        , start_date = "2006-02-01T01:00:00"
                                        , end_date = "2006-02-20T01:00:00"
  )

  EikonTimeseriesCorrect <- structure(list(Date = structure(c(1138838400, 1138924800, 1139184000, 1139270400, 1139356800, 1139443200, 1139529600, 1139788800, 1139875200, 1139961600, 1140048000, 1140134400), class = c("POSIXct", "POSIXt"))
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
  check_Eikonapi()
  requireNamespace("lubridate", quietly = TRUE)
  Eikon <- Refinitiv::EikonConnect()
  expect_warning(test_problem_ts <- EikonGetTimeseries( start_date = paste0(Sys.Date()-lubridate::years(20), "T01:00:00")
                                                        , end_date =  paste0(Sys.Date()-lubridate::years(10), "T23:59:00")
                                                        , rics = c("FORTUM.HE", "FBHS.K", "0656.HK")
                                                        , EikonObject = Eikon
                                                        , raw_output = FALSE
  ))

  expect_equal(lapply(test_problem_ts, class), list(Date = c("POSIXct", "POSIXt"), Security = "character", CLOSE = "numeric",HIGH = "numeric", LOW = "numeric", OPEN = "numeric", VOLUME = "numeric") )
  expect_false("FBHS.K" %in% test_problem_ts$Security)


})

