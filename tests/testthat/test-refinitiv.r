test_that( "",{ expect_equal(1,1)
  })

# Load required data for tests
load(file="testdata.rda")
#options( ".EikonApiKey" =  "Put your key here")




testthat::test_that("retry", {
  testthat::expect_equal(retry(retryfun = sum(1,1)), sum(1,1))
  testthat::expect_equal(retry(retryfun = sum(1,"a"), max = 1), NA)
})


# travis CI does not like this!
# testthat::test_that("check if install_eikon works", {
#   testthat::expect_equal(install_eikon(), "Eikon Python interface successfully installed")
# })


#As this package requires an api key testing is of api function is only possible when having an actual api key.
# Therefore best to run these after setting the EikonConnect function with the right password.


check_Eikonapi <- function() {
  if (is.null(getOption(".EikonApiKey"))) {
    skip("API not available")
  }
  print("Eikon API available performing test")
}


testthat::test_that("Check Eikon Connect is really a python object"
                   , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
                      testthat::expect_equal(class(Eikon), c("python.builtin.module", "python.builtin.object"),
                   )})



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





testthat::test_that("Check EikonGetData returns expected data with multiple rics"
                    , {check_Eikonapi()
                       Eikon <- Refinitiv::EikonConnect()
                       CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                                                          Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName")))

                       GoodCheckEikonData <- list( PostProcessedEikonGetData = structure(list(Instrument = c("MMM", "III.L")
                                                                                             , RDN_EXCHD2 = c("NYQ", "LSE")
                                                                                             , Company.Name = c("3M Co","3i Group PLC")
                                                                                             )
                                                 , class = "data.frame"
                                                 , row.names = c(NA, -2L))
                                                 , Eikon_Error_Data = structure(list()
                                                          , .Names = character(0), row.names = integer(0)
                                                          , class = "data.frame"))



                       testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
                      }
                      )


#add test case if only one ric is requested
testthat::test_that("Check EikonGetData returns expected data with only one ric"
                    , {check_Eikonapi()
                       Eikon <- Refinitiv::EikonConnect()

                       GoodCheckEikonData <- list(PostProcessedEikonGetData = structure(list(Instrument = "MMM", RDN_EXCHD2 = "NYQ", Company.Name = "3M Co")
                                                                                        , row.names = c(NA, -1L), class = "data.frame")
                                                  , Eikon_Error_Data = structure(list(), class = "data.frame", row.names = integer(0), .Names = character(0)))


                       CheckEikonData <- EikonGetData(EikonObject = Eikon, rics = c("MMM"),
                                                          Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"))


                       testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
                      }
 )

#add test case when 0 ric is requested
testthat::test_that("Check EikonGetData returns expected data with empty ric"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      CheckEikonData <- EikonGetData(EikonObject = Eikon, rics = c("wronric"),raw_output = FALSE
                                                    ,  Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"))


                      Correct_EikonData <- list(PostProcessedEikonGetData = structure(list(Instrument = "WRONRIC", RDN_EXCHD2 = NA, Company.Name = NA)
                                                                                      , row.names = c(NA, -1L), class = "data.frame")
                                                , Eikon_Error_Data = structure(list(code = c(251658243L, 416L), col = 1:2
                                                                                    , message = c("'The record could not be found' for the instrument 'WRONRIC'"
                                                                                                  , "Unable to collect data for the field 'TR.CompanyName' and some specific identifier(s).")
                                                                                    , row = c(0L, 0L)), row.names = c(NA, -2L), class = "data.frame"))
                      testthat::expect_identical(CheckEikonData, Correct_EikonData)
                    }
)







#add test case when not valid ric is requested
testthat::test_that("Check EikonGetData returns expected data with only one good ric and one wrong RIC"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      Correct_output <- list(PostProcessedEikonGetData = structure(list(Instrument = c("WRONGRIC",
                                                                                     "MMM"), RDN_EXCHD2 = c(NA, "NYQ"), Company.Name = c(NA, "3M Co"
                                                                                     )), class = "data.frame", row.names = c(NA, -2L)), Eikon_Error_Data = structure(list(
                                                                                       code = c(251658243L, 416L), col = 1:2, message = c("'The record could not be found' for the instrument 'WRONGRIC'",
                                                                                                                                          "Unable to collect data for the field 'TR.CompanyName' and some specific identifier(s)."
                                                                                       ), row = c(0, 0)), class = "data.frame", row.names = c(NA,
                                                                                                                                              -2L)))


                      CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("WRONGRIC", "MMM"),
                                                         Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName")))


                      testthat::expect_equal(CheckEikonData, Correct_output)
                    }
)





#add test case if only 2 rics and one field is requested
testthat::test_that("Check EikonGetData returns expected data with only 2 ric and one field"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      GoodCheckEikonData <- list(PostProcessedEikonGetData = structure(list(Instrument = c("MMM", "III.L")
                                                                                            , Company.Name = c("3M Co", "3i Group PLC")), class = "data.frame", row.names = c(NA, -2L))
                                                 , Eikon_Error_Data = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame"))

                      CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                                                         Eikonformulas = "TR.CompanyName"))


                      testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
                    }
)


#add test case if only 2 rics and one field is requested
testthat::test_that("Check EikonGetData returns expected data with only 2 ric and one field"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      GoodCheckEikonData <- list(PostProcessedEikonGetData = structure(list( Instrument = c("MMM", "III.L")
                                                                                            , CURRENCY = c("USD", "GBp")
                                                                                            , `Average.Daily.Volume.-.6.Months` = c(3465640L, 1993000L)
                                                                                            , Instrument.Type = c("Ordinary Shares", "Ordinary Shares")
                                                                                            , Exchange.Name = c("NO MARKET (E.G. UNLISTED)", "LONDON STOCK EXCHANGE")
                                                                                            , CF_EXCHNG = c("NYQ", "LSE")
                                                                                            , Exchange.Market.Identifier.Code = c("XXXX", "XLON")
                                                                                            , Instrument.Is.Active.Flag = c(TRUE, TRUE))
                                                                                       , row.names = c(NA, -2L), class = "data.frame")
                                                 , Eikon_Error_Data = structure(list(), class = "data.frame", row.names = integer(0), .Names = character(0)))


                      CheckEikonData <- try(EikonGetData( EikonObject = Eikon, rics = c("MMM", "III.L")
                                                        , Eikonformulas = c( "CURRENCY", "TR.AvgDailyVolume6M", "TR.InstrumentType", "TR.ExchangeName", "CF_EXCHNG", "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive")
                                                        , raw_output = FALSE
                      )
                      )


                      testthat::expect_equal(CheckEikonData, GoodCheckEikonData, tolerance = 1e-1)
                    }
)








#add test case if only 2 rics and one field is requested
testthat::test_that("Check EikonGetData returns expected data with only 2 ric and one field"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      GoodCheckEikonData <- list(PostProcessedEikonGetData = structure(list(Instrument = c("MMM", "III.L")
                                                                                            , Company.Name = c("3M Co", "3i Group PLC"))
                                                                                       , class = "data.frame", row.names = c(NA, -2L))
                                                 , Eikon_Error_Data = structure(list(), .Names = character(0), row.names = integer(0)
                                                                                , class = "data.frame"))

                      CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                                                         Eikonformulas = "TR.CompanyName"))


                      testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
                    }
)


#add test case if only 1 ric and one field is requested
testthat::test_that("Check EikonGetData returns expected data with only 1 ric and one field"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()

                      GoodCheckEikonData <- list(PostProcessedEikonGetData = structure(list(Instrument = "MMM", Company.Name = "3M Co")
                                                                                       , class = "data.frame", row.names = c(NA, -1L))
                                                 , Eikon_Error_Data = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame"))

                      CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("MMM"),
                                                         Eikonformulas = "TR.CompanyName"))


                      testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
                    }
)


#add test case if only 1 EikonGetData does not crash when only one wrong RIC is requested with one field
test_that("EikonGetData does not crash when only one wrong RIC is requested with one field" , {
  check_Eikonapi()
  Eikon <- Refinitiv::EikonConnect()

  GoodCheckEikonData <- list(PostProcessedEikonGetData = structure(list(V1 = "WrongRIC2", TR.COMPANYNAME = NA)
                                                                   , row.names = c(NA, -1L), class = "data.frame"),
                             Eikon_Error_Data = structure(list(code = 412L, col = 1L,
                                                               message = "Unable to resolve all requested identifiers.",
                                                               row = 0L), row.names = c(NA, -1L), class = "data.frame"))

  CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("WrongRIC2"),
                                     Eikonformulas = "TR.CompanyName", raw_output = FALSE))

  testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
})





## Test EikonNameCleaner ----

test_that("EikonShowAttributes returns an error when it should", {
  expect_error(EikonShowAttributes(EikonObject = NULL))
})

testthat::test_that("Check EikonShowAttributes returns expected vector of possibilities"
                    , { check_Eikonapi()
                        Eikon <- Refinitiv::EikonConnect()
                        test <- EikonShowAttributes(Eikon)
                        expect_identical(class(test), "character")
                        expect_true(is.vector(test))

                    }
)






## Test EikonNameCleaner ----

test_that("EikonNameCleaner satisfies testcases", {
  expect_equal(EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC")), c("Instrument","Company.Name","RDN_EXCHD2","Operating.MIC"))
  expect_equal(EikonNameCleaner(c("TR.ShortInterest(SDate=0D)/TR.SharesFreeFloat(SDate=0D)/*Short Interest as % of Float*/")), c("Short.Interest.as.%.of.Float"))
})


## Test EikonChunker ----

test_that("EikonChunker returns an error when it should", {
  expect_error(EikonChunker())
  expect_error(EikonChunker(RICS = rep('a', times = 200)))
  expect_error(EikonChunker(RICS = rep('a', times = 200), MaxCallsPerChunk = 12000, Duration = 12001))
})

test_that("EikonChunker satisfies testcases", {

  CorrectSolution <- lapply(1:8, FUN = function(x){rep("a", times = 25)})
  names(CorrectSolution) <-  c("1", "2", "3", "4", "5", "6", "7", "8")

  expect_equal( EikonChunker(RICS = rep('a', times = 200), MaxCallsPerChunk = 100, Eikonfields = c("TR.CompanyName","RDN_EXCHD2","TR.OperatingMIC", "TR.ISINCode"))
              , CorrectSolution
              )

  CorrectSolution <- NULL
})



test_that("EikonChunker satisfies testcases no split required due to MaxRicsperChunk", {

  expect_error({
  CorrectSolution <- list(`1` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `2` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `3` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `4` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `5` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `6` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `7` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `8` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `9` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `10` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `11` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `12` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `13` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
                         , `14` = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"))

  SplittedRics <-EikonChunker(RICS = rep('a', times = 200), Duration= 7, MaxCallsPerChunk = 100, MaxRicsperChunk = 20)}, NA)

  expect_equal( sum(unlist(lapply(SplittedRics, length))), 200)

  expect_equal( SplittedRics
              , CorrectSolution
  )




  CorrectSolution <- NULL
})


test_that("EikonChunker satisfies testcases split IS required due to MaxRicsperChunk", {

  expect_error({
    CorrectSolution <- list(`1` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `2` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `3` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `4` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `5` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `6` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `7` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `8` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `9` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `10` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `11` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `12` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `13` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `14` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `15` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `16` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `17` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `18` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `19` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `20` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `21` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `22` = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
                           , `23` = c("a", "a"))



    SplittedRics <-EikonChunker(RICS = rep('a', times = 200), Duration= 7, MaxCallsPerChunk = 100, MaxRicsperChunk = 10)}, NA)

  expect_equal( sum(unlist(lapply(SplittedRics, length))), 200)

  expect_equal( SplittedRics
                , CorrectSolution
  )




  CorrectSolution <- NULL
})


## test EikonPostProcessor ----


test_that("EikonPostProcessor satisfies testcases", {

  expect_equal(EikonPostProcessor(Eikon_get_dataOuput = StartTestEikonData), GoodOutcomeEikonPostProcessor)

  #load(file="./tests/testthat/testdata.rda")
  # ## add data for testing as:
  # # GoodCheckEikonData <- CheckEikonData
  #save(StartTestEikonData, GoodOutcomeEikonPostProcessor ,  file = "./tests/testthat/testdata.rda")


  expect_equal( EikonPostProcessor(Eikon_get_dataOuput = list(NULL))
              , list(PostProcessedEikonGetData = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame"),
                     Eikon_Error_Data = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame"))
              )

})


test_that("EikonPostProcessor can process empty strings without turning the entire column in to character", {

  testinput <- list(list(columnHeadersCount = 1L, data = list( list("LP68237734", "EUR", "", "Open-Ended Fund", "", "LIP", NULL, TRUE)
                                                             , list("NL0000280501=DAp", NULL, NULL, NULL, NULL, NULL, NULL, NULL)
                                                             , list("LP60073160^F20", NULL, "", "", "", NULL, NULL, ""))
                         , error = list( list(code = 251658243L, col = 1L, message = "'The record could not be found' for the instrument 'NL0000280501=DAp'", row = 1L)
                                        , list(code = 416L, col = 2L, message = "Unable to collect data for the field 'TR.AvgDailyVolume6M' and some specific identifier(s).", row = 1L)
                                        , list(code = 416L, col = 3L, message = "Unable to collect data for the field 'TR.InstrumentType' and some specific identifier(s).", row = 1L)
                                        , list(code = 416L, col = 4L, message = "Unable to collect data for the field 'TR.ExchangeName' and some specific identifier(s).", row = 1L)
                                        , list(code = 251658243L, col = 5L, message = "'The record could not be found' for the instrument 'NL0000280501=DAp'", row = 1L)
                                        , list(code = 416L, col = 6L, message = "Unable to collect data for the field 'TR.ExchangeMarketIdCode' and some specific identifier(s).", row = 1L)
                                        , list(code = 416L, col = 7L, message = "Unable to collect data for the field 'TR.InstrumentIsActive' and some specific identifier(s).", row = 1L)
                                        , list(code = 251658243L, col = 1L, message = "'The record could not be found' for the instrument 'LP60073160^F20'", row = 2L)
                                        , list(code = 251658243L, col = 5L, message = "'The record could not be found' for the instrument 'LP60073160^F20'", row = 2L))
                         , headerOrientation = "horizontal", headers = list(list(list(displayName = "Instrument"), list(displayName = "CURRENCY", field = "CURRENCY")
                                                                                 , list(displayName = "Average Daily Volume - 6 Months", field = "TR.AVGDAILYVOLUME6M")
                                                                                 , list(displayName = "Instrument Type", field = "TR.INSTRUMENTTYPE"), list(displayName = "Exchange Name", field = "TR.EXCHANGENAME"), list(displayName = "CF_EXCHNG", field = "CF_EXCHNG")
                                                                                 , list(displayName = "Exchange Market Identifier Code", field = "TR.EXCHANGEMARKETIDCODE"), list(displayName = "Instrument Is Active Flag", field = "TR.INSTRUMENTISACTIVE")))
                         , rowHeadersCount = 1L, totalColumnsCount = 8L, totalRowsCount = 4L))


  GoodOutcomeEikonPostProcessor <- list(PostProcessedEikonGetData = structure(list( Instrument = c("LP68237734", "NL0000280501=DAp", "LP60073160^F20"), CURRENCY = c("EUR", NA, NA)
                                                 , `Average.Daily.Volume.-.6.Months` = c(NA, NA, NA), Instrument.Type = c("Open-Ended Fund", NA, NA)
                                                 , Exchange.Name = c(NA, NA, NA), CF_EXCHNG = c("LIP", NA, NA)
                                                 , Exchange.Market.Identifier.Code = c(NA, NA, NA), Instrument.Is.Active.Flag = c(TRUE, NA, NA))
                                             , row.names = c(NA, -3L), class = "data.frame")
       , Eikon_Error_Data = structure(list(code = c(251658243L, 416L, 416L, 416L, 251658243L, 416L, 416L, 251658243L, 251658243L)
                                           , col = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L, 5L)
                                           , message = c("'The record could not be found' for the instrument 'NL0000280501=DAp'"
                                                         , "Unable to collect data for the field 'TR.AvgDailyVolume6M' and some specific identifier(s)."
                                                         , "Unable to collect data for the field 'TR.InstrumentType' and some specific identifier(s)."
                                                         , "Unable to collect data for the field 'TR.ExchangeName' and some specific identifier(s)."
                                                         , "'The record could not be found' for the instrument 'NL0000280501=DAp'"
                                                         , "Unable to collect data for the field 'TR.ExchangeMarketIdCode' and some specific identifier(s)."
                                                         , "Unable to collect data for the field 'TR.InstrumentIsActive' and some specific identifier(s)."
                                                         , "'The record could not be found' for the instrument 'LP60073160^F20'"
                                                         , "'The record could not be found' for the instrument 'LP60073160^F20'")
                                           , row = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L)), row.names = c(NA, -9L), class = "data.frame"))


  expect_equal(EikonPostProcessor(Eikon_get_dataOuput = testinput), GoodOutcomeEikonPostProcessor)

  # ## add data for testing as:
  # # GoodCheckEikonData <- CheckEikonData
  #save(StartTestEikonData, GoodOutcomeEikonPostProcessor ,  file = "./tests/testthat/testdata.rda")


  expect_equal( EikonPostProcessor(Eikon_get_dataOuput = list(NULL))
                , list(PostProcessedEikonGetData = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame"),
                       Eikon_Error_Data = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame"))
  )

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






test_that("CondaExists returns a logical" , {

  test <- CondaExists()

  expect_type(test, "logical")



})




test_that("EikonGetTimeseries can handle long requests gracefully", {

  check_Eikonapi()
  Eikon <- Refinitiv::EikonConnect()


 fields <- c("CURRENCY","TR.AvgDailyVolume6M","TR.InstrumentType","TR.ExchangeName","CF_EXCHNG","TR.ExchangeMarketIdCode","TR.InstrumentIsActive")

 Rics <- c("UMI.BR","UMIEUR.xbo","0RUY.L","NVJP.F","UMIb.BS","NVJP.DE","UMIb.CHI","UMIb.TQ","UMICF.PK","NVJP.BE","NVJP.SG","NVJP.MU","NVJP.H","NVJP.D","UMIN.MX","UMIb.ED","UMI.VI","UMIb.SIG","UMI.BN","UMI.xt"
         ,"UMI.BR1","UMIbr.TRE","UMICF.PQ","NVJP.TG","UMIN.BIV","UMIb.AQX","NVJP.DEU","UMIb.NXT","UMI.VIf","UMIbr.DAp","UMIbr.ICEM","UMIb.AQXd","NVJP.QTX","UMI.TX","UMIN.MCO","UMIb.MSF","BE0974320526.GTX"
         ,"UMIb.EDM","UMIb.BCU","UMIb.DXE","UMIbr.IGDM","UMIb.EDv","UMI.DE^E11","UMI.PAp^C18","UMI.SMp^J17","NVJN.VX^B09","ACUM.PA^L98","ACUM.F^D01","ACUM.DE^D01","ACUM.BR^G96","ACUMtT.BR^K91","ACUMtq.L^F99"
         ,"UMIEUR.PAp^B17","UMIEUR.DEp^A18","0GWH.L^E08","ACUM.DEU^C03","UMI.Sp^E18","UMI.SGp^L17","ACUMtbr.INS^G07","UMI.Sp^J18","UMIEUR.SGp^L17","UMIEUR.CHIp^E12","UMIEUR.MIp^L17","ACUM.BE^E03","UMIEUR.STp^J18"
         ,"UMIEUR.OLp^E10","UMIEUR.STp^H10","UMICFUSD.STp^I10","UMICFEUR.STp^J18","UMIbe.PZ^A12","UMI.PO^G04","UMIbr.STp^I20","UMIbr.ENp^H20","UMI.rEUR^J09","UMI.MB^L17","UMIEUR.Ip^G19","UMI.mGBP^L14","UMIEUR.PZ^A09"
         ,"UMIEUR.PZp^A09","0GWH.PO^G08","UMIf.INS^K08","UMIEUR.BUp^K11","UMIEUR.VIp^J18","UMIbr.NQX^G10","UMIEUR.PZp^B09","UMI.XR^L13","UMIb.QMF^A14","UMIb.TOM^F17","UMIb.BCO","UMIbr.TWEA","UMIbr.TRXA","UMIbr.TWEM"
         ,"DTEGn.DE","DTEGnEUR.xbo","DTEGn.F","DTEd.BS","DTEd.CHI","DTEGn.MU","DTEd.TQ","DTEGn.BE","DTEG.TFB","DTEGn.S","DTEGn.SG","DTEGn.D","DTEGn.H","DTEGn.HA","DTEGnEUR.S","DTEGF.PK","DTE.MI","DTEN.MX","DTEd.ED"
         ,"DTEd.SIG","DTEGn.BN","DTEGn.VI","DTE.BX","DTEG.BU","DTEGn.PR","DTEGn.xt","0MPH.L","DTEde.TRE","DTEG.TFB1","DTEGF.PQ","DTEtah.MI","DTEGn.TG","DTEN.BIV","DTEdl.BX","DTEd.AQX","DTEd.NXT","DTE.TI","DTEd.DXE"
         ,"DTEGn.DEU","DTEGn.VIf","DTEde.DAp","DTEde.ICEM","DTEd.EDM","DTEGFn.BCU","DTEd.BCU","DTEde.IGDM","1ASPDTEN.PIPB","1ADTEN.PIPB","DTEd.AQXd","DTE.QTX","DTEN.MCO","DTEd.MSF","DE0005557508.GTX","DTEG.BUf","DTEd.EDv"
         ,"DTEde.IEDM","DTEGn.EU^E08","DTEGq.L^B04","DTEGnq.L^K07","9496.T^G10","DTEGn.VX^B09","DTEGn.PAp^C18","DTEGn.SMp^J17","DTEG.T^A00","DTEG.F^A00","DTEG.ASx^G00","DTEG.ASOx^G00","EDTEG.EU^K99","DTEG.DE^A00","EDTEG.EU^E99"
         ,"DTEGnEUR.PAp^B17","DTEGnEUR.DEp^A18","O9496O.T^G10","DTEGq.L^J98","DTEGw.AS^J01","DTEG.ASO^G03","DTE.S^K08","DTEGn.SGp^L17","DTEG.BE^A00","DTEG.BM^A00","DTEG.D^A00","DTEG.H^A00","DTEG.HA^A00","DTEG.MU^A00","DTEG.SG^A00"
         ,"DTEGnEUR.SGp^L17","DTEGnEUR.CHIp^E12","DTEEUR.MIp^L17","DTEGn.Sp^E15","DTEGn.BM^D03","DTEGnEUR.OLp^E10","DTEGnEUR.STp^J18","DTEd.HFT^E14","DTEGn.NDT^K03","DTEGn.NDE^K03","DTEG.NA^G03","DTEGas.INS^K08","DTEs.INS^K08"
         ,"DTEG.DEU^A00","SAPG.DE","SAPGEUR.xbo","SAPG.F","SAPd.BS","SAPd.CHI","SAPd.TQ","SAPG.S","SAPG.BE","SAPG.SG","SAPG.D","SAPG.MU","SAPGF.PK","SAPG.H","SAPG.HA","SAP.MI","SAPGEUR.S","SAPd.ED","SAPd.SIG","SAPG.VI","SAPG.BN"
         ,"SAP.BX","SAPG.BU","SAPG.xt","0NW4.L","SAPde.TRE","SAPtah.MI","SAPGF.PQ","SAPG.TG","SAPd.NXT","SAPd.AQX","SAPG.TI","SAPd.DXE","SAPdl.BX","SAPG.DEU","SAPG.VIf","SAPde.DAp","SAPde.ICEM","SAPd.AQXd","SAP.QTX","SAPd.MSF"
         ,"DE0007164600.GTX","SAPde.IEDM","SAPd.EDM","SAPGFn.BCU","SAPd.BCU","SAPG.BUf","SAPde.IGDM","SAPd.EDv","SAPG.EU^E08","SAPGq.L^K07","SAPG.VX^B09","SAPG.PAp^C18","SAPG.SMp^J17","SAPD.CD^K00","SAPGEUR.PAp^B17","SAPGEUR.DEp^A18"
         ,"SAPG.S^B05","SAP.S^K08","SAPGEUR.SGp^L17","SAPGEUR.CHIp^E12","SAPEUR.MIp^L17","SAPG.Sp^E15","SAPG.BM^D03","SAPGEUR.OLp^E10","SAPGEUR.STp^J18","SAPG.NDT^K03","SAPd.HFT^E14","SAPG.TP^H01","SAPG.NDE^K03","SAPG.NA^G03","1ESAPGY.PIPB^E18"
         ,"1ESSAPGY.PIPB^E18","SAPG.IB^A98","SAPG.Z^L95","SAPG.rEUR^J09","SAPG.rGBP^K07","SAPG.MB^L17","SAPGEUR.Ip^G19","SAPG.mGBP^L14","SAPde.PZ^A12","SAPGEUR.PZp^B09","0H42.PO^L08","SAPGEUR.BUp^K11","SAPGEUR.VIp^J18","SAPGde.NQX^G10","SAP.D.JW^H02"
         ,"SAP.JW^G02","SAPG.G^K95","SAPd.QMF^A14","SAPd.BCO","SAPGFn.BCO","SAPde.TWEA","SAPde.TRXA","SAPde.STp","SAPde.ENp","SAPde.VIp","SAPde.TWEM","SAPde.BUp","ALVG.DE","ALVGEUR.xbo","ALVG.F","ALVd.BS","ALVd.CHI","ALVG.S","ALVG.BE","ALVG.SG","ALVG.D"
         ,"ALVG.MUx","ALVd.TQ","ALV.MI","ALVG.H","ALVG.HA","ALIZF.PK","ALVN.MX","ALVd.ED","ALVd.SIG","ALVG.VI","ALVG.BX","ALVG.BN","ALVG.BU","ALVG.xt","0M6S.L","ALVde.TRE","ALIZF.PQ","ALVGEUR.S","ALVtah.MI","ALVG.TG","ALVN.BIV","ALVGdl.BX","ALVd.AQX","ALVd.NXT"
         ,"ALV.TI","ALVd.DXE","ALVG.DEU","ALVG.VIf","ALVde.DAp","ALVde.ICEM","ALVd.EDM","ALVd.BCU","ALIZFn.BCU","ALVde.IGDM","1AALVN.PIPB","1ASPALVN.PIPB","ALV.QTX","ALVN.MCO","ALVd.MSF","DE0008404005.GTX","ALVG.BUf","ALVd.EDv","ALVd.AQXd","ALVG.EU^E08","ALVGq.L^K07"
         ,"ALVG.PA^D10","12820.PA^A04","ALVG.AS^K09","ALVG.VX^B09","GB5231485.L^C02","ALVG.PAp^C18","ALVG.SMp^J17","ALVG.L^H96","EALVG.EU^B99","ALVGEUR.PAp^B17","ALVGEUR.DEp^A18","ALVGq.L^A10","ALVGq.L^B98","ALVG.ASO^G03","ALVGw.AS^J01","GB5231485.L^F03","ALLZF.PK^K06"
         ,"ALVZn.S^B10","ALVG.SGp^L17","ALVZn.BS^L95","ALVGEUR.CHIp^E12","ALVEUR.MIp^L17","ALVG.Sp^E15","ALVG.BM^D03","ALVGEUR.OLp^E10","ALVGEUR.STp^J18","ALVG.NDT^K03","ALVd.HFT^E14","ALVde.PZ^A12","ALVG.NDE^K03","ALVG.ED^C03","ALVG.MB^L17","ALVGas.INS^K08","ALVGpa.INS^K08"
         ,"ALVZns.INS^G08","ALVG.IB^A98","ALVZn.Z^L95","ALVG.TI^H07","ALVZn.G^L95","ALVG.TP^H01","ALVmi.INS^K08","ALVde.MCp^I20","ALVG.rEUR^J09","ALVG.rGBP^K07","ALVG.rCHF^L08","LINUde.ICEM","LIN1EUR.xbo^J18","LIN1.DE^J18","0RUF.L^F19","LINUd.CHI^J18","LINUd.BS^J18","LIN1.F^J18"
         ,"LINUd.TQ^K18","LIN1.SG^J18","LINUde.TRE^F19","LIN1.BE^J18","LIN1.D^J18","LNDXF.PK^J19","LIN1.MU^J18","LIN1.H^J18","LIN1.HA^J18","LIN1EUR.MIp^L17","LNDXF.PQ^J19","LIN1.xt^J18","LIN1EUR.STp^J18","LINUd.ED^J18","LIN1.VI^K18","LIN1.DEU^J18","LINUd.BCO^B19","LIN1.TG^J18"
         ,"LINUd.AQX^J18","LIN1.VIf^K18","LINUde.TWEA^F19","LINUd.EDv^J18","LINUde.DAp^F19","LIN1.TX^J18","LINUde.ENp^F19","LINUd.AQXd^K18","LINU.QTX^K18","LINUde.TRXA","DE000A2E4L75.TRXA","NZYMb.CO","NZYMbDKK.xbo","NZYMBc.BS","NZYMBc.CHI","NZM2b.F","NZYMBc.TQ","NVZMF.PK"
         ,"NZM2b.BE","NZM2b.SG","NZM2b.D","NZM2b.MU","NZYMN.MX","NZYMBc.SIG","NZYMb.VI","NZYMBc.ED","NZYMb.xt","NZYMBco.TRE","0Q4U.L","NZYMb.S","NVZMF.PQ","NZYMb.COf","NZM2b.TG","NZM2b.DEU","NZYMBc.AQX","NZYMBco.DAp","NZYMN.BIV","NZYMBco.ICEM","NZYMBc.EDv","NZYMb.BN"
         ,"NZYMBc.NXT","NZYMb.VIf","NZYMBc.AQXd","NZM2.QTX","NZYMN.MCO","NZYMBc.MSF","DK0060336014.GTX","NZYMBc.EDM","NVZMFn.BCU","NZM2d.BCU","NZYMBc.BCU","NZYMBc.DXE","NZYMq.L^K07","NOZMb.DE^E11","NZYMbEUR.DEp^A18","NZYMbDKK.DEp^A18","NZYMb.VX^K02","NZYMb.PAp^C18"
         ,"NZYMb.SMp^J17","NZYMbDKK.PAp^B17","NZYMbEUR.PAp^B17","NZM2bEUR.DEp^C12","NZM2bDKK.DEp^C12","NZYMbEUR.SGp^L17","NZYMbDKK.SGp^L17","NZYMb.Sp^J18","NZYMb.SGp^L17","NZM2bDKK.SGp^C12","NZM2bEUR.SGp^C12","NZYMbDKK.CHIp^E12","NZYMbEUR.MIp^L17","NOZMb.BE^B04"
         ,"NZYMbEUR.OLp^E10","NZYMbDKK.OLp^E10","NZYMbEUR.STp^J18","NZYMbDKK.STp^J18","NZYMbx.CO^I04","NZYMbx.co^I04","NZYMbEUR.xt^I11","NZYM_Bdk.PZ^A12","NZYMBc.BD^E15","NZYMbco.SIG^K11","NZYMbco.HFT^K11","NZYMBco.ENp^H20","NOZMbf.INS^K08","NZYMb.rDKK^J09","NZYMb.rEUR^J09"
         ,"NZYMb.rGBP^K07","NZYMb.xEUR^A09","NZYMb.MB^L17","NZYMb.mEUR^L14","NZYMbDKK.Ip^G19","NZYMbEUR.Ip^G19","NZYMb.mGBP^L14","NZYMbDKK.NGMp^C10","0H7F.PO^L08","NZYMbEUR.BUp^K11","NZYMbDKK.BUp^L11","NZYMbDKK.VIp^J18","NZYMbEUR.VIp^J18","NZYMbco.NQX^G10","NZYMBc.QMF^E13","NZM2b.XR^L13"
         ,"NZYMBc.HFT^E14","NZYMBc.BCO","NZM2d.BCO","NVZMFn.BCO","NZYMBco.TWEA","NZYMBco.TRXA","REP_r.MC^A19","REPDe.BS^A19","REP_r.MC1^A19","REPDe.CHI^A19","REPC_r.MU^A19","REPC_r.BE^A19","REPC_r.SG^A19","REPDe.TQ^A19","RSPAF.PQ^A19","RSPAF.PK^A19","REPDe.BCO^E19","REPDrmc.VIp^I20"
         ,"REPDrmc.MCp^I20","REPDrmc.TWEA","ES06735169D7.VIp","ES06735169D7.MCp","REPDrmc.TRE","REP_r.MC^A20","REPE_r.F^A20","REP_r.MC1^A20","REPDe.BS^A20","REPDe.CHI^A20","REPDe.TQ^A20","REPE_r.SG^A20","REPE_r.MU^A20","REPE_r.BE^A20","REPE_r.DEU^A20","REPDe.DXE^A20","NOKIA.HE","NOKIAEUR.xbo"
         ,"NOKIASEK.xbo","NOKS.DE","NOKIA.PA","NOKIAh.BS","NOKS.F","NOKIAh.CHI","NOKIAh.TQ","NOKBF.PK","NOKIAs.BS","NOKIAs.CHI","NOKIAp.CHI","NOKS.BE","NOKIAp.BS","NOKS.D","NOKS.SG","NOKIAs.TQ","NOKIAp.TQ","NOKIA.MI","NOKS.MU","NOKIA.ST","NOKIA.S","NOKS.H","NOKS.HA","NOKIAm.BS","NOKIAm.CHI","NOKIAp.ED"
         ,"NOKIAh.SIG","NOKIA.VI","NOKIAh.ED","NOKIA.BN","NOKIAs.ED","NOKIA.PR","NOKIA.xt","0HAF.L","NOKIAhe.TRE","NOKIA.PA1","NOKIAm.TQ","NOKBF.PQ","NOKIAtah.MI","NOKIA.HEf","NOKIA.STf","NOKS.TG","NOKIAh.AQX","NOKIAp.AQX","NOK1V.TI")


test <- EikonGetData(EikonObject = Eikon, rics = Rics, Eikonformulas = fields, raw_output = FALSE)


PastandCurrentStocks_TS <- Refinitiv::EikonGetTimeseries( EikonObject = Eikon
                                                        , verbose = TRUE
                                                        , rics = head(Rics, n =4)
                                                        , start_date = paste0("2014-01-01", "T01:00:00")
                                                        , end_date = paste0(Sys.Date(), "T01:00:00")
                                                        , raw_output = FALSE

) # %>% data.table::as.data.table()



expect_equal(lapply(test$PostProcessedEikonGetData, class)
            , list(Instrument = "character", CURRENCY = "character", `Average.Daily.Volume.-.6.Months` = "integer",
                   Instrument.Type = "character", Exchange.Name = "character",
                   CF_EXCHNG = "character", Exchange.Market.Identifier.Code = "character",
                   Instrument.Is.Active.Flag = "logical") )

expect_equal(lapply(test$Eikon_Error_Data, class)
             , list(code = "integer", col = "integer", message = "character", row = "integer"))

})


