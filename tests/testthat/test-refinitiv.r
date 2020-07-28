test_that( "",{ expect_equal(1,1)
  })

# Load required data for tests
load(file="testdata.rda")
#options( ".EikonApiKey" =  "Put your key here")




testthat::test_that("retry", {
  testthat::expect_equal(retry(retryfun = sum(1,1)), sum(1,1))
  testthat::expect_equal(retry(retryfun = sum(1,"a"), max = 1), NULL)
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
                     CheckTimeSeries <- EikonGetTimeseries( EikonObject = Eikon,
                                                                rics = c("MMM", "III.L"),
                                                                start_date = "2020-01-01T01:00:00",
                                                                end_date = "2020-01-10T01:00:00")

                     GoodCheckEikonTimeSeries <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800,
                                                                                   1578355200, 1578441600, 1578528000, 1578614400, 1577923200, 1578009600,
                                                                                   1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                                 , class = c("POSIXct", "POSIXt"))
                                                                , Security = c("III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "III.L", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                , CLOSE = c(1116.5, 1108, 1088.5, 1086.5, 1088, 1094, 1088.5, 180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47)
                                                                , HIGH = c(1123, 1110.5, 1102.5, 1101, 1089.5, 1096, 1100, 180.01, 178.66, 178.71, 178.51, 181.5, 181.59, 182.18)
                                                                , LOW = c(1107.5, 1090.5, 1074, 1086.5, 1075, 1083, 1085, 177.1356, 175.63, 176.35, 176.82, 177.65, 179.76, 180.14)
                                                                , OPEN = c(1108, 1110.5, 1101, 1096.5, 1079.5, 1090.5, 1100, 177.68, 177.02, 177.15, 178.28, 178, 181.51, 181.61)
                                                                , VOLUME = c(723692, 717234, 813990, 1163565, 1451744, 975325, 2377611, 3448335, 2467310, 1997981, 2176615, 2758339, 2746346, 2103818))
                                                           , row.names = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 2L, 4L, 6L, 8L, 10L, 12L, 14L), class = "data.frame")



                     testthat::expect_identical(CheckTimeSeries, GoodCheckEikonTimeSeries)}
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
                                                           , row.names = c(NA, 7L), class = "data.frame")

                      testthat::expect_identical(CheckTimeSeries, GoodCheckEikonTimeSeries)}
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

                      testthat::expect_identical(CheckTimeSeries, GoodCheckEikonTimeSeries)}
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
                                                            , row.names = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 2L, 4L, 6L, 8L, 10L, 12L, 14L), class = "data.frame")
                      testthat::expect_identical(CheckTimeSeries, GoodCheckEikonTimeSeries)}
)







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

                       GoodCheckEikonData <- list( PostProcessedEikonGetData = structure(list(Instrument = "MMM",RDN_EXCHD2 = "NYQ", Company.Name = "3M Co")
                                                 , class = "data.frame", row.names = c(NA, -1L))
                                                 , Eikon_Error_Data = structure(list(), .Names = character(0), row.names = integer(0), class = "data.frame")
                                                 )

                       CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("MMM"),
                                                          Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName")))


                       testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
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

    # GoodOutcomeEikonPostProcessor <- EikonPostProcessor(Eikon_get_dataOuput = StartTestEikonData)
    expect_equal(EikonPostProcessor(Eikon_get_dataOuput = StartTestEikonData), GoodOutcomeEikonPostProcessor)

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

  expected_outcome <- structure(list(VOLUME = character(0), HIGH = character(0), LOW = character(0),
                                     OPEN = character(0), CLOSE = character(0))
                                , class = "data.frame", row.names = character(0))


  expect_equivalent(EikonTimeseries,expected_outcome )

})



test_that( "ProcessSymbology works correctly", {

  input_df <- list(structure(list(RICs = list(c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                                "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAa.CHI", "RDSAa.BS", "RDSAl.BS",
                                                "RDSAl.CHI", "RDSAa.TQ", "RDSa.BE", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                                "RDSa.D", "RDSa.SG", "R6Cd.BS", "R6Cd.CHI", "RYDAF.PK", "RDSa.H",
                                                "RDSa.HA", "RDSAa.ED", "RDSAa.SIG", "RDSAl.ED", "RDSa.BN", "RDSA.PR",
                                                "RDSa.xt", "RDSa.AS1", "RDSAas.TRE", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                                "RDSAa.DXE", "R6Cd.DXE", "RDSAa.AQX", "RDSAa.NXT", "R6Cd.AQX",
                                                "RDSa.DEU", "RDSAl.EDM", "RDSAl.BCU", "RDSAas.DAp", "RDSAas.ICEM",
                                                "RDSAa.EDM", "R6Cd.BCU", "RDSAa.BCU", "RYDAFn.BCU", "R6C.QTX",
                                                "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAas.IGDM", "RDSAl.EDv",
                                                "RDSAa.EDv", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "R6Cd.AQXd",
                                                "RDSAas.TBEM", "0LN9.L^A20", "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18",
                                                "RDSa.SMp^J17", "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17",
                                                "RDSaEUR.DEp^A18", "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18",
                                                "RDSa.SGp^L17", "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17",
                                                "RDSaEUR.CHIp^E12", "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18",
                                                "RDSaGBP.xt^I11", "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18",
                                                "RDSaGBP.OLp^E10", "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18",
                                                "1ESRDSANA.PIPB^C18", "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20",
                                                "RDSaEUR.VIp^J18", "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09",
                                                "RDSa.MB^L17", "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19",
                                                "RDSaGBP.PZp^B09")
                                              , c("AAPL.O", "AAPLEUR.xbo", "0R2V.L", "AAPL.OQ",
                                                  "AAPL.Z", "AAPL.DG", "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL.BE",
                                                  "AAPL.MU", "AAPL.D", "AAPL.SG", "AAPLE.MI", "AAPL.HA", "AAPL.H",
                                                  "AAPLUSD.S", "AAPL.S", "AAPLEUR.S", "AAPL.MX", "AAPL.DY", "AAPL.ZY",
                                                  "AAPL.PH", "AAPL.DF", "AAPL.MW", "AAPL.SN", "AAPL.BN", "AAPL.VI",
                                                  "AAPL.LM", "AAPL.UAX", "US_AAPL.KZ", "AAPL.xt", "APCde.TRE",
                                                  "AAPLn.TQ", "AAPLEtah.MI", "AAPL.ARC", "AAPL.NB", "AAPL.CN",
                                                  "AAPL.BIV", "AAPL.CE", "AAPL.PFT", "AAPL.TI", "AAPL.PFTQ", "AAPL.TG",
                                                  "AAPL.N", "AAPL.P", "AAPL.BT1", "AAPL.ITC", "AAPL.A", "AAPL.BAT",
                                                  "AAPL.EI", "AAPL.C", "AAPL.BYX", "AAPL.DEU", "APCd.BCU", "AAPL.VIf",
                                                  "APCde.DAp", "APCde.ICEM", "AAPL.PFTP", "AAPL.PFTR", "0R2Vl.BCU",
                                                  "AAPL.MCO", "1ASPAAPL.PIPB", "1AAAPL.PIPB", "APC.QTX", "US0378331005.GTX",
                                                  "AAPLq.L^K07", "AAPL.T^L04", "AAPLEUR.Lp^H16", "AAPLz.F^D94",
                                                  "AAPLq.L^A00", "AAPL.CD^K00", "AAPLqEUR.PAp^K07", "AAPLqGBP.PAp^K07",
                                                  "AAPLEUR.DEp^A10", "AAPLGBP.DEp^A10", "AAPLEUR.PAp^B17", "AAPLGBP.PAp^F11",
                                                  "0HDZ.L^A08", "AAPLEUR.Lp^F08", "0HDZ.L^L08", "0JQ4.L^D10", "AAPLUSD.DEp^D13",
                                                  "AAPLEUR.DEp^D13", "AAPL.DEU^A04", "AAPLUSD.PAp^B17", "AAPL.S^K08",
                                                  "AAPL.SI^D02", "AAPL.B^J07", "AAPLde.INS^H07", "AAPLk.SI^B97",
                                                  "AAPLGBP.SGp^A10", "AAPLEUR.SGp^A10", "AAPLUSD.SGp^D13", "AAPL.HA^B08",
                                                  "AAPLEUR.SGp^D13", "AAPLEUR.CHIp^E12", "AAPL.BM^D03", "AAPLc.MX^J07"
                                                )), bestMatch = list(list(RIC = "RDSa.AS"), list(RIC = "AAPL.O")),
                                  symbol = c("GB00B03MLX29", "US0378331005"))
                             , class = "data.frame", row.names = c("GB00B03MLX29", "US0378331005")))


  test_ProcessSymbology <- ProcessSymbology(EikonSymbologyResult = input_df,  from_symbol_type = "ISIN" , to_symbol_type = "RIC")


  Good_outcome_ProcessSymbology <- structure(list(RIC = c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                         "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAa.CHI", "RDSAa.BS", "RDSAl.BS",
                                         "RDSAl.CHI", "RDSAa.TQ", "RDSa.BE", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                         "RDSa.D", "RDSa.SG", "R6Cd.BS", "R6Cd.CHI", "RYDAF.PK", "RDSa.H",
                                         "RDSa.HA", "RDSAa.ED", "RDSAa.SIG", "RDSAl.ED", "RDSa.BN", "RDSA.PR",
                                         "RDSa.xt", "RDSa.AS1", "RDSAas.TRE", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                         "RDSAa.DXE", "R6Cd.DXE", "RDSAa.AQX", "RDSAa.NXT", "R6Cd.AQX",
                                         "RDSa.DEU", "RDSAl.EDM", "RDSAl.BCU", "RDSAas.DAp", "RDSAas.ICEM",
                                         "RDSAa.EDM", "R6Cd.BCU", "RDSAa.BCU", "RYDAFn.BCU", "R6C.QTX",
                                         "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAas.IGDM", "RDSAl.EDv",
                                         "RDSAa.EDv", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "R6Cd.AQXd",
                                         "RDSAas.TBEM", "0LN9.L^A20", "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18",
                                         "RDSa.SMp^J17", "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17",
                                         "RDSaEUR.DEp^A18", "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18",
                                         "RDSa.SGp^L17", "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17",
                                         "RDSaEUR.CHIp^E12", "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18",
                                         "RDSaGBP.xt^I11", "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18",
                                         "RDSaGBP.OLp^E10", "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18",
                                         "1ESRDSANA.PIPB^C18", "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20",
                                         "RDSaEUR.VIp^J18", "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09",
                                         "RDSa.MB^L17", "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19",
                                         "RDSaGBP.PZp^B09", "AAPL.O", "AAPLEUR.xbo", "0R2V.L", "AAPL.OQ",
                                         "AAPL.Z", "AAPL.DG", "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL.BE",
                                         "AAPL.MU", "AAPL.D", "AAPL.SG", "AAPLE.MI", "AAPL.HA", "AAPL.H",
                                         "AAPLUSD.S", "AAPL.S", "AAPLEUR.S", "AAPL.MX", "AAPL.DY", "AAPL.ZY",
                                         "AAPL.PH", "AAPL.DF", "AAPL.MW", "AAPL.SN", "AAPL.BN", "AAPL.VI",
                                         "AAPL.LM", "AAPL.UAX", "US_AAPL.KZ", "AAPL.xt", "APCde.TRE",
                                         "AAPLn.TQ", "AAPLEtah.MI", "AAPL.ARC", "AAPL.NB", "AAPL.CN",
                                         "AAPL.BIV", "AAPL.CE", "AAPL.PFT", "AAPL.TI", "AAPL.PFTQ", "AAPL.TG",
                                         "AAPL.N", "AAPL.P", "AAPL.BT1", "AAPL.ITC", "AAPL.A", "AAPL.BAT",
                                         "AAPL.EI", "AAPL.C", "AAPL.BYX", "AAPL.DEU", "APCd.BCU", "AAPL.VIf",
                                         "APCde.DAp", "APCde.ICEM", "AAPL.PFTP", "AAPL.PFTR", "0R2Vl.BCU",
                                         "AAPL.MCO", "1ASPAAPL.PIPB", "1AAAPL.PIPB", "APC.QTX", "US0378331005.GTX",
                                         "AAPLq.L^K07", "AAPL.T^L04", "AAPLEUR.Lp^H16", "AAPLz.F^D94",
                                         "AAPLq.L^A00", "AAPL.CD^K00", "AAPLqEUR.PAp^K07", "AAPLqGBP.PAp^K07",
                                         "AAPLEUR.DEp^A10", "AAPLGBP.DEp^A10", "AAPLEUR.PAp^B17", "AAPLGBP.PAp^F11",
                                         "0HDZ.L^A08", "AAPLEUR.Lp^F08", "0HDZ.L^L08", "0JQ4.L^D10", "AAPLUSD.DEp^D13",
                                         "AAPLEUR.DEp^D13", "AAPL.DEU^A04", "AAPLUSD.PAp^B17", "AAPL.S^K08",
                                         "AAPL.SI^D02", "AAPL.B^J07", "AAPLde.INS^H07", "AAPLk.SI^B97",
                                         "AAPLGBP.SGp^A10", "AAPLEUR.SGp^A10", "AAPLUSD.SGp^D13", "AAPL.HA^B08",
                                         "AAPLEUR.SGp^D13", "AAPLEUR.CHIp^E12", "AAPL.BM^D03", "AAPLc.MX^J07"
                            ), ISIN = c("GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                        "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                        "US0378331005", "US0378331005")
                        , BestMatch = c("RDSa.AS", "RDSa.AS","RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                             "RDSa.AS", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                             "AAPL.O", "AAPL.O", "AAPL.O")), row.names = c(NA, -198L), class = "data.frame")

  expect_equal(test_ProcessSymbology, Good_outcome_ProcessSymbology)


  # test BestMatch = TRUE

  input_Bestmatch <- list(structure(list(RIC = c("RDSa.AS", "AAPL.O")), class = "data.frame"
                                    , row.names = c("GB00B03MLX29","US0378331005")))

  test_bestmatch <- ProcessSymbology(EikonSymbologyResult = input_Bestmatch,  from_symbol_type = "ISIN", to_symbol_type = "RIC")

  Good_outcomeTRUEBM <- structure(list(RIC = c("GB00B03MLX29", "US0378331005")), row.names = c(NA,-2L), class = "data.frame")

  expect_equal(test_bestmatch, Good_outcomeTRUEBM)


  expect_error(ProcessSymbology(EikonSymbologyResult = data.frame(),  from_symbol_type = "ISIN", to_symbol_type = "RIC"))

})




test_that("CondaExists returns a logical" , {

  test <- CondaExists()

  expect_type(test, "logical")



})


## test portfolio function








## add data for testing as:
# GoodCheckEikonTimeSeries <- CheckTimeSeries
# GoodCheckEikonData <- CheckEikonData
# save(StartTestEikonData, GoodOutcomeEikonPostProcessor , GoodCheckEikonTimeSeries, GoodCheckEikonData, file = "./tests/testthat/testdata.rda")


