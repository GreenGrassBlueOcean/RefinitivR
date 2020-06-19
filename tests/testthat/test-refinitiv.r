test_that( "",{ expect_equal(1,1)
  })

# Load required data for tests
load(file="testdata.rda")




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
                     CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                rics = c("MMM", "III.L"),
                                                                start_date = "2020-01-01T01:00:00",
                                                                end_date = "2020-01-10T01:00:00"))

                     GoodCheckEikonTimeSeries <- structure(list( Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400, 1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                   , class = c("POSIXct", "POSIXt")), Security = c("III.L", "III.L", "III.L", "III.L","III.L", "III.L", "III.L", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                   , CLOSE = c(1116.5, 1108, 1088.5, 1086.5, 1088, 1094, 1088.5, 180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47)
                                   , HIGH = c(1123, 1110.5, 1102.5, 1101, 1089.5, 1096, 1100, 180.01, 178.66, 178.71, 178.51, 181.5, 181.59, 182.18)
                                   , LOW = c(1107.5, 1090.5, 1074, 1086.5, 1075, 1083, 1085, 177.1356, 175.63, 176.35, 176.82, 177.65, 179.76, 180.14)
                                   , OPEN = c(1108, 1110.5, 1101, 1096.5, 1079.5, 1090.5, 1100, 177.68, 177.02, 177.15, 178.28, 178, 181.51, 181.61)
                                   , VOLUME = c(723692, 717234, 813990, 1163565, 1451744, 975325, 2377611, 3448335, 2467310, 1997981, 2176615, 2758339, 2746346, 2103818))
                               , idvars = c("Date", "Security")
                               , rdimnames = list(structure(list( Date = structure(c(1577923200, 1577923200, 1578009600, 1578009600, 1578268800, 1578268800, 1578355200, 1578355200, 1578441600, 1578441600, 1578528000, 1578528000, 1578614400, 1578614400)
                                                                , class = c("POSIXct", "POSIXt")), Security = c("III.L", "MMM", "III.L", "MMM", "III.L", "MMM", "III.L", "MMM", "III.L", "MMM", "III.L", "MMM", "III.L", "MMM"))
                                                                , row.names = c("2020-01-02 01:00:00_III.L", "2020-01-02 01:00:00_MMM", "2020-01-03 01:00:00_III.L", "2020-01-03 01:00:00_MMM", "2020-01-06 01:00:00_III.L"
                                                                               , "2020-01-06 01:00:00_MMM", "2020-01-07 01:00:00_III.L", "2020-01-07 01:00:00_MMM", "2020-01-08 01:00:00_III.L", "2020-01-08 01:00:00_MMM", "2020-01-09 01:00:00_III.L"
                                                                               , "2020-01-09 01:00:00_MMM", "2020-01-10 01:00:00_III.L", "2020-01-10 01:00:00_MMM"), class = "data.frame")
                                                                , structure(list(Field = c("CLOSE", "HIGH", "LOW", "OPEN", "VOLUME"))
                                                                            , row.names = c("CLOSE", "HIGH", "LOW", "OPEN", "VOLUME"), class = "data.frame")), row.names = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 2L, 4L, 6L, 8L, 10L, 12L, 14L), class = "data.frame")




                     testthat::expect_identical(CheckTimeSeries, GoodCheckEikonTimeSeries)}
                   )



testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries with only one ric"
                    , {check_Eikonapi()
                      Eikon <- Refinitiv::EikonConnect()
                      CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                 rics = c("MMM"),
                                                                 start_date = "2020-01-01T01:00:00",
                                                                 end_date = "2020-01-10T01:00:00"))

                      GoodCheckEikonTimeSeries <- structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400)
                                                                , class = c("POSIXct", "POSIXt"))
                                                                , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM")
                                                                , CLOSE = c(180, 178.45, 178.62, 177.9, 180.63, 181.2, 180.47)
                                                                , HIGH = c(180.01, 178.66, 178.71, 178.51, 181.5, 181.59, 182.18)
                                                                , LOW = c(177.1356, 175.63, 176.35, 176.82, 177.65, 179.76, 180.14)
                                                                , OPEN = c(177.68, 177.02, 177.15, 178.28, 178, 181.51, 181.61)
                                                                , VOLUME = c(3448335, 2467310, 1997981, 2176615, 2758339, 2746346, 2103818))
                                                            , idvars = c("Date", "Security")
                                                            , rdimnames = list(structure(list(Date = structure(c(1577923200, 1578009600, 1578268800, 1578355200, 1578441600, 1578528000, 1578614400), class = c("POSIXct", "POSIXt"))
                                                                                              , Security = c("MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM"))
                                                                                         , row.names = c("2020-01-02 01:00:00_MMM", "2020-01-03 01:00:00_MMM", "2020-01-06 01:00:00_MMM", "2020-01-07 01:00:00_MMM", "2020-01-08 01:00:00_MMM", "2020-01-09 01:00:00_MMM", "2020-01-10 01:00:00_MMM")
                                                                                         , class = "data.frame"), structure(list(Field = c("CLOSE", "HIGH", "LOW", "OPEN", "VOLUME"))
                                                                                                                            , row.names = c("CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
                                                                                                                            , class = "data.frame")), row.names = c(NA, 7L), class = "data.frame")


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


## test EikonPostProcessor ----


test_that("EikonPostProcessor satisfies testcases", {

# GoodOutcomeEikonPostProcessor <- EikonPostProcessor(Eikon_get_dataOuput = StartTestEikonData)
    expect_equal(EikonPostProcessor(Eikon_get_dataOuput = StartTestEikonData), GoodOutcomeEikonPostProcessor)

})



## add data for testing as:
# GoodCheckEikonTimeSeries <- CheckTimeSeries
# GoodCheckEikonData <- CheckEikonData
# save(StartTestEikonData, GoodOutcomeEikonPostProcessor , GoodCheckEikonTimeSeries, GoodCheckEikonData, file = "./tests/testthat/testdata.rda")


