# Load required data for tests
load(file="testdata.rda")


testthat::test_that("retry", {
  testthat::expect_equal(retry(retryfun = sum(1,1)), sum(1,1))
  testthat::expect_equal(retry(retryfun = sum(1,"a"), max = 1), NULL)
})


testthat::test_that("check if install_eikon works", {
  testthat::expect_equal(install_eikon(), "Eikon Python interface successfully installed")
})


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



testthat::test_that("Check EikonGetTimeseries returns previously downloaded timeseries"
                   , {check_Eikonapi()
                     Eikon <- Refinitiv::EikonConnect()
                     CheckTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
                                                                rics = c("MMM", "III.L"),
                                                                start_date = "2020-01-01T01:00:00",
                                                                end_date = "2020-01-10T01:00:00"))
                     testthat::expect_identical(CheckTimeSeries, GoodCheckEikonTimeSeries)}
                   )




testthat::test_that("Check EikonGetData returns expected timeseries"
                    , {check_Eikonapi()
                       Eikon <- Refinitiv::EikonConnect()
                       CheckEikonData <- try(EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                                                          Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName")))
                       testthat::expect_identical(CheckEikonData, GoodCheckEikonData)
                      }
                      )



## add data for testing as:
# GoodCheckEikonTimeSeries <- CheckTimeSeries
# GoodCheckEikonData <- CheckEikonData
# save(StartTestEikonData, GoodOutcomeEikonPostProcessor , GoodCheckEikonTimeSeries, GoodCheckEikonData, file = "./tests/testthat/testdata.rda")




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



