# Load required data for tests
system.file("testdata","testdata.rda", package="Refinitiv")

testthat::test_that("retry", {
  testthat::expect_equal(retry(retryfun = sum(1,1)), sum(1,1))
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
# save(GoodCheckEikonTimeSeries, GoodCheckEikonData, file = "./tests/testdata/testdata.rda")

