test_that("EikonConnect does not work without application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)

  if (is.null(getOption(".EikonApiKey"))){
      expect_error( EikonConnect()
                  , "Please supply Eikonapplication_id")
  }

  options(.EikonApiKey = originalOptionValue)
})

test_that("EikonConnect does not work with wrong connection method", {

  originalOptionValue = getOption(".EikonApiKey")
  originalAPIValue = getOption(".RefinitivAPI")
  options(.RefinitivAPI = NULL)
  options(.EikonApiKey = "testing_key")

  expect_error( RDConnect(PythonModule = "NA")
              , "RDConnect parameter PythonModule can only be RD (python) or JSON (direct JSON message) but is NA"
              , fixed = TRUE)

  expect_error( EikonConnect(PythonModule =NA)
                , "EikonConnect parameter PythonModule can only be RD (python) or JSON (direct JSON message) but is NA"
                , fixed = TRUE)


  options(.EikonApiKey = originalOptionValue)
  options(.RefinitivAPI = originalAPIValue)
})


test_that("EikonConnect does not work when parameter TestConnection is not a logical ", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = "testing_key")

  expect_error( EikonConnect(TestConnection = "notalogical")
                , "TestConnection should be TRUE or FALSE"
                , fixed = TRUE)

  options(.EikonApiKey = originalOptionValue)
})


test_that("EikonConnect does work when parameter TestConnection is JSON", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = "testing_key")

  expect_error(EikonConnect(PythonModule = "JSON")
              , NA, fixed = TRUE)

  options(.EikonApiKey = originalOptionValue)
})

test_that("RDConnect does not work without application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)

  if (is.null(getOption(".EikonApiKey"))){
    expect_error( RDConnect()
                  , "Please supply application_id")
  }

  options(.EikonApiKey = originalOptionValue)

})
