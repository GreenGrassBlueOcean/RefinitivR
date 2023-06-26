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

  expect_error( RDPConnect()
              , "assign value 'RDP' or 'JSON' to parameter PythonModule"
              , fixed = TRUE)

  expect_error( EikonConnect(PythonModule =NA)
                , "assign value 'RDP','Eikon' or 'JSON' to parameter PythonModule"
                , fixed = TRUE)


  options(.EikonApiKey = originalOptionValue)
  options(.RefinitivAPI = originalAPIValue)
})


test_that("EikonConnect does not work with wrong connection method", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = "testing_key")



  expect_error( EikonConnect(PythonModule = "WrongModule")
              , "EikonConnect parameter PythonModule can only be Eikon (python),RDP (python) or JSON (direct JSON message) but is WrongModule"
              , fixed = TRUE)

  expect_error( RDPConnect(PythonModule = "WrongModule")
                , "RDPConnect parameter PythonModule can only be RDP (python),JSON (direct JSON message) but is WrongModule"
                , fixed = TRUE)


  options(.EikonApiKey = originalOptionValue)
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







test_that("RDPConnect does not work without application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)

  if (is.null(getOption(".EikonApiKey"))){
    expect_error( RDPConnect()
                  , "Please supply application_id")
  }

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
