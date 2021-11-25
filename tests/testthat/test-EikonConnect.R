test_that("EikonConnect does not work without application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)

  if (is.null(getOption(".EikonApiKey"))){
      expect_error( EikonConnect()
                  , "Please supply Eikonapplication_id")
  }

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

