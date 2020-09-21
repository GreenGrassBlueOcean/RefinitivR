test_that("EikonConnect does not work without application id", {


  if (is.null(getOption(".EikonApiKey"))){
  expect_error( EikonConnect()
                  , "Please supply Eikonapplication_id"
    )
  }

})
