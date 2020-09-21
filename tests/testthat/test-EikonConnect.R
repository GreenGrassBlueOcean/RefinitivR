test_that("EikonConnect does not work without application id", {

  expect_error( EikonConnect()
                  , "Please supply Eikonapplication_id"
    )


})
