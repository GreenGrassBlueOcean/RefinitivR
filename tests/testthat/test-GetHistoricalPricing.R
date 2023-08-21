test_that("stops when it has the wrong eikonobject", {

  expect_error( rd_GetHistoricalPricing(EikonObject = NULL)
              , "historical pricing is only available when RefinitivJsonConnect is used as EikonObject" )

})
