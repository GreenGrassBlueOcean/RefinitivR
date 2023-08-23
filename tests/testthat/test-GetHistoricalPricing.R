test_that("stops when it has the wrong eikonobject", {

  OldModuleName <- getOption(".RefinitivPyModuleName")
  options(.RefinitivPyModuleName = "wrong object")

    expect_error( rd_GetHistoricalPricing(EikonObject = NULL)
                , "historical pricing is only available when JSON --> RefinitivJsonConnect() or Python Refinitiv data --> RDConnect() is used as EikonObject"
                , fixed = TRUE)

  options(.RefinitivPyModuleName = OldModuleName)
})
