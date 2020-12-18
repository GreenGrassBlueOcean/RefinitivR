## Test EikonNameCleaner ----

test_that("EikonNameCleaner can convert spaces", {
  expect_equal(EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC"), SpaceConvertor = ","), c("Instrument","Company,Name","RDN_EXCHD2","Operating,MIC"))
  expect_equal(EikonNameCleaner(c("TR.ShortInterest(SDate=0D)/TR.SharesFreeFloat(SDate=0D)/*Short Interest as % of Float*/"), SpaceConvertor = "."), c("Short.Interest.as.%.of.Float"))
})


test_that("EikonNameCleaner can handle mixture of Reuters and self defined variable names", {
  expect_equal(EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC","TR.ShortInterest(SDate=0D)/TR.SharesFreeFloat(SDate=0D)/*Short Interest as % of Float*/"), SpaceConvertor = "."), c("Instrument","Company.Name","RDN_EXCHD2","Operating.MIC","Short.Interest.as.%.of.Float"))
})


test_that("EikonNameCleaner can switch off space conversion", {
  expect_equal(EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC"), SpaceConvertor = NA), c("Instrument","Company Name","RDN_EXCHD2","Operating MIC"))
  expect_equal(EikonNameCleaner(c("TR.ShortInterest(SDate=0D)/TR.SharesFreeFloat(SDate=0D)/*Short Interest as % of Float*/"), SpaceConvertor = NA), c("Short Interest as % of Float"))
})


test_that("EikonNameCleaner satisfies in default settings", {
  expect_equal(EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC", "Dividend yield")), c("Instrument","Company.Name","RDN_EXCHD2","Operating.MIC", "Dividend.Yield"))
  expect_equal(EikonNameCleaner(c("TR.ShortInterest(SDate=0D)/TR.SharesFreeFloat(SDate=0D)/*Short Interest as % of Float*/"), SpaceConvertor = "."), c("Short.Interest.as.%.of.Float"))
})
