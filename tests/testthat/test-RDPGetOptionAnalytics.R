test_that("RDPGetOptionAnalytics fails when it should", {

  expect_error(RDPGetOptionAnalytics(OptionRics = NULL)
              , "OptionRics should be supplied currently is not supplied or is in the wrong format")
  expect_error(RDPGetOptionAnalytics(OptionRics = c(1,2))
               , "OptionRics should be supplied currently is not supplied or is in the wrong format")

})
