test_that("RDPGetOptionAnalytics is defunct", {
  # RDPGetOptionAnalytics was removed in 0.2.0 (Python/reticulate dependency dropped).
  # It should always error with a .Defunct() message regardless of input.

  expect_error(RDPGetOptionAnalytics(OptionRics = NULL),
               "has been removed")
  expect_error(RDPGetOptionAnalytics(OptionRics = c("AAPL.O", "MSFT.O")),
               "has been removed")
})

dump_refinitiv_options("test-RDPGetOptionAnalytics")
