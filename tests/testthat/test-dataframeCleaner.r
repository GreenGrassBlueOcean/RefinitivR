test_that("make.true.NA_df works", {

  dfr <- data.frame( A = c(1,2,NaN,3)
                   , B = c("a", "<NA>" ,"","d")
                   , C = c(as.Date("2012-01-01"), as.Date("2012-01-02"), as.Date("2012-01-03"), as.Date("2012-01-04"))
                   , stringsAsFactors = FALSE
                   )
  test_outcome <- make.true.NA_df(dfr)
  correct_outcome <- structure(list( A = c(1, 2, NA, 3)
                                    , B = c("a", NA, NA, "d")
                                    , C = structure(c(15340, 15341, 15342, 15343), class = "Date"))
                               , class = "data.frame", row.names = c(NA, -4L))
  expect_equal(test_outcome, correct_outcome)
})
