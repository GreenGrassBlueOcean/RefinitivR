test_that("replaceInList works", {

  x <- list(list(NA, NULL, NULL), list("a", "b", "c"))
  test <- Refinitiv:::replaceInList(x, function(x)if(is.null(x))NA else x)


  Expected_outcome <- list(list(NA, NA, NA), list("a", "b", "c"))

  expect_equal(test, Expected_outcome)
})
