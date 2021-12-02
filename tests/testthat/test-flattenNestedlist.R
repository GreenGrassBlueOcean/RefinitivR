test_that("flattenNestedlist works", {

  test <- Refinitiv:::flattenNestedlist(list(list("a", "b"), c(1,2)))
  CorrectOutcome <- list(structure(list(V1 = "a", V2 = "b"), row.names = c(NA, -1L)
                                  , class = c("data.table", "data.frame")),
  structure(list(V1 = 1, V2 = 2), row.names = c(NA, -1L), class = c("data.table", "data.frame")))


  expect_equal(test,CorrectOutcome)
})
