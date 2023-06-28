test_that("jsonlistbuilder works", {


  expect_equal(jsonlistbuilder(x = "a"),list("a"))
  expect_equal(jsonlistbuilder(x = list("a")),list("a"))

})
