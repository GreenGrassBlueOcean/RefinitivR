test_that("JsonListBuilder works", {

  expect_equal(JsonListBuilder(x = "a"),list("a"))
  expect_equal(JsonListBuilder(x = list("a")),list("a"))

})
