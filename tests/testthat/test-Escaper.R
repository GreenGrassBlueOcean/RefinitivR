test_that("Escaper works", {
  expect_equal(Escaper("a"), "\"a\"")
  expect_equal(Escaper(list(a = "a")), "\"a\"")
  expect_error( Escaper(x = c("a", "b"))
              , "function escaper can only be used for single length character or list")
})
