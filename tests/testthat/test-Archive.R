test_that("Archive works", {
  testthat::expect_equal(object = archivist::areadLocal("01d4b824508ccc93bfa03efc4ed14ce2"
                                                       , repo = paste0(testthat::test_path(),"/RefTestData")
                                                       )
                         , expected = "This is a test object")
})
