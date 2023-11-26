test_that("dateFormatCheck works", {

  date <- '1985-01-01'
  dateFormatCheck(date)
  expect_true(dateFormatCheck(date))


  dateWrong <- '1999/1/7'
  expect_false(dateFormatCheck(dateWrong))

  dateWrong <- '0D'
  expect_false(dateFormatCheck(dateWrong))

  dateWrong <- '0'
  expect_false(dateFormatCheck(dateWrong))

})
