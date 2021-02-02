test_that("EikonTimeSeriesPreprocessor works", {

  test <- EikonTimeSeriesPreprocessor(interval = "daily", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")

  expected_outcome <- list(`1` = c("a", "b", "c"), `2` = c("d", "e", "f")
                          , `3` = c("g", "h", "i"), `4` = c("j", "k", "l")
                          , `5` = c("m", "n", "o"), `6` = c("p", "q", "r")
                          , `7` = c("s", "t", "u"), `8` = c("v", "w", "x")
                          , `9` = c("y", "z"))
  expect_identical(test, expected_outcome)
})


test_that("EikonTimeSeriesPreprocessor works for yearly interval", {


  test <- EikonTimeSeriesPreprocessor(interval = "yearly", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")

  expected_outcome <- list(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                             "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                             "y", "z"))

  expect_identical(test, expected_outcome)
})


test_that("EikonTimeSeriesPreprocessor works for quarterly interval", {


  test <- EikonTimeSeriesPreprocessor(interval = "quarterly", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")

  expected_outcome <- list(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                             "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                             "y", "z"))

  expect_identical(test, expected_outcome)
})


test_that("EikonTimeSeriesPreprocessor works for monthly interval", {


  test <- EikonTimeSeriesPreprocessor(interval = "monthly", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")

  expected_outcome <- list(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                             "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                             "y", "z"))

  expect_identical(test, expected_outcome)
})

test_that("EikonTimeSeriesPreprocessor works for weekly interval", {


  test <- EikonTimeSeriesPreprocessor(interval = "weekly", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")

  expected_outcome <- list(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                             "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                             "y", "z"))

  expect_identical(test, expected_outcome)
})


test_that("EikonTimeSeriesPreprocessor works for hour interval", {


  test <- EikonTimeSeriesPreprocessor(interval = "hour", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")

  expected_outcome <- list(`1` = c("a", "b"), `2` = c("c", "d"), `3` = c("e", "f"),
                           `4` = c("g", "h"), `5` = c("i", "j"), `6` = c("k", "l"),
                           `7` = c("m", "n"), `8` = c("o", "p"), `9` = c("q", "r"),
                           `10` = c("s", "t"), `11` = c("u", "v"), `12` = c("w", "x"
                           ), `13` = c("y", "z"))
  expect_identical(test, expected_outcome)
})


test_that("EikonTimeSeriesPreprocessor works for minute interval", {

  expect_error(EikonTimeSeriesPreprocessor(interval = "minute", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")
               , "Duration is too long for even one RIC, Reduce Duration by changing start_date or end_date!")
})

test_that("EikonTimeSeriesPreprocessor does not work for tick interval", {

  expect_error(EikonTimeSeriesPreprocessor(interval = "tick", rics = letters, start_date = "2015-01-01", end_date = "2018-01-01")
               , "Intraday tick data chunking currently not supported, maximum 50.000 data points per request")
})





