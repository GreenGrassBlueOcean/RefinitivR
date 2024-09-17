# tests/testthat/test-translate_to_iso8601_duration.R

test_that("translate_to_iso8601_duration handles valid inputs correctly", {
  expect_equal(translate_to_iso8601_duration('minute'), 'PT1M')
  expect_equal(translate_to_iso8601_duration('5 minutes'), 'PT5M')
  expect_equal(translate_to_iso8601_duration('hour'), 'PT1H')
  expect_equal(translate_to_iso8601_duration('weekly'), 'P1W')
  expect_equal(translate_to_iso8601_duration('yearly'), 'P1Y')
})

test_that("translate_to_iso8601_duration handles invalid inputs by defaulting to daily", {
  expect_equal(translate_to_iso8601_duration('unknown'), 'P1D')
  expect_equal(translate_to_iso8601_duration(''), 'P1D')
  expect_equal(translate_to_iso8601_duration(NULL), 'P1D')
})

test_that("translate_to_iso8601_duration handles edge cases", {
  expect_equal(translate_to_iso8601_duration(NA), 'P1D')
  expect_equal(translate_to_iso8601_duration('7 days'), 'P7D')
})
