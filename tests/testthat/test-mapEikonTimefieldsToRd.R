library(testthat)

test_that("mapEikonTimefieldsToRd translates known columns correctly and retains names", {
  original_cols <- c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE")
  expected_cols <- c(TIMESTAMP = "TIMESTAMP", VOLUME = "ACVOL_UNS", HIGH = "MKT_HIGH",
                     LOW = "MKT_LOW", OPEN = "MKT_OPEN", CLOSE = "CLS_AUC")

  translated_cols <- mapEikonTimefieldsToRd(original_cols)

  expect_equal(translated_cols, expected_cols)
})

test_that("mapEikonTimefieldsToRd keeps unmapped columns unchanged and retains names", {
  original_cols <- c("UNKNOWN_COLUMN", "VOLUME", "HIGH")
  expected_cols <- c(UNKNOWN_COLUMN = "UNKNOWN_COLUMN", VOLUME = "ACVOL_UNS", HIGH = "MKT_HIGH")

  translated_cols <- mapEikonTimefieldsToRd(original_cols)

  expect_equal(translated_cols, expected_cols)
})

test_that("mapEikonTimefieldsToRd handles empty input gracefully and returns empty named vector", {
  original_cols <- character(0)
  expected_cols <- setNames(character(0), character(0))  # Empty named vector

  translated_cols <- mapEikonTimefieldsToRd(original_cols)

  expect_equal(translated_cols, expected_cols)
})

test_that("mapEikonTimefieldsToRd handles input with only unmapped columns and retains names", {
  original_cols <- c("UNKNOWN_COLUMN_1", "UNKNOWN_COLUMN_2")
  expected_cols <- c(UNKNOWN_COLUMN_1 = "UNKNOWN_COLUMN_1", UNKNOWN_COLUMN_2 = "UNKNOWN_COLUMN_2")

  translated_cols <- mapEikonTimefieldsToRd(original_cols)

  expect_equal(translated_cols, expected_cols)
})

test_that("mapEikonTimefieldsToRd handles input with only mapped columns and retains names", {
  original_cols <- c("VOLUME", "HIGH", "LOW")
  expected_cols <- c(VOLUME = "ACVOL_UNS", HIGH = "MKT_HIGH", LOW = "MKT_LOW")

  translated_cols <- mapEikonTimefieldsToRd(original_cols)

  expect_equal(translated_cols, expected_cols)
})
