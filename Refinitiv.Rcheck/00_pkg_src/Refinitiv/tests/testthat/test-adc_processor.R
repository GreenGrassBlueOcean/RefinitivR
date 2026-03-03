# Tests for process_adc_response() — the shared ADC response processor

# ── Mock ADC payloads ──

mock_adc_basic <- list(

  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "score",      title = "ESG Score",  type = "Double"),
    list(name = "period_end", title = "Period End",  type = "Date")
  ),
  data = list(
    list("AAPL.O", 82.5, "2023-12-31"),
    list("MSFT.O", 78.3, "2023-12-31")
  )
)

mock_adc_with_nulls <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String"),
    list(name = "value",      title = "Value",      type = "Float"),
    list(name = "note",       title = "Note",       type = "String")
  ),
  data = list(
    list("AAPL.O", 100.5, "OK"),
    list("MSFT.O", NULL,  ""),
    list("IBM.N",  NULL,  NULL)
  )
)

mock_adc_empty_data <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String")
  ),
  data = list()
)

mock_adc_no_data <- list(
  headers = list(
    list(name = "instrument", title = "Instrument", type = "String")
  )
)

mock_adc_no_headers <- list(
  data = list(list("AAPL.O", 82.5))
)

mock_adc_error <- list(
  error = list(message = "Insufficient entitlement for professional package")
)

mock_adc_fault <- list(
  fault = list(faultstring = "Rate limit exceeded")
)

mock_adc_mixed_types <- list(
  headers = list(
    list(name = "instrument", title = "Instrument",   type = "String"),
    list(name = "volume",     title = "Volume",        type = "Int64"),
    list(name = "price",      title = "Close Price",   type = "Double"),
    list(name = "trade_date", title = "Trade Date",    type = "Date"),
    list(name = "timestamp",  title = "Last Updated",  type = "DateTime"),
    list(name = "count",      title = "Count",         type = "Int32")
  ),
  data = list(
    list("AAPL.O", 1000000, 185.25, "2024-01-15", "2024-01-15T16:00:00", 42),
    list("MSFT.O", 2500000, 390.10, "2024-01-15", "2024-01-15T16:00:00", 87)
  )
)


# ── Tests ──

test_that("process_adc_response returns data.frame with correct columns (title headers)", {
  result <- process_adc_response(mock_adc_basic, use_field_names_in_headers = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_equal(names(result), c("Instrument", "ESG Score", "Period End"))
})

test_that("process_adc_response uses name headers when use_field_names_in_headers = FALSE", {
  result <- process_adc_response(mock_adc_basic, use_field_names_in_headers = FALSE)
  expect_equal(names(result), c("instrument", "score", "period_end"))
})

test_that("process_adc_response coerces types from header metadata", {
  result <- process_adc_response(mock_adc_basic)
  expect_type(result[["Instrument"]], "character")
  expect_type(result[["ESG Score"]], "double")
  expect_s3_class(result[["Period End"]], "Date")
})

test_that("process_adc_response handles NULL and empty string values", {
  result <- process_adc_response(mock_adc_with_nulls)
  expect_equal(nrow(result), 3L)
  # Row 2: NULL value → NA, "" note → NA

  expect_true(is.na(result[["Value"]][2]))
  expect_true(is.na(result[["Note"]][2]))
  # Row 3: both NULL → NA
  expect_true(is.na(result[["Value"]][3]))
  expect_true(is.na(result[["Note"]][3]))
  # Row 1: valid values preserved
  expect_equal(result[["Value"]][1], 100.5)
  expect_equal(result[["Note"]][1], "OK")
})

test_that("process_adc_response returns empty data.frame for empty data", {
  result <- process_adc_response(mock_adc_empty_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("process_adc_response returns empty data.frame when data is missing", {
  result <- process_adc_response(mock_adc_no_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("process_adc_response returns empty data.frame when headers are missing", {
  result <- process_adc_response(mock_adc_no_headers)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("process_adc_response stops on API error", {
  expect_error(
    process_adc_response(mock_adc_error),
    "LSEG API error: Insufficient entitlement"
  )
})

test_that("process_adc_response stops on API fault", {
  expect_error(
    process_adc_response(mock_adc_fault),
    "LSEG API fault: Rate limit exceeded"
  )
})

test_that("process_adc_response handles error with nested structure", {
  nested_error <- list(error = list(description = "Access denied"))
  expect_error(
    process_adc_response(nested_error),
    "LSEG API error: Access denied"
  )
})

test_that("process_adc_response handles error as plain string", {
  string_error <- list(error = "Something went wrong")
  expect_error(
    process_adc_response(string_error),
    "LSEG API error: Something went wrong"
  )
})

test_that("process_adc_response coerces all supported types", {
  result <- process_adc_response(mock_adc_mixed_types)
  expect_equal(nrow(result), 2L)

  # String
  expect_type(result[["Instrument"]], "character")
  expect_equal(result[["Instrument"]], c("AAPL.O", "MSFT.O"))

  # Int64 → numeric (double), not integer (avoids 32-bit overflow)
  expect_type(result[["Volume"]], "double")
  expect_equal(result[["Volume"]], c(1000000, 2500000))

  # Double → numeric
  expect_type(result[["Close Price"]], "double")
  expect_equal(result[["Close Price"]], c(185.25, 390.10))

  # Date
  expect_s3_class(result[["Trade Date"]], "Date")
  expect_equal(result[["Trade Date"]], as.Date(c("2024-01-15", "2024-01-15")))

  # DateTime → POSIXct
  expect_s3_class(result[["Last Updated"]], "POSIXct")

  # Int32 → integer
  expect_type(result[["Count"]], "integer")
  expect_equal(result[["Count"]], c(42L, 87L))
})

test_that("process_adc_response handles single row", {
  single_row <- list(
    headers = list(
      list(name = "instrument", title = "Instrument", type = "String"),
      list(name = "value",      title = "Value",      type = "Double")
    ),
    data = list(
      list("AAPL.O", 82.5)
    )
  )
  result <- process_adc_response(single_row)
  expect_equal(nrow(result), 1L)
  expect_equal(result[["Instrument"]], "AAPL.O")
  expect_equal(result[["Value"]], 82.5)
})

test_that("process_adc_response handles unknown type gracefully", {
  unknown_type <- list(
    headers = list(
      list(name = "x", title = "X", type = "WeirdType")
    ),
    data = list(list("hello"))
  )
  result <- process_adc_response(unknown_type)
  # Should leave as-is (character)
  expect_type(result[["X"]], "character")
  expect_equal(result[["X"]], "hello")
})

test_that("process_adc_response handles header with missing type field", {
  no_type <- list(
    headers = list(
      list(name = "x", title = "X")
    ),
    data = list(list("hello"))
  )
  result <- process_adc_response(no_type)
  expect_equal(result[["X"]], "hello")
})

test_that("process_adc_response handles header with missing title field", {
  no_title <- list(
    headers = list(
      list(name = "my_field", type = "String")
    ),
    data = list(list("hello"))
  )
  # With use_field_names_in_headers = TRUE, should fall back to name
  result <- process_adc_response(no_title, use_field_names_in_headers = TRUE)
  expect_equal(names(result), "my_field")
})

test_that("process_adc_response survives coercion failure gracefully", {
  bad_coerce <- list(
    headers = list(
      list(name = "x", title = "X", type = "Double")
    ),
    data = list(list("not_a_number"))
  )
  # Should not error — leaves column as NA from failed coercion
  result <- process_adc_response(bad_coerce)
  expect_equal(nrow(result), 1L)
})

test_that("Int64 values exceeding 2^31-1 are preserved (Q1 fix)", {
  large_int_response <- list(
    headers = list(
      list(name = "instrument", title = "Instrument",       type = "String"),
      list(name = "shares",     title = "Shares Outstanding", type = "Int64"),
      list(name = "volume",     title = "Volume",            type = "Int64"),
      list(name = "count",      title = "Count",             type = "Int32")
    ),
    data = list(
      # Shares: 15.4 billion (exceeds 2^31-1 = 2,147,483,647)
      # Volume: 3 billion
      list("0700.HK", 15400000000, 3000000000, 42),
      # Shares: exactly at the 32-bit boundary
      list("TEST.L",  2147483648, 2147483647, 10)
    )
  )
  result <- process_adc_response(large_int_response)

  # Int64 → numeric (double), values preserved

  expect_type(result[["Shares Outstanding"]], "double")
  expect_equal(result[["Shares Outstanding"]], c(15400000000, 2147483648))

  expect_type(result[["Volume"]], "double")
  expect_equal(result[["Volume"]], c(3000000000, 2147483647))

  # None should be NA (the pre-fix bug: as.integer() silently → NA)
  expect_false(any(is.na(result[["Shares Outstanding"]])))
  expect_false(any(is.na(result[["Volume"]])))

  # Int32 still uses as.integer()
  expect_type(result[["Count"]], "integer")
  expect_equal(result[["Count"]], c(42L, 10L))
})
