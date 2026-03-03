library(testthat)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# These tests require a live Eikon/Workspace terminal connection.
# Offline coverage for EikonGetTimeseries is provided by
# test-httptest2-integration.R using recorded fixtures.

test_that("EikonGetTimeseries handles fields = NULL with multiple rics", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetTimeseries(
    EikonObject = Eikon,
    rics = c("MMM", "III.L"),
    fields = NULL,
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    corax = "unadjusted"
  )

  expect_s3_class(result, "data.frame")
  expect_true("Date" %in% names(result))
  expect_true("Security" %in% names(result))
  expect_true(all(c("MMM", "III.L") %in% result$Security))
  expect_true(nrow(result) > 0)
})

test_that("EikonGetTimeseries returns raw data when requested", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetTimeseries(
    EikonObject = Eikon,
    rics = c("MMM"),
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    raw_output = TRUE,
    corax = "unadjusted"
  )

  expect_type(result, "list")
})

test_that("EikonGetTimeseries works with single ric and single field", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetTimeseries(
    EikonObject = Eikon,
    rics = "MMM",
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    fields = "CLOSE",
    corax = "unadjusted"
  )

  expect_s3_class(result, "data.frame")
  expect_true("CLOSE" %in% names(result))
  expect_equal(unique(result$Security), "MMM")
  expect_equal(nrow(result), 7)
})

test_that("EikonGetTimeseries fails gracefully with empty ric list", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  expect_warning({
    result <- EikonGetTimeseries(
      EikonObject = Eikon,
      rics = c(),
      interval = "daily",
      fields = c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE"),
      start_date = "2020-07-21T01:00:00",
      end_date = "2020-07-28T01:00:00"
    )
  })
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("EikonGetTimeseries handles wrong ric gracefully", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetTimeseries(
    EikonObject = Eikon,
    rics = c("wrongRic", "MMM"),
    interval = "daily",
    fields = c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE"),
    start_date = "2020-07-21T01:00:00",
    end_date = "2020-07-28T01:00:00",
    corax = "unadjusted"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_false("wrongRic" %in% result$Security)
})

test_that("EikonGetTimeseries can switch between adjusted and unadjusted", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  adjusted <- EikonGetTimeseries(
    EikonObject = Eikon,
    rics = "MMM",
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    corax = "adjusted"
  )
  expect_s3_class(adjusted, "data.frame")
  expect_true(nrow(adjusted) > 0)

  unadjusted <- EikonGetTimeseries(
    EikonObject = Eikon,
    rics = "MMM",
    start_date = "2020-01-01T01:00:00",
    end_date = "2020-01-10T01:00:00",
    corax = "unadjusted"
  )
  expect_s3_class(unadjusted, "data.frame")
  expect_true(nrow(unadjusted) > 0)
})


restore_refinitiv_state(.saved_state, "test-EikonGetTimeSeries")
