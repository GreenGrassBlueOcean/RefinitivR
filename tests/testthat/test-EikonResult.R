# ── EikonResult S3 class tests ────────────────────────────────────────────────

# Helper: build a mock EikonResult with mismatched row counts (the bug scenario)
mock_eikon_result <- function(n_data = 38L, n_errors = 40L) {
  df <- if (n_data > 0L) {
    data.frame(
      Instrument = paste0("RIC", seq_len(n_data)),
      Value = seq_len(n_data) * 1.1,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(Instrument = character(0), Value = numeric(0), stringsAsFactors = FALSE)
  }
  err <- if (n_errors > 0L) {
    data.frame(
      code = rep(412L, n_errors),
      col = rep(1L, n_errors),
      message = rep("Unable to resolve", n_errors),
      row = seq_len(n_errors) - 1L,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }
  Refinitiv:::new_EikonResult(list(
    PostProcessedEikonGetData = df,
    Eikon_Error_Data = err
  ))
}


## Backward compatibility ──────────────────────────────────────────────────────

test_that("EikonResult$PostProcessedEikonGetData returns the data.frame", {
  res <- mock_eikon_result()
  expect_s3_class(res$PostProcessedEikonGetData, "data.frame")
  expect_equal(nrow(res$PostProcessedEikonGetData), 38L)
  expect_true("Instrument" %in% names(res$PostProcessedEikonGetData))
})

test_that("EikonResult$Eikon_Error_Data returns the error frame", {
  res <- mock_eikon_result()
  expect_s3_class(res$Eikon_Error_Data, "data.frame")
  expect_equal(nrow(res$Eikon_Error_Data), 40L)
})

test_that("names(EikonResult) returns the list element names", {
  res <- mock_eikon_result()
  expect_equal(names(res), c("PostProcessedEikonGetData", "Eikon_Error_Data"))
})

test_that("'PostProcessedEikonGetData' %in% names(result) works", {
  res <- mock_eikon_result()
  expect_true("PostProcessedEikonGetData" %in% names(res))
})

test_that("EikonResult inherits from list", {
  res <- mock_eikon_result()
  expect_true(inherits(res, "list"))
  expect_true(is.list(res))
})


## as.data.frame / as.data.table ──────────────────────────────────────────────

test_that("as.data.frame.EikonResult returns only data (no recycling)", {
  res <- mock_eikon_result(n_data = 38, n_errors = 40)
  df <- as.data.frame(res)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 38L)
  expect_false(inherits(df, "EikonResult"))
})

test_that("as.data.table.EikonResult returns only data without warning", {
  res <- mock_eikon_result(n_data = 38, n_errors = 40)
  expect_no_warning(dt <- data.table::as.data.table(res))
  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 38L)
})

test_that("as.data.table on mismatched row counts produces NO recycling warning", {
  # This is the exact user-reported scenario
  res <- mock_eikon_result(n_data = 38, n_errors = 40)
  expect_no_warning(data.table::as.data.table(res))
})

test_that("as.data.table works when error frame is empty", {
  res <- mock_eikon_result(n_data = 10, n_errors = 0)
  expect_no_warning(dt <- data.table::as.data.table(res))
  expect_equal(nrow(dt), 10L)
})


## print ───────────────────────────────────────────────────────────────────────

test_that("print.EikonResult shows summary header", {
  res <- mock_eikon_result(n_data = 5, n_errors = 3)
  out <- capture.output(print(res))
  expect_true(any(grepl("EikonResult: 5 rows, 2 cols, 3 API error", out)))
})

test_that("print.EikonResult omits error count when no errors", {
  res <- mock_eikon_result(n_data = 5, n_errors = 0)
  out <- capture.output(print(res))
  expect_false(any(grepl("error", out, ignore.case = TRUE)))
})


## Edge cases ─────────────────────────────────────────────────────────────────

test_that("EikonResult works with zero data rows", {
  res <- mock_eikon_result(n_data = 0, n_errors = 0)
  expect_equal(nrow(as.data.frame(res)), 0L)
  expect_no_warning(data.table::as.data.table(res))
})

test_that("EikonResult [[ accessor works", {
  res <- mock_eikon_result()
  expect_identical(res[["PostProcessedEikonGetData"]], res$PostProcessedEikonGetData)
})
