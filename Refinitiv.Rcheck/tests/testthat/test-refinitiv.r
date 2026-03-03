test_that("placeholder", {
  expect_equal(1, 1)
})

# Load required data for pure unit tests
load(file = "testdata.rda")

## Test retry ----

test_that("retry works for successful calls", {
  expect_equal(retry(function() sum(1, 1)), sum(1, 1))
})

test_that("retry stops with informative error by default after exhaustion", {
  expect_error(
    suppressMessages(retry(function() stop("boom"), max_attempts = 1, backoff = 0)),
    "All 1 retry attempts failed. Last error: boom"
  )
})

test_that("retry includes last error detail in stop message", {
  expect_error(
    suppressMessages(retry(function() stop("connection timed out"),
                           max_attempts = 2, backoff = 0)),
    "Last error: connection timed out"
  )
})

test_that("retry with on_failure = 'NA' returns NA and warns after exhaustion", {
  expect_warning(
    result <- suppressMessages(
      retry(function() stop("boom"), max_attempts = 1, backoff = 0, on_failure = "NA")
    ),
    "All 1 retry attempts failed. Last error: boom"
  )
  expect_identical(result, NA)
})

test_that("retry actually re-executes the function on each attempt", {
  counter <- 0L
  result <- retry(function() {
    counter <<- counter + 1L
    if (counter < 3L) stop("not yet")
    "done"
  }, max_attempts = 3, backoff = 0)
  expect_equal(result, "done")
  expect_equal(counter, 3L)
})

test_that("retry respects the refinitiv_max_retries option", {
  counter <- 0L
  withr::with_options(list(refinitiv_max_retries = 4L), {
    result <- retry(function() {
      counter <<- counter + 1L
      if (counter < 4L) stop("not yet")
      "done"
    }, backoff = 0)
  })
  expect_equal(result, "done")
  expect_equal(counter, 4L)
})

test_that("retry returns NULL when fn() legitimately returns NULL", {
  # A1 fix: NULL is a valid return value, not a failure signal.
  result <- retry(function() NULL, max_attempts = 2, backoff = 0)
  expect_null(result)
})

test_that("retry does not re-execute when fn() returns NULL", {
  counter <- 0L
  result <- retry(function() { counter <<- counter + 1L; NULL },
                  max_attempts = 3, backoff = 0)
  expect_null(result)
  expect_equal(counter, 1L)
})


## Test EikonConnect / JSON connection ----

test_that("JSON connection object has expected methods", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  expect_true(all(c("get_data", "get_symbology", "get_timeseries") %in% names(Eikon)))
})


## Test EikonGetData (live API) ----

test_that("EikonGetData returns expected data with multiple rics", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                         Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"))

  expect_type(result, "list")
  expect_true("PostProcessedEikonGetData" %in% names(result))
  expect_s3_class(result$PostProcessedEikonGetData, "data.frame")
  expect_equal(nrow(result$PostProcessedEikonGetData), 2)
  expect_true(all(c("MMM", "III.L") %in% result$PostProcessedEikonGetData$Instrument))
})

test_that("EikonGetData returns raw data when requested", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                         Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"),
                         raw_output = TRUE)

  expect_type(result, "list")
})

test_that("EikonGetData returns expected data with single ric", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetData(EikonObject = Eikon, rics = "MMM",
                         Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"))

  expect_type(result, "list")
  expect_equal(nrow(result$PostProcessedEikonGetData), 1)
})

test_that("EikonGetData handles wrong RIC gracefully", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetData(EikonObject = Eikon, rics = "WRONRIC",
                         Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"),
                         raw_output = FALSE)

  expect_type(result, "list")
  expect_s3_class(result$PostProcessedEikonGetData, "data.frame")
})

test_that("EikonGetData handles mix of good and wrong RICs", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonGetData(EikonObject = Eikon, rics = c("WRONGRIC", "MMM"),
                         Eikonformulas = c("RDN_EXCHD2", "TR.CompanyName"),
                         verbose = TRUE)

  expect_s3_class(result$PostProcessedEikonGetData, "data.frame")
  expect_equal(nrow(result$PostProcessedEikonGetData), 2)
})


## Test EikonShowAttributes ----

test_that("EikonShowAttributes returns an error when it should", {
  expect_error(EikonShowAttributes(EikonObject = NULL),
               "EikonObject should be supplied in function EikonShowAttributes")
})

test_that("EikonShowAttributes returns expected vector", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  result <- EikonShowAttributes(Eikon)
  expect_type(result, "character")
  expect_true(is.vector(result))
})


## Test EikonChunker ----

test_that("EikonChunker returns an error when it should", {
  expect_error(EikonChunker())
  expect_error(EikonChunker(RICS = rep("a", times = 200)))
  expect_error(EikonChunker(RICS = rep("a", times = 200),
                            MaxCallsPerChunk = 12000, Duration = 12001))
})

test_that("EikonChunker satisfies basic splitting", {
  CorrectSolution <- lapply(1:8, FUN = function(x) { rep("a", times = 25) })
  names(CorrectSolution) <- as.character(1:8)

  expect_equal(
    EikonChunker(RICS = rep("a", times = 200), MaxCallsPerChunk = 100,
                 Eikonfields = c("TR.CompanyName", "RDN_EXCHD2", "TR.OperatingMIC", "TR.ISINCode")),
    CorrectSolution
  )
})

test_that("EikonChunker respects MaxRicsperChunk = 20", {
  expect_error({
    SplittedRics <- EikonChunker(RICS = rep("a", times = 200), Duration = 7,
                                 MaxCallsPerChunk = 100, MaxRicsperChunk = 20)
  }, NA)

  expect_equal(sum(unlist(lapply(SplittedRics, length))), 200)
})

test_that("EikonChunker respects MaxRicsperChunk = 10", {
  expect_error({
    SplittedRics <- EikonChunker(RICS = rep("a", times = 200), Duration = 7,
                                 MaxCallsPerChunk = 100, MaxRicsperChunk = 10)
  }, NA)

  expect_equal(sum(unlist(lapply(SplittedRics, length))), 200)
})


## Test EikonPostProcessor ----

test_that("EikonPostProcessor satisfies testcases", {
  expect_equal(EikonPostProcessor(Eikon_get_dataOuput = StartTestEikonData),
               GoodOutcomeEikonPostProcessor)

  expect_equal(
    EikonPostProcessor(Eikon_get_dataOuput = list(NULL)),
    list(PostProcessedEikonGetData = structure(list(), .Names = character(0),
                                              row.names = integer(0), class = "data.frame"),
         Eikon_Error_Data = structure(list(), .Names = character(0),
                                     row.names = integer(0), class = "data.frame"))
  )
})

test_that("EikonPostProcessor handles empty strings without turning column to character", {
  testinput <- list(list(
    columnHeadersCount = 1L,
    data = list(
      list("LP68237734", "EUR", "", "Open-Ended Fund", "", "LIP", NULL, TRUE),
      list("NL0000280501=DAp", NULL, NULL, NULL, NULL, NULL, NULL, NULL),
      list("LP60073160^F20", NULL, "", "", "", NULL, NULL, "")
    ),
    error = list(
      list(code = 251658243L, col = 1L,
           message = "'The record could not be found' for the instrument 'NL0000280501=DAp'", row = 1L),
      list(code = 416L, col = 2L,
           message = "Unable to collect data for the field 'TR.AvgDailyVolume6M' and some specific identifier(s).", row = 1L),
      list(code = 416L, col = 3L,
           message = "Unable to collect data for the field 'TR.InstrumentType' and some specific identifier(s).", row = 1L),
      list(code = 416L, col = 4L,
           message = "Unable to collect data for the field 'TR.ExchangeName' and some specific identifier(s).", row = 1L),
      list(code = 251658243L, col = 5L,
           message = "'The record could not be found' for the instrument 'NL0000280501=DAp'", row = 1L),
      list(code = 416L, col = 6L,
           message = "Unable to collect data for the field 'TR.ExchangeMarketIdCode' and some specific identifier(s).", row = 1L),
      list(code = 416L, col = 7L,
           message = "Unable to collect data for the field 'TR.InstrumentIsActive' and some specific identifier(s).", row = 1L),
      list(code = 251658243L, col = 1L,
           message = "'The record could not be found' for the instrument 'LP60073160^F20'", row = 2L),
      list(code = 251658243L, col = 5L,
           message = "'The record could not be found' for the instrument 'LP60073160^F20'", row = 2L)
    ),
    headerOrientation = "horizontal",
    headers = list(list(
      list(displayName = "Instrument"),
      list(displayName = "CURRENCY", field = "CURRENCY"),
      list(displayName = "Average Daily Volume - 6 Months", field = "TR.AVGDAILYVOLUME6M"),
      list(displayName = "Instrument Type", field = "TR.INSTRUMENTTYPE"),
      list(displayName = "Exchange Name", field = "TR.EXCHANGENAME"),
      list(displayName = "CF_EXCHNG", field = "CF_EXCHNG"),
      list(displayName = "Exchange Market Identifier Code", field = "TR.EXCHANGEMARKETIDCODE"),
      list(displayName = "Instrument Is Active Flag", field = "TR.INSTRUMENTISACTIVE")
    )),
    rowHeadersCount = 1L, totalColumnsCount = 8L, totalRowsCount = 4L
  ))

  GoodOutcome <- list(
    PostProcessedEikonGetData = structure(list(
      Instrument = c("LP68237734", "NL0000280501=DAp", "LP60073160^F20"),
      CURRENCY = c("EUR", NA, NA),
      `Average.Daily.Volume.-.6.Months` = c(NA, NA, NA),
      Instrument.Type = c("Open-Ended Fund", NA, NA),
      Exchange.Name = c(NA, NA, NA),
      CF_EXCHNG = c("LIP", NA, NA),
      Exchange.Market.Identifier.Code = c(NA, NA, NA),
      Instrument.Is.Active.Flag = c(TRUE, NA, NA)
    ), row.names = c(NA, -3L), class = "data.frame"),
    Eikon_Error_Data = structure(list(
      code = c(251658243L, 416L, 416L, 416L, 251658243L, 416L, 416L, 251658243L, 251658243L),
      col = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L, 5L),
      message = c(
        "'The record could not be found' for the instrument 'NL0000280501=DAp'",
        "Unable to collect data for the field 'TR.AvgDailyVolume6M' and some specific identifier(s).",
        "Unable to collect data for the field 'TR.InstrumentType' and some specific identifier(s).",
        "Unable to collect data for the field 'TR.ExchangeName' and some specific identifier(s).",
        "'The record could not be found' for the instrument 'NL0000280501=DAp'",
        "Unable to collect data for the field 'TR.ExchangeMarketIdCode' and some specific identifier(s).",
        "Unable to collect data for the field 'TR.InstrumentIsActive' and some specific identifier(s).",
        "'The record could not be found' for the instrument 'LP60073160^F20'",
        "'The record could not be found' for the instrument 'LP60073160^F20'"
      ),
      row = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L)
    ), row.names = c(NA, -9L), class = "data.frame")
  )

  expect_equal(EikonPostProcessor(Eikon_get_dataOuput = testinput), GoodOutcome)
})

test_that("EikonPostProcessor handles nested and non-nested list combos", {
  testinput <- list(list(
    columnHeadersCount = 1L,
    data = list(
      c("NL0000280501", "LP60073160"),
      c("NL0009538784", ""),
      list("NL0013040330", NULL),
      list("NL0013495534", NULL),
      c("NL0014270233", "LP68611200")
    ),
    error = list(
      list(code = 416L, col = 1L,
           message = "Unable to collect data for the field 'TR.LipperRICCode' and some specific identifier(s).", row = 3L),
      list(code = 416L, col = 1L,
           message = "Unable to collect data for the field 'TR.LipperRICCode' and some specific identifier(s).", row = 4L)
    ),
    headerOrientation = "horizontal",
    headers = list(list(
      list(displayName = "Instrument"),
      list(displayName = "Lipper RIC", field = "TR.LIPPERRICCODE")
    )),
    rowHeadersCount = 1L, totalColumnsCount = 2L, totalRowsCount = 5L
  ))

  GoodOutcome <- list(
    PostProcessedEikonGetData = structure(list(
      Instrument = c("NL0000280501", "NL0009538784", "NL0013040330", "NL0013495534", "NL0014270233"),
      Lipper.RIC = c("LP60073160", NA, NA, NA, "LP68611200")
    ), row.names = c(NA, -5L), class = "data.frame"),
    Eikon_Error_Data = structure(list(
      code = c(416L, 416L), col = c(1L, 1L),
      message = c(
        "Unable to collect data for the field 'TR.LipperRICCode' and some specific identifier(s).",
        "Unable to collect data for the field 'TR.LipperRICCode' and some specific identifier(s)."
      ),
      row = 3:4
    ), row.names = c(NA, -2L), class = "data.frame")
  )

  expect_identical(EikonPostProcessor(testinput), GoodOutcome)
})


## Test EikonGetData with long requests (live) ----

test_that("EikonGetData can handle long requests gracefully", {
  skip_if(!has_live_api(), "No live API available")
  Eikon <- RefinitivJsonConnect(getOption(".EikonApiKey"))

  fields <- c("CURRENCY", "TR.AvgDailyVolume6M", "TR.InstrumentType",
              "TR.ExchangeName", "CF_EXCHNG", "TR.ExchangeMarketIdCode",
              "TR.InstrumentIsActive")

  # Use a representative subset of RICs (not the full 500+)
  Rics <- c("UMI.BR", "DTEGn.DE", "SAPG.DE", "ALVG.DE", "NOKIA.HE",
            "NZYMb.CO", "0RUY.L", "NVJP.F")

  result <- EikonGetData(EikonObject = Eikon, rics = Rics,
                         Eikonformulas = fields, raw_output = FALSE)

  expect_type(result, "list")
  expect_s3_class(result$PostProcessedEikonGetData, "data.frame")
  expect_true(nrow(result$PostProcessedEikonGetData) > 0)
})

dump_refinitiv_options("test-refinitiv")
