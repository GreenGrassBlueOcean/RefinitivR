test_that("rd_GetHistory fails when it should", {

  expect_error(rd_GetHistory(RD = NULL, fields = c("BID", "ASK"))
               , "Parameter universe should be supplied and is not")

})

test_that("rd_GetHistory can handle requests with no fields", {

  skip_if(!has_live_api(), "No live API available")

  TestRDObject <- RDConnect()
  test_python <- rd_GetHistory(RD = TestRDObject,  universe="AAPL.O")

  TestJSONObject <- RefinitivJsonConnect()
  test_json <- rd_GetHistory(RD = TestJSONObject,  universe="AAPL.O")

  expect_equal(class(test_python), "data.frame")
  expect_equal(unique(test_python$Instrument), "AAPL.O")

  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)


  data.table::setcolorder( test_json
                           , neworder = names(test_python)
  )


  data.table::setorderv(test_python, keycol)
  data.table::setorderv(test_json, keycol)

  expect_equal(test_python, test_json)

})


test_that("rd_GetHistory can handle requests with only timeseries fields", {

  skip_if(!has_live_api(), "No live API available")


  Fields <- c("TRDPRC_1", "HIGH_1", "LOW_1", "ACVOL_UNS",
              "OPEN_PRC", "BID", "ASK", "TRNOVR_UNS", "VWAP", "BLKCOUNT", "BLKVOLUM",
              "NUM_MOVES", "TRD_STATUS", "SALTIM", "NAVALUE" )

  TestRDObject <- RDConnect()
  test_python <- rd_GetHistory(RD = TestRDObject,  universe="AAPL.O"
                              , fields =Fields)

  TestJSONObject <- RefinitivJsonConnect()
  test_json <- rd_GetHistory(RD = TestJSONObject,  universe="AAPL.O", fields =Fields)

  expect_equal(class(test_python), "data.frame")

  # The proxy may return integer-valued fields as either "integer" or "numeric"
  # depending on the Workspace/Eikon version — accept both.
  numeric_cols <- c("TRDPRC_1", "HIGH_1", "LOW_1", "ACVOL_UNS", "OPEN_PRC", "BID",
                    "ASK", "TRNOVR_UNS", "VWAP", "BLKCOUNT", "BLKVOLUM",
                    "NUM_MOVES", "TRD_STATUS", "SALTIM")
  for (col in numeric_cols) {
    expect_true(class(test_python[[col]]) %in% c("integer", "numeric"),
                info = paste("column", col, "should be integer or numeric, got:", class(test_python[[col]])))
  }
  expect_s3_class(test_python$Date, "Date")
  expect_type(test_python$Instrument, "character")
  expect_type(test_python$NAVALUE, "logical")

  expect_equal(unique(test_python$Instrument), "AAPL.O")

  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)

  data.table::setcolorder( test_python
                          , neworder = c(keycol,  sort(Fields))
  )

  data.table::setcolorder( test_json
                           , neworder = c(keycol,  sort(Fields))
  )


  data.table::setorderv(test_python, keycol)
  data.table::setorderv(test_json, keycol)

  expect_equal(test_python, test_json)

})

test_that("rd_GetHistory can handle request with simple fields", {
  skip_if(!has_live_api(), "No live API available")
  # TestRD <- check_Eikonapi(ExecutionMode = "RD")


  Universe = c("GOOG.O","MSFT.O")
  Fields =  c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
  Parameters = list("SDate" = 0,"EDate" = -3
                    ,"FRQ" = "FY", "Curn" = "EUR") #"Scale" = 6

  test_json <- suppressWarnings(rd_GetHistory( RD = RDConnect()
                                               , universe = Universe
                                               , fields = Fields
                                               , parameters = Parameters
                                               , use_field_names_in_headers = TRUE))

  Universe2 <- Universe

  test_python <- suppressWarnings(rd_GetHistory( RD = RDConnect()
                                               , universe = Universe2
                                               , fields = Fields
                                               , parameters = Parameters
                                               , use_field_names_in_headers = TRUE)
                                  )


  expect_equal(class(test_python), "data.frame")

  actual <- lapply(test_python, class)
  actual <- actual[order(names(actual))]

  expected <- list(Date = "Date", Instrument = "character"
                   , TR.GROSSPROFIT = "numeric",
                   TR.REVENUE = "numeric", TR.REVENUE.DATE ="Date")
  expected <- expected[order(names(expected))]

  expect_equal(actual, expected)


  expect_equal(sort(unique(test_python$Instrument)), c("GOOG.O", "MSFT.O"))


  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)
  #
  data.table::setorderv(test_python, keycol, order = -1)
  data.table::setorderv(test_json, keycol, order = -1)

  data.table::setcolorder( test_json, neworder = names(test_python))

  expect_equal(test_python, test_json)
})

test_that("rd_GetHistory can handle request with explicit date", {
  skip_if(!has_live_api(), "No live API available")

  Universe = c("GOOG.O","MSFT.O")
  Start = "2020-01-01T01:00:00"
  End = "2020-01-10T01:00:00"

  TestRDObject <- RDConnect()
  test_python <- rd_GetHistory( RD = TestRDObject
                       , universe = Universe
                       , start = Start
                       , end = End
                       )

  TestRDObject <- RDConnect()
  test_json <- rd_GetHistory( RD = TestRDObject
                                , universe = Universe
                                , start = Start
                                , end = End
  )

  expect_equal(class(test_python), "data.frame")

  test_json <- data.table::as.data.table(test_json)
  test_python <- data.table::as.data.table(test_python)

  data.table::setcolorder( test_json, neworder = names(test_python))

  expect_equal(test_json, test_python)

})


test_that("rd_GetHistory can handle with fields and dates", {
  skip_if(!has_live_api(), "No live API available")

  Universe <- "AAPL.O"
  Fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)"
             ,"TR.FreeFloatPct()/100/*FreefloatWeight*/"
             ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
             ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")

  Parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01")

  test_json <- rd_GetHistory( RD = RDConnect(), universe= Universe
                            , fields = Fields
                            , parameters = Parameters)

  test_python <-  rd_GetHistory(RD = RDConnect(),  universe= Universe
                               , fields = Fields, parameters = Parameters)

  expect_equal(class(test_json), "data.frame")
  expect_equal(sort(unique(test_json$Date))
              , structure(c(18536, 18562, 18563, 18564, 18565, 18567, 18568,
                            18569, 18570, 18571, 18572, 18575, 18576, 18577, 18578, 18579,
                            18582, 18583, 18584, 18585, 18586, 18589, 18590, 18591, 18593,
                            18596, 18597), class = "Date"))

  expect_equal(sort(unique(test_json$Instrument)), c("AAPL.O"))

  test_json <- data.table::as.data.table(test_json)
  test_python <- data.table::as.data.table(test_python)

  data.table::setcolorder( test_json, neworder = names(test_python))

  expect_equal(test_json, test_python)

})


test_that("rd_GetHistory will not handle requests ", {

  skip_if(!has_live_api(), "No live API available")
  universe = c("GOOG.O","MSFT.O")
  fields =  c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
  parameters = list("Scale" = 6,"SDate" = 0,"EDate" = -3
                    ,"FRQ" = "FY", "Curn" = "EUR")


  expect_error(rd_GetHistory( RD = NULL, universe = universe
                            , fields = fields
                            , parameters = parameters
                            , use_field_names_in_headers = FALSE),
               "Fields should not contain Tr.Eikonformula.date  and use_field_names_in_headers = FALSE for use in GetHistory function removing TR.Revenue.date you will receive than multiple columns that have the same name (Date) and is therefore forbidden"
               , fixed = TRUE)
})



test_that("rd_GetHistory will handle requests with multiple instruments and one field",{

  skip_if(!has_live_api(), "No live API available")

  rics <- c("AAPL.O", "NVDA.O")
  fields <- "TR.ClosePrice"
  StartDate <- "2020-01-02"
  EndDate <- "2020-01-10"

  timeseries1_python <-  rd_GetHistory(universe= rics
                                      , fields = fields
                                      , RD = RDConnect()
                                      , start = StartDate, end = EndDate)


  timeseries1_JSON <-  rd_GetHistory(universe= rics
                                       , fields = fields
                                       , RD = RDConnect()
                                       , start = StartDate, end = EndDate)


  timeseries1_python <- timeseries1_python[order(names(timeseries1_python))]
  timeseries1_JSON <- timeseries1_JSON[order(names(timeseries1_JSON))]


  expect_equal(timeseries1_python, timeseries1_JSON)



})


test_that("rd_GetHistory will handle requests with one instruments and multiple fields",{

  skip_if(!has_live_api(), "No live API available")

  rics <- c("AAPL.O")
  Fields <- c("TR.ClosePrice", "TR.OpenPrice")
  StartDate <- "2020-01-02"
  EndDate <- "2020-01-10"

  timeseries2_python <- rd_GetHistory(universe=rics
                                     , fields = Fields
                                     , RD = RDConnect()
                                     , start = StartDate, end = EndDate)

  timeseries2_JSON <-  rd_GetHistory(universe=rics
                                    , fields = Fields
                                    , RD = RDConnect()
                                    , start = StartDate,  end = EndDate)

  expect_equal(timeseries2_python, timeseries2_JSON)

})

test_that("rd_GetHistory can handle timedates", {
  skip_if(!has_live_api(), "No live API available")

  rics <- "AAPL.O"
  Eikonformulas <- "TR.IssueMarketCap(Scale=6,ShType=FFL,Curn=USD)"

  test_python <- Refinitiv::rd_GetHistory( universe = rics, RD = RDConnect()
                                       , fields = Eikonformulas
                                       , start = "2020-10-27T01:00:00"
                                       , end = "2020-12-01T01:00:00"
                                       , parameters = NULL, adjustments = NULL
                                       )

  test_json <- Refinitiv::rd_GetHistory( universe = rics, RD = RDConnect()
                                           , fields = Eikonformulas
                                           , start = "2020-10-27T01:00:00"
                                           , end = "2020-12-01T01:00:00"
                                           , parameters = NULL, adjustments = NULL
  )


  test_python <- test_python[order(names(test_python))]
  test_json <- test_json[order(names(test_json))]

  keycol <- c("Date", "Instrument")
  test_python <- data.table::as.data.table(test_python)
  test_json <- data.table::as.data.table(test_json)


  data.table::setcolorder( test_json
                           , neworder = names(test_python)
  )


  data.table::setorderv(test_python, keycol)
  data.table::setorderv(test_json, keycol)





  expect_equal(test_python, test_json)

})


test_that("rd_GetHistory can handle problematic fields",{

  skip_if(!has_live_api(), "No live API available")

  fields <- c('BID', 'ASK', 'TRDPRC_1', 'SETTLE')
  universe <- c("AAPL.O", "NVDA.O")

  timeseries2_python <- suppressMessages(suppressWarnings(rd_GetHistory(universe=universe
                                      , fields = fields
                                      , RD = RDConnect()
                                      )))
  timeseries2_JSON <- suppressMessages(suppressWarnings(rd_GetHistory(universe=universe
                                  , fields = fields
                                  , RD = RDConnect()
                                  )))
  timeseries2_python <- timeseries2_python[order(names(timeseries2_python))]
  timeseries2_JSON <- timeseries2_JSON[order(names(timeseries2_JSON))]


  keycol <- c("Date", "Instrument")
  timeseries2_python <- data.table::as.data.table(timeseries2_python)
  timeseries2_JSON <- data.table::as.data.table(timeseries2_JSON)


  data.table::setcolorder( timeseries2_JSON
                           , neworder = names(timeseries2_python)
  )


  data.table::setorderv(timeseries2_python, keycol)
  data.table::setorderv(timeseries2_JSON, keycol)



  expect_equal(timeseries2_python, timeseries2_JSON)



})


test_that("rd_GetHistory can handle non existing rics",{

  skip_if(!has_live_api(), "No live API available")

  fields <- c('BID', 'ASK', 'TRDPRC_1', "TR.IssueMarketCap(Scale=6,ShType=FFL,Curn=USD)")


  NotExistingOption_json <- rd_GetHistory( universe = c("AAPLA212216000.U^22"),
                                         , interval = "P1D", count = 20L, RD = RDConnect()
                                         , fields = fields ) |> suppressWarnings()

  CorrectOutcome <- structure(list( Instrument = character(0)
                                  , Date = structure(numeric(0), class = "Date")
                                  , `TR.ISSUEMARKETCAP(SCALE=6,SHTYPE=FFL,CURN=USD)` = logical(0))
                              , row.names = integer(0), class = "data.frame")

  NotExistingOption_json <- NotExistingOption_json[order(names(NotExistingOption_json))]
  CorrectOutcome <- CorrectOutcome[order(names(CorrectOutcome))]

  expect_equal(NotExistingOption_json, CorrectOutcome)

})


library(testthat)
library(mockery)
library(data.table)

#####
# 1. Error & Input Validation Tests (live)
#####

test_that("rd_GetHistory fails when universe is not supplied", {
  expect_error(
    rd_GetHistory(RD = NULL, fields = c("BID", "ASK")),
    "Parameter universe should be supplied and is not"
  )
})

test_that("rd_GetHistory errors on problematic fields with use_field_names_in_headers=FALSE", {
  expect_error(
    rd_GetHistory(RD = list(), universe = "AAPL.O", fields = "TR.Eikonformula.date", use_field_names_in_headers = FALSE),
    "Fields should not contain Tr.Eikonformula.date"
  )
})

#####
# 2. Live API tests (if available)
#####
test_that("rd_GetHistory can handle requests with no fields", {
  skip_if(!has_live_api(), "No live API available")
  TestRDObject <- RDConnect()
  res_rd <- rd_GetHistory(RD = TestRDObject, universe = "AAPL.O")

  TestJSONObject <- RefinitivJsonConnect()
  res_json <- rd_GetHistory(RD = TestJSONObject, universe = "AAPL.O")

  expect_true(is.data.frame(res_rd))
  expect_equal(unique(res_rd$Instrument), "AAPL.O")

  # Compare the two outputs (after ordering and aligning columns)
  dt_rd <- data.table::as.data.table(res_rd)
  dt_json <- data.table::as.data.table(res_json)
  data.table::setcolorder(dt_json, neworder = names(dt_rd))
  data.table::setorderv(dt_rd, c("Date", "Instrument"))
  data.table::setorderv(dt_json, c("Date", "Instrument"))
  expect_equal(dt_rd, dt_json)
})

#####
# 3. Stubbed Tests for the JSON branch using mockery::stub
#####
test_that("rd_GetHistory (JSON branch) processes stubbed GetData and HistoricalPricing outputs correctly", {
  # Force JSON branch and set HistoricalPricingFields to force field split.
  withr::local_options(.RefinitivPyModuleName = "JSON", HistoricalPricingFields = "BID")

  # Create a dummy RD object; not used by the stubs below.
  dummy_RD <- list()

  # Stub CheckifCustomInstrument to always return FALSE.
  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)

  # Create dummy data.tables to mimic outputs:
  # rd_GetData returns data with the "ASK" field.
  dummy_GetData <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    ASK = c(110, 111, 112)
  )
  # rd_GetHistoricalPricing returns data with the "BID" field.
  dummy_HistPricing <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )

  # Stub out external calls so that the function uses our dummy data.
  stub(rd_GetHistory, "rd_GetData", function(...) dummy_GetData)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) dummy_HistPricing)

  # Call rd_GetHistory with fields forcing both branches.
  res <- rd_GetHistory(
    RD = dummy_RD,
    universe = "AAPL.O",
    fields = c("BID", "ASK"),
    start = "2020-01-01",
    end = "2020-01-03",
    debug = TRUE
  )

  expect_true(is.data.frame(res))
  # Expect both BID and ASK columns present after merge.
  expect_true("BID" %in% names(res))
  expect_true("ASK" %in% names(res))
  expect_equal(nrow(res), 3)
  expect_equal(unique(res$Instrument), "AAPL.O")
  # Q2: merge_info attribute attached
  mi <- attr(res, "merge_info")
  expect_type(mi, "list")
  expect_equal(mi$merge_type, "full_outer")
})


#####
# 4. Stubbed tests for date-format warnings
#####

test_that("rd_GetHistory warns when SDate and EDate in parameters are in invalid format", {
  # Force JSON branch.
  withr::local_options(.RefinitivPyModuleName = "JSON")

  # Stub dateFormatCheck to always return FALSE (simulate invalid date format).
  stub(rd_GetHistory, "dateFormatCheck", function(x) FALSE)

  # Create a dummy RD object.
  dummy_RD <- list()

  # Return minimal but non-empty data.tables that have Date & Instrument columns
  dummy_dt_data <- data.table::data.table(
    Date = as.Date("2020-01-01"),
    Instrument = "AAPL.O",
    Value = 100
  )
  dummy_dt_hist <- data.table::data.table(
    Date = as.Date("2020-01-01"),
    Instrument = "AAPL.O",
    Price = 101
  )

  # Stub out data-fetching so that we do get columns for merging & ordering.
  stub(rd_GetHistory, "rd_GetData", function(...) dummy_dt_data)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) dummy_dt_hist)

  # Provide parameters with SDate and EDate in an invalid format.
  params <- list(SDate = "invalid_date", EDate = "invalid_date")

  # Capture warnings during the call.
  warns <- testthat::capture_warnings(
    res <- rd_GetHistory(RD = dummy_RD, universe = "AAPL.O", parameters = params)
  )

  expect_true(any(grepl("List parameters has 'SDate'", warns)),
              info = "Expected warning about invalid SDate format not found.")
  expect_true(any(grepl("List parameters has 'EDate'", warns)),
              info = "Expected warning about invalid EDate format not found.")
  # Confirm result is a data.frame, so the function can proceed after warnings
  expect_true(is.data.frame(res))
})

test_that("rd_GetHistory uses SDate and EDate from parameters without warning when valid", {
  # Force JSON branch.
  withr::local_options(.RefinitivPyModuleName = "JSON")

  # Stub dateFormatCheck to always return TRUE (simulate valid date format).
  stub(rd_GetHistory, "dateFormatCheck", function(x) TRUE)

  # Create a dummy RD object.
  dummy_RD <- list()

  # Again, return minimal but non-empty data.tables with Date & Instrument
  dummy_dt_data <- data.table::data.table(
    Date = as.Date("2020-01-01"),
    Instrument = "AAPL.O",
    Value = 100
  )
  dummy_dt_hist <- data.table::data.table(
    Date = as.Date("2020-01-01"),
    Instrument = "AAPL.O",
    Price = 101
  )

  stub(rd_GetHistory, "rd_GetData", function(...) dummy_dt_data)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) dummy_dt_hist)

  # Provide valid date strings.
  params <- list(SDate = "2020-10-27", EDate = "2020-12-01")

  # Capture warnings.
  warns <- testthat::capture_warnings(
    res <- rd_GetHistory(RD = dummy_RD, universe = "AAPL.O", parameters = params)
  )

  # No warnings for valid format
  expect_equal(length(warns), 0, info = "No warnings should be produced with valid SDate/EDate.")

  # Confirm the function returns a data.frame (with the stubbed data).
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1)
  expect_equal(unique(res$Instrument), "AAPL.O")
})

#####
# 5. Q2 merge-fix tests
#####

# Helper: creates a minimal stubbed rd_GetHistory call that exercises
# the merge logic without hitting any network endpoint.
.stub_merge_test <- function(hist_pricing_dt, get_data_dt,
                             fields = c("BID", "ASK"),
                             hist_pricing_fields = "BID") {
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = hist_pricing_fields
  )
  dummy_RD <- list()
  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetData", function(...) data.table::copy(get_data_dt))
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) data.table::copy(hist_pricing_dt))
  rd_GetHistory(
    RD = dummy_RD,
    universe = "AAPL.O",
    fields = fields,
    start = "2020-01-01",
    end = "2020-12-31"
  )
}


test_that("merge attaches merge_info attribute when both sides contribute", {
  hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )
  gd <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    ASK = c(110, 111, 112)
  )
  res <- .stub_merge_test(hp, gd)
  mi <- attr(res, "merge_info")
  expect_type(mi, "list")
  expect_equal(mi$pricing_rows, 3L)
  expect_equal(mi$fundamental_rows, 3L)
  expect_equal(mi$merge_type, "full_outer")
  expect_equal(mi$final_rows, nrow(res))
})


test_that("granularity mismatch warning fires when ratio > 5:1", {
  # 20 daily pricing rows vs 2 quarterly fundamental rows (ratio 10:1)
  hp <- data.table(
    Date = as.Date("2020-01-01") + 0:19,
    Instrument = rep("AAPL.O", 20),
    BID = seq(100, 119)
  )
  gd <- data.table(
    Date = as.Date(c("2020-03-31", "2020-06-30")),
    Instrument = rep("AAPL.O", 2),
    ASK = c(110, 115)
  )
  expect_warning(
    res <- .stub_merge_test(hp, gd),
    "Date granularity mismatch"
  )
  # All data is preserved despite the warning
  expect_true("BID" %in% names(res))
  expect_true("ASK" %in% names(res))
  # At least as many rows as the larger side
  expect_gte(nrow(res), 20L)
})


test_that("no granularity warning when date counts are similar", {
  hp <- data.table(
    Date = as.Date("2020-01-01") + 0:4,
    Instrument = rep("AAPL.O", 5),
    BID = 100:104
  )
  gd <- data.table(
    Date = as.Date("2020-01-01") + 0:4,
    Instrument = rep("AAPL.O", 5),
    ASK = 110:114
  )
  expect_no_warning(res <- .stub_merge_test(hp, gd))
  expect_equal(nrow(res), 5L)
})


test_that("unique(by = ...) prevents many-to-many join from duplicate keys", {
  # Simulate duplicate (Instrument, Date) rows on both sides
  hp <- data.table(
    Date = rep(as.Date("2020-01-01"), 2),
    Instrument = rep("AAPL.O", 2),
    BID = c(100, 100.5)
  )
  gd <- data.table(
    Date = rep(as.Date("2020-01-01"), 2),
    Instrument = rep("AAPL.O", 2),
    ASK = c(110, 110.5)
  )
  # Without unique(by=...), merge would produce 4 rows (2x2 Cartesian).
  # With the fix, each side is deduped to 1 row first, so result is 1 row.
  res <- .stub_merge_test(hp, gd)
  expect_equal(nrow(res), 1L)
  expect_equal(res$Instrument, "AAPL.O")
})


test_that("all-NA row filter correctly removes Date-only noise rows", {
  # Pricing has dates 1-3; fundamentals has only date 4.
  # Outer join creates dates 1-4, where date 4 has NA for BID
  # and dates 1-3 have NA for ASK.
  hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )
  gd <- data.table(
    Date = as.Date("2020-01-04"),
    Instrument = "AAPL.O",
    ASK = 115
  )
  res <- .stub_merge_test(hp, gd)
  # All 4 rows have at least one non-NA data column, so all survive

  expect_equal(nrow(res), 4L)

  # Now test with a fundamentals row that has Date+Instrument but NA data
  gd_with_na <- data.table(
    Date = as.Date("2020-01-04"),
    Instrument = "AAPL.O",
    ASK = NA_real_
  )
  res2 <- .stub_merge_test(hp, gd_with_na)
  # Date 2020-01-04 has BID=NA (no pricing) and ASK=NA (explicit NA),
  # so all data columns are NA → should be filtered out
  expect_equal(nrow(res2), 3L)
})


test_that("empty columns_to_check does not delete all rows", {
  # Edge case: result has only Instrument and Date columns, no data fields.
  # Use only HistoricalPricing (no GetData fields) to trigger single-side branch.
  withr::local_options(
    .RefinitivPyModuleName = "JSON",
    HistoricalPricingFields = "BID"
  )
  dummy_RD <- list()
  dummy_hp <- data.table(
    Date = as.Date("2020-01-01") + 0:2,
    Instrument = rep("AAPL.O", 3),
    BID = c(100, 101, 102)
  )
  stub(rd_GetHistory, "CheckifCustomInstrument", function(symbol, UUID) FALSE)
  stub(rd_GetHistory, "rd_GetHistoricalPricing", function(...) dummy_hp)

  # Only pricing fields requested → no GetData call → single-side branch
  res <- rd_GetHistory(
    RD = dummy_RD,
    universe = "AAPL.O",
    fields = "BID",
    start = "2020-01-01",
    end = "2020-01-03"
  )
  expect_equal(nrow(res), 3L)
})


test_that("instruments only on one side do not cause division-by-zero in granularity check", {
  # AAPL.O on both sides; MSFT.O only on pricing side.
  # Inner join in .check_date_granularity excludes MSFT.O entirely.
  hp <- data.table(
    Date = rep(as.Date("2020-01-01") + 0:19, 2),
    Instrument = rep(c("AAPL.O", "MSFT.O"), each = 20),
    BID = seq_len(40)
  )
  gd <- data.table(
    Date = as.Date(c("2020-03-31", "2020-06-30")),
    Instrument = rep("AAPL.O", 2),
    ASK = c(110, 115)
  )
  # Should warn about AAPL.O (ratio 10:1), not error about MSFT.O
  expect_warning(
    res <- .stub_merge_test(hp, gd, fields = c("BID", "ASK")),
    "AAPL.O"
  )
  # Both instruments present in result
  expect_true("AAPL.O" %in% res$Instrument)
  expect_true("MSFT.O" %in% res$Instrument)
})


dump_refinitiv_options("test-rd_GetHistory")
