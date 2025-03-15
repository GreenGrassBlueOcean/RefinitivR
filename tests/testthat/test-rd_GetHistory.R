test_that("rd_GetHistory fails when it should", {

  expect_error(rd_GetHistory(RD = NULL, fields = c("BID", "ASK"))
               , "Parameter universe should be supplied and is not")

})

test_that("rd_GetHistory can handle requests with no fields", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  TestRDObject <- RDConnect(PythonModule = "RD")
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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))


  Fields <- c("TRDPRC_1", "HIGH_1", "LOW_1", "ACVOL_UNS",
              "OPEN_PRC", "BID", "ASK", "TRNOVR_UNS", "VWAP", "BLKCOUNT", "BLKVOLUM",
              "NUM_MOVES", "TRD_STATUS", "SALTIM", "NAVALUE" )

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python <- rd_GetHistory(RD = TestRDObject,  universe="AAPL.O"
                              , fields =Fields)

  TestJSONObject <- RefinitivJsonConnect()
  test_json <- rd_GetHistory(RD = TestJSONObject,  universe="AAPL.O", fields =Fields)

  expect_equal(class(test_python), "data.frame")

  expect_equal(lapply(test_python, class)
              , list(Date = "Date", Instrument = "character", TRDPRC_1 = "numeric",
                     HIGH_1 = "numeric", LOW_1 = "numeric", ACVOL_UNS = "integer",
                     OPEN_PRC = "numeric", BID = "numeric", ASK = "numeric", TRNOVR_UNS = "numeric",
                     VWAP = "numeric", BLKCOUNT = "integer", BLKVOLUM = "integer",
                     NUM_MOVES = "integer", TRD_STATUS = "integer", SALTIM = "integer",
                     NAVALUE = "logical")
  )

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
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  # TestRD <- check_Eikonapi(ExecutionMode = "RD")


  Universe = c("GOOG.O","MSFT.O")
  Fields =  c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
  Parameters = list("SDate" = 0,"EDate" = -3
                    ,"FRQ" = "FY", "Curn" = "EUR") #"Scale" = 6

  test_json <- suppressWarnings(rd_GetHistory( RD = RDConnect(PythonModule = "JSON")
                                               , universe = Universe
                                               , fields = Fields
                                               , parameters = Parameters
                                               , use_field_names_in_headers = TRUE))

  Universe2 <- Universe

  test_python <- suppressWarnings(rd_GetHistory( RD = RDConnect(PythonModule = "RD")
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
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  Universe = c("GOOG.O","MSFT.O")
  Start = "2020-01-01T01:00:00"
  End = "2020-01-10T01:00:00"

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python <- rd_GetHistory( RD = TestRDObject
                       , universe = Universe
                       , start = Start
                       , end = End
                       )

  TestRDObject <- RDConnect(PythonModule = "JSON")
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
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  Universe <- "AAPL.O"
  Fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)"
             ,"TR.FreeFloatPct()/100/*FreefloatWeight*/"
             ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
             ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")

  Parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01")

  test_json <- rd_GetHistory( RD = RDConnect(PythonModule = "JSON"), universe= Universe
                            , fields = Fields
                            , parameters = Parameters)

  test_python <-  rd_GetHistory(RD = RDConnect(PythonModule = "RD"),  universe= Universe
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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))
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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- c("AAPL.O", "NVDA.O")
  fields <- "TR.ClosePrice"
  StartDate <- "2020-01-02"
  EndDate <- "2020-01-10"

  timeseries1_python <-  rd_GetHistory(universe= rics
                                      , fields = fields
                                      , RD = RDConnect(PythonModule = "RD")
                                      , start = StartDate, end = EndDate)


  timeseries1_JSON <-  rd_GetHistory(universe= rics
                                       , fields = fields
                                       , RD = RDConnect(PythonModule = "JSON")
                                       , start = StartDate, end = EndDate)


  timeseries1_python <- timeseries1_python[order(names(timeseries1_python))]
  timeseries1_JSON <- timeseries1_JSON[order(names(timeseries1_JSON))]


  expect_equal(timeseries1_python, timeseries1_JSON)



})


test_that("rd_GetHistory will handle requests with one instruments and multiple fields",{

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- c("AAPL.O")
  Fields <- c("TR.ClosePrice", "TR.OpenPrice")
  StartDate <- "2020-01-02"
  EndDate <- "2020-01-10"

  timeseries2_python <- rd_GetHistory(universe=rics
                                     , fields = Fields
                                     , RD = RDConnect(PythonModule = "RD")
                                     , start = StartDate, end = EndDate)

  timeseries2_JSON <-  rd_GetHistory(universe=rics
                                    , fields = Fields
                                    , RD = RDConnect(PythonModule = "JSON")
                                    , start = StartDate,  end = EndDate)

  expect_equal(timeseries2_python, timeseries2_JSON)

})

test_that("rd_GetHistory can handle timedates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- "AAPL.O"
  Eikonformulas <- "TR.IssueMarketCap(Scale=6,ShType=FFL,Curn=USD)"

  test_python <- Refinitiv::rd_GetHistory( universe = rics, RD = RDConnect(PythonModule = "RD")
                                       , fields = Eikonformulas
                                       , start = "2020-10-27T01:00:00"
                                       , end = "2020-12-01T01:00:00"
                                       , parameters = NULL, adjustments = NULL
                                       )

  test_json <- Refinitiv::rd_GetHistory( universe = rics, RD = RDConnect(PythonModule = "JSON")
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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  fields <- c('BID', 'ASK', 'TRDPRC_1', 'SETTLE')
  universe <- c("AAPL.O", "NVDA.O")

  timeseries2_python <- suppressMessages(suppressWarnings(rd_GetHistory(universe=universe
                                      , fields = fields
                                      , RD = RDConnect(PythonModule = "RD")
                                      )))
  timeseries2_JSON <- suppressMessages(suppressWarnings(rd_GetHistory(universe=universe
                                  , fields = fields
                                  , RD = RDConnect(PythonModule = "JSON")
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

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  fields <- c('BID', 'ASK', 'TRDPRC_1', "TR.IssueMarketCap(Scale=6,ShType=FFL,Curn=USD)")


  NotExistingOption_json <- rd_GetHistory( universe = c("AAPLA212216000.U^22"),
                                         , interval = "P1D", count = 20L, RD = RDConnect(PythonModule = "JSON")
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
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  TestRDObject <- RDConnect(PythonModule = "RD")
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
  options(.RefinitivPyModuleName = "JSON", HistoricalPricingFields = "BID")

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

  # Clean up the option so it doesn't affect other tests.
  options(HistoricalPricingFields = NULL)
})


#####
# 4. Stubbed Tests for the RD branch using a fake get_history call
#####
test_that("rd_GetHistory (refinitiv.data branch) processes a stubbed get_history call", {
  # Force refinitiv.data branch.
  options(.RefinitivPyModuleName = "refinitiv.data")

  # Create a dummy RD object with a stubbed get_history function.
  dummy_py_obj <- list(
    columns = list(
      nlevels = 1,         # Force single level columns
      name = "AAPL.O"
    ),
    # Create minimal stubs for the methods that will be called:
    insert = function(pos, colname, value) {
      self <<- list(Instrument = value)
      return(invisible(NULL))
    },
    stack = function(index) { return(dummy_py_obj) },
    reset_index = function() { return(dummy_py_obj) },
    set_index = function(col) { return(dummy_py_obj) }
  )
  # Simulate a py object that, after processing, returns a JSON string.
  dummy_py_obj$to_json <- function(date_format) '{"Date":["2020-01-01","2020-01-02","2020-01-03"],"Instrument":["AAPL.O","AAPL.O","AAPL.O"],"BID":[100,101,102]}'

  # Stub the call to RD[["get_history"]] so that it returns our dummy_py_obj.
  fake_get_history <- function(...) dummy_py_obj
  stub(rd_GetHistory, "RD[[\"get_history\"]]", fake_get_history)

  # Also, stub reticulate::py_to_r to just return the value from dummy_py_obj$columns.
  stub(rd_GetHistory, "reticulate::py_to_r", function(x, ...) { x })

  # Stub Process_RDP_output to simply convert the JSON to a data.frame.
  stub(rd_GetHistory, "Process_RDP_output", function(JsonString, RemoveNA, SpaceConvertor) {
    df <- data.frame(
      Date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
      Instrument = rep("AAPL.O", 3),
      BID = c(100, 101, 102),
      stringsAsFactors = FALSE
    )
    return(df)
  })

  # Create a dummy RD object as a list that supports the get_history call.
  dummy_RD <- list()
  dummy_RD[["get_history"]] <- fake_get_history

  res <- rd_GetHistory(
    RD = dummy_RD,
    universe = "AAPL.O",
    fields = "BID",
    debug = TRUE
  )

  expect_true(is.data.frame(res))
  expect_equal(unique(res$Instrument), "AAPL.O")
  expect_equal(nrow(res), 3)
})

#####
# 5. Cleanup: reset options if needed
#####
options(.RefinitivPyModuleName = NULL)

test_that("rd_GetHistory warns when SDate and EDate in parameters are in invalid format", {
  # Force JSON branch.
  options(.RefinitivPyModuleName = "JSON")

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
  options(.RefinitivPyModuleName = "JSON")

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
