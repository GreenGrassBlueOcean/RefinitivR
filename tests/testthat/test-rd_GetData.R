library(testthat)
library(mockery)
library(data.table)

# --- Dummy RDObject Simulators ---

# Dummy RDObject for JSON branch.
dummy_rd_json <- list(
  get_data = function(instruments, fields, parameters, SyncFields, debug, raw_output) {
    # Return dummy content that rd_GetData will pass to processing.
    list(dummy_json_chunk = "some data")
  },
  set_app_key = function(app_key) { }
)

# Dummy RDObject for refinitiv.data branch.
# This dummy object returns a structure with a to_json() method.
dummy_rd_refdata <- list(
  get_data = function(universe, fields, parameters, use_field_names_in_headers) {
    structure(
      list(
        to_json = function(date_format) {
          '{"dummy_refdata_result": 999}'
        }
      ),
      class = "dummy_pycall"
    )
  },
  set_app_key = function(app_key) { }
)

# --- Unit Tests ---

test_that("rd_GetData calls rd_OutputProcesser in JSON mode", {
  # Force the JSON branch by stubbing getOption to return "JSON"
  stub(
    where = rd_GetData,
    what = "getOption",
    how = function(...) {
      if (...[[1]] == ".RefinitivPyModuleName") return("JSON")
      base::getOption(...[[1]], ...)
    }
  )

  # Stub the JSON processing function (which should be rd_OutputProcesser)
  json_called <- FALSE
  stub(
    where = rd_GetData,
    what = "rd_OutputProcesser",
    how = function(x, use_field_names_in_headers, NA_cleaning, SpaceConvertor) {
      json_called <<- TRUE
      data.frame(mocked_json_result = 42)
    }
  )

  # Call rd_GetData with the dummy JSON RDObject.
  result <- rd_GetData(
    RDObject = dummy_rd_json,
    rics = c("MOCKED_RIC"),
    Eikonformulas = "MOCKED_FORMULA",
    raw_output = FALSE
  )

  expect_true(json_called)
  expect_s3_class(result, "data.frame")
  expect_true("mocked_json_result" %in% names(result))
  expect_equal(result$mocked_json_result, 42)
})

test_that("rd_GetData calls Process_RDP_output in refinitiv.data mode", {
  # Force the refinitiv.data branch by stubbing getOption to return "refinitiv.data"
  stub(
    where = rd_GetData,
    what = "getOption",
    how = function(...) {
      if (...[[1]] == ".RefinitivPyModuleName") return("refinitiv.data")
      base::getOption(...[[1]], ...)
    }
  )

  # Stub the refinitiv.data processing function
  refdata_called <- FALSE
  stub(
    where = rd_GetData,
    what = "Process_RDP_output",
    how = function(x, RemoveNA, SpaceConvertor) {
      refdata_called <<- TRUE
      data.frame(mocked_refdata_result = 999)
    }
  )

  # Call rd_GetData with the dummy refinitiv.data RDObject.
  result <- rd_GetData(
    RDObject = dummy_rd_refdata,
    rics = c("FAKE_RIC"),
    Eikonformulas = "FAKE_FORMULA",
    raw_output = FALSE
  )

  expect_true(refdata_called)
  expect_s3_class(result, "data.frame")
  expect_true("mocked_refdata_result" %in% names(result))
})




test_that("rd_GetData satisfies basic request", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  RICS = c("AAPL.O", "GOOG.O", "III.L", "MMM")
  Eikonformulas = c("TR.PriceClose.Currency", "TR.InstrumentType", "TR.ExchangeName"
                   , "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive","TR.ISINCode")

  # not working in RD "CF_CURR", "CF_EXCHNG"

  test3_RD <- rd_GetData(rics =  RICS, Eikonformulas = Eikonformulas
                      , RD = RDConnect(PythonModule = "RD") )

  test3_json <- rd_GetData(rics =  RICS, Eikonformulas = Eikonformulas
                           , RD = RDConnect(PythonModule = "JSON") )

  test3_json <- data.table::as.data.table(test3_json)
  test3_RD <- data.table::as.data.table(test3_RD)

  data.table::setcolorder( test3_json, neworder = names(test3_RD))


  expect_equal(test3_RD, test3_json)
  expect_equal(sort(names(test3_RD)), sort(c("Instrument", "Currency", "Instrument Type", "Exchange Name",
                                        "Exchange Market Identifier Code", "Instrument Is Active Flag",
                                        "ISIN Code")))

  expect_equal(sort(unique(test3_RD$Instrument))
              , sort(RICS))

})



test_that("rd_GetData can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  rics <- c("AAPL.O")
  fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
              ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")

  parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01", "Fill" ="None")

  test_json <- rd_GetData(RD = RDConnect(PythonModule = "JSON"), rics =  rics
                             , Eikonformulas =  fields
                             , Parameters = parameters
                          , use_field_names_in_headers = TRUE
                          , SyncFields = FALSE)

  test_python <-  rd_GetData(RD = RDConnect(PythonModule = "RD"),  rics= rics
                                , Eikonformulas = fields
                               , Parameters = parameters
                             , use_field_names_in_headers = TRUE)

  expect_equal(class(test_json), "data.frame")


  test_json <- data.table::as.data.table(test_json)
  test_python <- data.table::as.data.table(test_python)

  data.table::setcolorder( test_json, neworder = names(test_python))


  expect_equal(sort(unique(test_python$Instrument)), c("AAPL.O"))
  expect_equal(test_json, test_python)

})
#



test_that("rd_GetData can handle previous problem case", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))


asset <- "AAPL.O"

# Retrieve corporate events
corp_event_rd <- rd_GetData( SpaceConvertor = ".",  RD = RDConnect(PythonModule = "JSON"),
                        , rics = asset
                        , Eikonformulas = c("TR.CAEffectiveDate", "TR.CAAdjustmentFactor", "TR.CAAdjustmentType")
                        , Parameters = list("CAEventType" = "SSP"
                                           ,"SDate" = as.character(format(Sys.Date() - lubridate::dyears(50), "%Y-%m-%d"))
                                           , "EDate" = as.character(Sys.Date())
                                           )
                                          #, use_field_names_in_headers = TRUE
                                          #, SyncFields = TRUE
                                          #,
                                          )
corp_event_rd$Capital.Change.Effective.Date <- as.character(corp_event_rd$Capital.Change.Effective.Date)

corp_event_eikon <- EikonGetData(EikonObject = EikonConnect()
                          , rics = asset
                          , Eikonformulas = c("TR.CAEffectiveDate", "TR.CAAdjustmentFactor", "TR.CAAdjustmentType")
                          , Parameters = list("CAEventType" = "SSP"
                                             ,"SDate" = as.character(format(Sys.Date() - lubridate::dyears(50), "%Y-%m-%d"))
                                             , "EDate" = as.character(Sys.Date()))

                                            #, use_field_names_in_headers = TRUE
                                            #, SyncFields = TRUE

                          )

testthat::expect_equal(corp_event_rd, corp_event_eikon$PostProcessedEikonGetData)

})
