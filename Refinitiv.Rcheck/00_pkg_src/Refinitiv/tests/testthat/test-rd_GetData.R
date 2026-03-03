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
    how = function(x, use_field_names_in_headers, SpaceConvertor) {
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






test_that("rd_GetData satisfies basic request", {

  skip_if(!has_live_api(), "No live API available")

  RICS <- c("AAPL.O", "GOOG.O", "III.L", "MMM")
  Eikonformulas <- c("TR.PriceClose.Currency", "TR.InstrumentType", "TR.ExchangeName",
                     "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive", "TR.ISINCode")

  result <- rd_GetData(rics = RICS, Eikonformulas = Eikonformulas,
                       RD = RDConnect())

  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(c("Instrument", "Currency", "Instrument Type", "Exchange Name",
                                           "Exchange Market Identifier Code", "Instrument Is Active Flag",
                                           "ISIN Code")))
  expect_equal(sort(unique(result$Instrument)), sort(RICS))
})



test_that("rd_GetData can handle with fields and dates", {
  skip_if(!has_live_api(), "No live API available")

  rics <- c("AAPL.O")
  fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)", "TR.FreeFloatPct()/100/*FreefloatWeight*/",
              "TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/", "TR.CLOSEPRICE(Adjusted=0)/*close*/")

  parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01", "Fill" = "None")

  result <- rd_GetData(RD = RDConnect(), rics = rics,
                       Eikonformulas = fields, Parameters = parameters,
                       use_field_names_in_headers = TRUE, SyncFields = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(sort(unique(result$Instrument)), "AAPL.O")
})
#



test_that("rd_GetData can handle previous problem case", {
  skip_if(!has_live_api(), "No live API available")


asset <- "AAPL.O"

# Retrieve corporate events
corp_event_rd <- rd_GetData( SpaceConvertor = ".",  RD = RDConnect(),
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

dump_refinitiv_options("test-rd_GetData")
