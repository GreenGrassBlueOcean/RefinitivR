test_that("json_builder works", {

  directions <- 'DataGrid_StandardAsync'
  payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
        'fields' = lapply(list("TR.RICCode"), \(x) list("name" = x))
      )))

  TestOutcome <- json_builder(directions, payload)
  CorrectOutcome <- list(Entity = list(E = "DataGrid_StandardAsync"
                                      , W = list(requests = list(list(instruments = list("TSLA.O")
                                                                      , fields = list(list(name = "TR.RICCode")))))))

  expect_equal(TestOutcome, CorrectOutcome)
})

test_that("jsonDataGridConstructor works with simple request", {

payload <- list(universe = "AAPL.O", fields = c("BID", "ASK"))
TestOutcome <- jsonDataGridConstructor(payload)
CorrectOutcome <- "{ \"universe\" : [ \"AAPL.O\" ] , \"fields\" : [ \"BID\",\"ASK\" ] }"

expect_equal(TestOutcome, CorrectOutcome)
})

test_that("jsonDataGridConstructor works with complex request", {

  payload <- list(universe = "AAPL.O"
                , fields = c("TR.Revenue", "TR.GrossProfit")
                , parameters = list("Curn" = "USD", "SDate" = "2020-10-27"
                                    , "EDate" = "2020-12-01"))

  TestOutcome <- jsonDataGridConstructor(payload)
  CorrectOutcome <- "{ \"universe\" : [ \"AAPL.O\" ] , \"fields\" : [ \"TR.Revenue\",\"TR.GrossProfit\" ] , \"parameters\" : { \"Curn\" : \"USD\" , \"SDate\" : \"2020-10-27\" , \"EDate\" : \"2020-12-01\" } }"

  expect_equal(TestOutcome, CorrectOutcome)
})

test_that("RefinitivJsonConnect does not work without application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)

  if (is.null(getOption(".EikonApiKey"))){
    expect_error( RefinitivJsonConnect()
                  , "Please supply Eikonapplication_id")
  }

  options(.EikonApiKey = originalOptionValue)
})


test_that("RefinitivJsonConnect does work with application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = "testing_key")

  EikonJson <- RefinitivJsonConnect(Eikonapplication_port = 9000L)

  expect_equal(names(EikonJson)
              , c("search", "set_app_port", "get_data", "set_app_key", "get_symbology",
                  "get_search_metadata", "get_history", "get_app_port", "get_timeseries",
                  "get_app_key"))

  expect_equal(EikonJson$get_app_key(), "testing_key")

  options(.EikonApiKey = originalOptionValue)
})



