test_that("json_builder works", {

  directions <- 'DataGrid_StandardAsync'
  payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
        'fields' = lapply(list("TR.RICCode"), function(x) list("name" = x))
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

test_that("Construct_url does work correctlt", {

  original_EikonPort = getOption("eikon_port")
  original_EikonApi = getOption("eikon_api")
  original_rdp_port = getOption("rdp_port")
  original_rdp_api = getOption("rdp_api")
  original_base_url = getOption("refinitiv_base_url")


  options(refinitiv_base_url = 'http://localhost')
  options(eikon_port = 9000L)
  options(eikon_api = '/api/v1/data')
  options(rdp_api = '/api/rdp/')
  options(rdp_port=9060L)


  expect_equal( Construct_url(service = "eikon")
              ,  "http://localhost:9000/api/v1/data"
              )

  expect_equal( Construct_url(service = "rdp", EndPoint = "discovery/search/v1/")
                , "http://localhost:9060/api/rdp/discovery/search/v1/"
  )

  options(eikon_port = original_EikonPort)
  options(eikon_api = original_EikonApi)
  options(rdp_port = original_rdp_port)
  options(rdp_api = original_rdp_api)
  options(refinitiv_base_url = original_base_url)


})


