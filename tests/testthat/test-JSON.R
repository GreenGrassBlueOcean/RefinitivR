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


test_that("jsonDataGridConstructor works with parameters as empty list", {
payload <- list(universe = "AAPL.O", fields = c("TR.Revenue", "TR.GrossProfit") , parameters = list())
TestOutcome <- jsonDataGridConstructor(payload)

CorrectOutcome <- "{ \"universe\" : [ \"AAPL.O\" ] , \"fields\" : [ \"TR.Revenue\",\"TR.GrossProfit\" ] }"
expect_equal(TestOutcome, CorrectOutcome)
})



test_that("RefinitivJsonConnect does not work without application id", {

  originalOptionValue = getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)

  if (is.null(getOption(".EikonApiKey"))){
    expect_error( suppressWarnings(RefinitivJsonConnect())
                  , "Please supply Eikonapplication_id")
  }

  options(.EikonApiKey = originalOptionValue)
})


test_that("RefinitivJsonConnect does work with application id", {

  originalOptionValue = getOption(".EikonApiKey")
  original_EikonPort = getOption("eikon_port")

  options(.EikonApiKey = "testing_key")

  EikonJson <- suppressWarnings(RefinitivJsonConnect(Eikonapplication_port = 9000L))

  expect_equal(names(EikonJson)
              , c(
                "get_intraday_custominstrument_pricing", "create_custom_instrument",
                "search", "set_app_port", "get_rdp_streaming_url", "get_interday_custominstrument_pricing",
                "get_data", "rd_get_news_headlines", "rd_get_news_story", "manage_custom_instrument",
                "set_app_key", "get_symbology", "get_historical_pricing", "get_search_metadata",
                "get_news_story", "get_app_port", "get_timeseries", "get_news_headlines",
                "get_app_key", "search_custom_instrument", "get_data_rdp"
              )

              )

  expect_equal(EikonJson$get_app_key(), "testing_key")

  options(eikon_port = original_EikonPort)
  options(.EikonApiKey = originalOptionValue)
})

test_that("Construct_url does work correctly", {

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

  expect_error( Construct_url(service = "wrongservice", EndPoint = "discovery/search/v1/")
                , "wrong service selected in function Construct_url, only rdp, udf or eikon allowed but wrongservice is chosen"
  )


  options(eikon_port = original_EikonPort)
  options(eikon_api = original_EikonApi)
  options(rdp_port = original_rdp_port)
  options(rdp_api = original_rdp_api)
  options(refinitiv_base_url = original_base_url)


})

test_that("send_json_request can make a GET and POST request", {

  original_APIKEY = getOption(".EikonApiKey")
  options(.EikonApiKey = "testing_key")



  test_GET <- send_json_request(json = list(), request_type = "GET"
                                , url = "https://fakerapi.it/api/v1/companies?_seed=12456")

  expect_equal(lapply(test_GET, class)
              , list(status = "character", code = "integer", locale = "character",
                     seed = "character", total = "integer", data = "list"))

  directions <- 'DataGrid_StandardAsync'
  payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
                                         'fields' = lapply(list("TR.RICCode"), function(x) list("name" = x))
                                         )))

  json <- json_builder(directions, payload)

  test_POST <- send_json_request(json = json, request_type = "POST"
                                , url = "https://jsonplaceholder.typicode.com/todos/1/posts")


  expect_equal(test_POST, list(Entity = list(E = "DataGrid_StandardAsync", W = list(requests = list(
    list(instruments = list("TSLA.O"), fields = list(list(name = "TR.RICCode")))))),
    todoId = "1", id = 101L))


  options(.EikonApiKey = original_APIKEY)

})

