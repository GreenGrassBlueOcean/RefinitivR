#' Title
#'
#' @param Eikonapplication_id
#' @param Eikonapplication_port
#' @param ReturnObject
#'
#' @return
#' @export
#'
#' @examples
LSEG <- function(Eikonapplication_id = NA , Eikonapplication_port = 9000L, ReturnObject = "ALL"){

  # 1. check input ----
  if (is.na(Eikonapplication_id)){
    try(Eikonapplication_id <- getOption(".EikonApiKey") )
    if(is.null(Eikonapplication_id)){stop("Please supply Eikonapplication_id")}
  }
  payload <- NULL
  options(.EikonApiKey = Eikonapplication_id)
  options(.RefinitivPyModuleName = "JSON")
  options(.RefinitivPyModuleVersion = "NA")
  options(.RefinitivPyModuleType = "direct JSON connection through httr2 package")


  EikonList <- list( get_data = EikonGetData
        , get_news_headlines = EikonGetNewsHeadlines
        , get_news_story = EikonGetNewsStory
        , get_symbology = EikonGetSymbology
        , get_timeseries = EikonGetTimeseries
        , set_app_key = function() {return(x+y)}
        , TR_Field = TR_Field
  )


  custom_instruments_list <- list()
  esg_list <- list()
  estimates_list <- list()
  filings_list <- list()
  fundamental_and_reference_list <- list()
  historical_pricing_list <- list()
  ipa_list <- list()
  news_list <- list( get_story = EikonGetNewsStory
                   , get_headlines = EikonGetNewsHeadlines)
  ownership_list <- list()
  pricing_list <- list()
  search_list <- list()
  symbol_conversion_list <- list()
  trade_data_service_list <- list()


  content_list <- list( custom_instruments = custom_instruments_list
                      , esg = esg_list
                      , estimates = estimates_list
                      , filings = filings_list
                      , fundamental_and_reference = fundamental_and_reference_list
                      , historical_pricing = historical_pricing_list
                      , ipa = ipa_list
                      , news = news_list
                      , ownership = ownership_list
                      , pricing = pricing_list
                      , search = search_list
                      , symbol_conversion = symbol_conversion_list
                      , trade_data_service = trade_data_service_list
  )






  LSEG_0bject <- rlang::env(Eikon = EikonList
                           , RD = list( content = content_list
                                      , Eikon = EikonList
                                      , get_data = rd_GetData
                                      , get_history = rd_GetHistory
                                      )
                           )

  if(ReturnObject == toupper("all")){
    return(LSEG_0bject)
  } else if(ReturnObject == toupper("RD")){
    return(LSEG_0bject$RD)
  } else if(ReturnObject == toupper("Eikon")){
    return(LSEG_0bject$Eikon)

  }
}
