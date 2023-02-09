#' Builds a json like list for the Eikon Data API
#'
#' @param directions - Where
#' @param payload - What
#'
#' @return Nested list that fit into the JSON scheme
#' @noRd
#' @examples
#' directions <- 'DataGrid_StandardAsync'
#' payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
#'      'fields' = lapply(list("TR.RICCode"), \(x) list("name" = x))
#'    )))
#'
#'json_builder(directions, payload)
json_builder <- function(directions, payload) {
  list('Entity' = list('E' = directions, 'W' = payload))

}


#' Send JSON POST request to the given service
#'
#' Sends a POST request to the given service with a json like object
#'
#' @param json - The nested list resembling JSON that should be sent to the server
#' @param service - !!WIP!! The service to send the message to
#' @param debug - Default to FALSE, turns on debugging messages
#' @param request_type character: Json Request type POST or GET
#' @param EndPoint character url endpoint from Refinitiv
#'
#' @return Returns the results from the query
#' @noRd
#'
#' @examples
#'  #data reuqest
#'  directions <- 'DataGrid_StandardAsync'
#'  payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
#'      'fields' = lapply(list("TR.RICCode"), \(x) list("name" = x))
#'    )))
#'
#'  json <- json_builder(directions, payload)
#'  print(json)
#'  send_json_request(json)
#'
#'  #time series request
#'  directions = 'TimeSeries'
#'  # Builds the payload to be sent
#'  payload <- list('rics' = list("AAPL.O"),'fields' = c('TIMESTAMP', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE')
#'                , 'interval' = "daily", 'calender' = "tradingdays", 'corax' = "adjusted"
#'                , 'startdate' = "2020-01-01T01:00:00"
#'                , 'enddate' = "2021-01-01T01:00:00"
#'                )
#'                json <- json_builder(directions, payload)
#'                print(json)
#'                send_json_request(json)
send_json_request <- function(json, service = "eikon", debug = FALSE, request_type = "POST", EndPoint = NULL) {

  # 0. helper functions ----

  # Fetches the url to send data requests to
  Construct_url <- function(service="eikon") {
    if(tolower(service)=="eikon"){
      url <- paste0( getOption("refinitiv_base_url"), ":"
                   , getOption("eikon_port")
                   , getOption("eikon_api")
                   )

      #print(url)
    } else if(tolower(service) == "rdp") {
      url <- paste0( getOption("refinitiv_base_url"), ":"
                     , getOption("rdp_port")
                     , getOption("rdp_api")
                     , EndPoint
                     )

    } else{
      stop(paste0("wrong service selected in function Construct_url, only rdp or eikon allowed but "
                  , service, " is chosen"))
    }
    return(url)
  }

  # 2. main function ----
  counter <- 0
  results <- data.frame()
  while (TRUE & counter < 2){
    if(toupper(request_type) == "POST"){
      query <- httr::POST( url =  Construct_url(service=service)
                            , httr::add_headers(
                               'Content-Type' = 'application/json',
                               'x-tr-applicationid' = getOption(".EikonApiKey"))
                            , body = json
                            , encode = "json"
                            )

    } else if(toupper(request_type) == "GET"){
        query <- httr::GET( Construct_url(service=service)
                          , httr::add_headers(
                                'Content-Type' = 'application/json',
                                'x-tr-applicationid' = getOption(".EikonApiKey"))
                          , encode = "json"
                          , httr::timeout(5)
                          )
    }

    tryresults <- httr::content(query)

    # Fetches the content from the query
    # Checks for ErrorCode and then aborts after printing message
     if (is.numeric(tryresults$ErrorCode)) {

        if (tryresults$ErrorCode %in% c(2504,500,400)) {
          Sys.sleep(5)
          counter <- counter + 1
        } else {
         stop(paste0("Error code: ", tryresults$ErrorCode, " ", tryresults$ErrorMessage))
     }
    } else {
      results <- tryresults
       break
    }
}
  # Returns the results
  results
}




#' Connect to Eikon directly with JSON requests
#'
#' @param Eikonapplication_id character eikon api key
#' @param Eikonapplication_port numeric proxy port 9000 or 9060 for RDP/RD
#'
#' @return RefinitivConnection Object
#' @importFrom rlang env
#' @export
#'
#' @examples
#' \dontrun{
#'  EikonJson <- RefinitivJsonConnect()
#' }
RefinitivJsonConnect <- function(Eikonapplication_id = NA , Eikonapplication_port = 9000L){

  #0. helper functions ----
  jsonlistbuilder <- function(x){
    if (length(x)==1 & !is.list(x)){
      return(list(x))
    } else{
      return(x)
    }
  }


  # 1. check input ----
  if (is.na(Eikonapplication_id)){
    try(Eikonapplication_id <- getOption(".EikonApiKey") )
    if(is.null(Eikonapplication_id)){stop("Please supply Eikonapplication_id")}
  }
  payload <- NULL
  options(.EikonApiKey = Eikonapplication_id)


  JSON_EK <- rlang::env( set_app_key =  function(app_key = Eikonapplication_id){
                                                  options(.EikonApiKey = app_key)}
                       , get_app_key = function(){return(getOption(".EikonApiKey"))}
                       , set_app_port =  function(app_port = Eikonapplication_id){
                                                   options(.EikonApplicationPort = app_port)}
                       , get_app_port =  function(){return(getOption(".EikonApplicationPort"))}
                       , get_timeseries = function(rics, interval, calendar, fields
                                                  , start_date, end_date, corax, normalize, raw_output){

                                           directions = 'TimeSeries'
                                           # Builds the payload to be sent
                                           payload <- NULL
                                           payload <- list('rics' = jsonlistbuilder(rics)
                                                          , 'fields' = jsonlistbuilder(fields)
                                                          , 'interval' = interval
                                                          , 'calender' = calendar
                                                          , 'corax' = corax
                                                          , 'startdate' = start_date
                                                          , 'enddate' = end_date
                                                          )
                                         json <- json_builder(directions, payload)
                                         return(send_json_request(json))}
                        , get_data = function( instruments, fields, parameters = NULL
                                              , debug, raw_output){
                                         payload <- NULL
                                         directions <- 'DataGrid_StandardAsync'
                                         if(!is.null(parameters)){
                                           payload <- list(
                                             'requests' = list(
                                               list(
                                                 'instruments' = jsonlistbuilder(instruments),
                                                 'fields' = lapply(fields, function(x){list("name" = x)}),
                                                 'parameters' = parameters
                                               )
                                             )
                                           )
                                         } else {
                                           payload <- list(
                                             'requests' = list(
                                               list(
                                                 'instruments' = jsonlistbuilder(instruments),
                                                 'fields' = lapply(fields, function(x){ list("name" = x)})
                                               )
                                             )
                                           )
                                         }
                                         #print(payload)
                                         json <- json_builder(directions, payload)
                                         returnvar <- send_json_request(json)
                                         return(returnvar$responses[[1]])
                        }
                       , get_symbology = function( symbol, from_symbol_type
                                                 , to_symbol_type, raw_output
                                                 , debug, best_match){
                         payload <- NULL
                         directions <- 'SymbologySearch'
                         payload <- list( 'symbols'= jsonlistbuilder(symbol)
                                        , 'from'= from_symbol_type
                                        , 'to'= to_symbol_type
                                        , 'bestMatchOnly' = best_match)
                         json <- json_builder(directions, payload)
                         returnvar <- send_json_request(json)

                       }
                       , search = function( query =  NULL, view = "SearchAll"
                                          ,  select = NULL, top = NULL, filter = NULL
                                          ,  boost= NULL, order_by = NULL, group_by = NULL
                                          ,  group_count = NULL, navigators = NULL, features = NULL){
                         EndPoint = "search/v1/"
                         payload <- NULL
                         payload <- list( 'Query'= query
                                        , 'View'= view
                                        , 'Select'=select
                                        , 'Top'= top
                                        , 'Filter' = filter
                                        , 'Boost' = boost
                                        , 'OrderBy' = order_by
                                        , 'GroupBy' = group_by
                                        , 'GroupCount'= group_count
                                        , 'Navigators'= navigators
                                        , 'Features' = features
                                        #, 'Skip'= skip
                                        )
                         payload[sapply(payload, is.null)] <- NULL
                         #json <- json_builder(directions, payload)

                         response <- send_json_request(payload, service = "rdp", EndPoint = EndPoint, request_type = "POST")
                         return_DT <- data.table::rbindlist(response$Hits,fill=TRUE, use.names = T)

                         # Check for lists columns with null inside and fix those
                         ListCols <- names(which(lapply(return_DT, class) == "list"))

                         if(!identical(ListCols, character(0))){
                           NullRemover <- function(x){replaceInList(x, function(y)if(is.null(y) || identical(y,"")) NA else y )}
                           for (i in 1:length(ListCols)){
                             return_DT[[ListCols[i] ]] <- unlist(NullRemover( return_DT[[ListCols[i] ]] ))
                           }


                         }

                         return(return_DT)

                       }
                       , get_search_metadata = function(RDP, searchView){


                         EndPoint <- paste0("search/v1/metadata/views/",searchView)
                         returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint =  EndPoint)

                         return_DT <- data.table::rbindlist(returnvar$Properties, fill = TRUE, use.names = TRUE
                                                            , idcol = "Refinitiv_index")

                         for (i in seq_along(return_DT)) data.table::set(return_DT, i=which(is.na(return_DT[[i]])), j=i, value=FALSE)
                         Properties <-NULL
                         if("Properties" %in% names(return_DT)){
                           return_DT <- return_DT[,Properties := NULL]
                         }

                         return(data.table::setDF(return_DT))

                       }

                       )

  return(JSON_EK)
}

#
# http://localhost:9060/api/rdp/discovery/search/v1/
#   POST
# {'Content-Type': 'application/json'}
# {'Query': 'aapl.o', 'View': <Views.SEARCH_ALL: 'SearchAll'>, 'Top': 10, 'Skip': 0, 'GroupCount': 3}
