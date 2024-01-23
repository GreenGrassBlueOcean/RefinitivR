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
#
#
# 'layout' = list("columns" = list(list("item" = "dataitem")), "rows" = list(list('item'= 'instrument'), list('item'= 'date')) )
#
#
# 'layout': {'columns': [{'item': 'dataitem'}], 'rows': [{'item': 'instrument'}, {'item': 'date'}]}}]}}


#' Escape characters for use in json strings
#'
#' @param x character with length == 1
#'
#' @return escaped character
#'
#' @noRd
#' @seealso [jsonDataGridConstructor()]
#'
#' @examples
#' Escaper("a")
Escaper <- function(x){
  if(length(x) !=1){
    stop("function escaper can only be used for single length character or list")
  }

  if(is.list(x)){
    return(paste0("\"",x[[1]],"\""))
  } else{
    return(paste0("\"",x,"\""))
  }



}



#' Takes a list with elements and converts them to a json string
#'
#' @param payload a list for constructing a json string from
#'
#' @return character json string
#' @noRd
#' @examples
#' payload <- list(universe = "AAPL.O", fields = c("BID", "ASK"))
#' payload <- list(universe = "AAPL.O", fields = c("TR.Revenue", "TR.GrossProfit")
#' , parameters = list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01"))
#' payload <- list(universe = "AAPL.O", fields = c("TR.Revenue", "TR.GrossProfit") , parameters = list())
#' jsonDataGridConstructor(payload)
jsonDataGridConstructor <- function(payload){

  if( "parameters" %in% names(payload)){
    if(length(payload[["parameters"]]) == 0 ){
      payload[["parameters"]] <- NULL
    }
  }


  body <- paste("{", paste(lapply( X = 1: length(payload)
                                   , function(x, payload){
                                                          if(names(payload)[x] != "parameters"  ){

                                                           return(paste(Escaper(names(payload)[x]), ": [", paste(lapply(X = payload[[x]], Escaper), collapse = ",") ,"]"))
                                                          } else {
                                                              parameterlist <- lapply(X = 1:length(payload[x][[1]])
                                                                                     , FUN = function(x, parameters){paste(Escaper(names(parameters)[x]), ":" , Escaper(parameters[x]))}
                                                                                     , parameters = payload[x][[1]])
                                                              parameterstring <- paste(Escaper(names(payload)[x]),  ": {", paste(parameterlist, collapse = " , "), "}")
                                                              return(parameterstring)
                                                          }
                                     }
                                   , payload = payload)
                           , collapse = " , ") , "}" )

  return(body)
}



#' Constructs the url to send data requests to
#'
#' @param service "eikon" or "rdp"
#' @param EndPoint for RDP only
#'
#' @return url (string)
#' @noRd
#'
#' @examples
#' Construct_url(service = "eikon")
#' Construct_url(service = "rdp", EndPoint = "discovery/search/v1/")
Construct_url <- function(service="eikon", EndPoint = NULL) {
  if(tolower(service) %in% c("eikon", "udf")){
    url <- paste0( getOption("refinitiv_base_url"), ":"
                   , getOption("eikon_port")
                   , getOption("eikon_api")

    )

    "http://localhost:9060/api/udf"

    #print(url)
  } else if(tolower(service) == "rdp") {
    url <- paste0( getOption("refinitiv_base_url"), ":"
                   , getOption("rdp_port")
                   , getOption("rdp_api")
                   , EndPoint
    )

  # } else if(tolower(service) == "udf"){
  #   url <- paste0( getOption("refinitiv_base_url"), ":"
  #                  , getOption("rdp_port")
  #                  , "/api/udf/"
  #                  , EndPoint
  #   )
  } else{
    stop(paste0("wrong service selected in function Construct_url, only rdp, udf or eikon allowed but "
                , service, " is chosen"))
  }
  # print(url)
  return(url)
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
#' @param url character url to overwrite default url, not required only for testing
#'
#' @return Returns the results from the query
#' @noRd
#'
#' @examples
#'  #data reuqest
#'  directions <- 'DataGrid_StandardAsync'
#'  payload <- list('requests' = list(list('instruments' = list("TSLA.O"),
#'      'fields' = lapply(list("TR.RICCode"), function(x) list("name" = x))
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
send_json_request <- function(json=NULL, service = "eikon", debug = TRUE, request_type = "POST", EndPoint = NULL, url = NULL, apikey = getOption(".EikonApiKey")) {

  # 0. url manipulation ----
  if(is.null(url)){
    Request_url <- Construct_url(service=service, EndPoint = EndPoint)
  } else {
    Request_url <- url
  }


  if(debug){
  message(Request_url)
    if(!is.null(json)){
      message(json)
    }
  }

  # 2. post or het request ----
  counter <- 0
  results <- NA

  while (TRUE & counter < 3){
    if(toupper(request_type) == "POST"){
      query <- httr2::request(base_url = Request_url) |>
        httr2::req_headers('x-tr-applicationid' = apikey) |>
        httr2::req_headers('Content-Type' = 'application/json') |>
        httr2::req_user_agent("RefinitivR (https://github.com/GreenGrassBlueOcean/RefinitivR)") |>
        #httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_timeout(60)
        httr2::req_body_json(json)|>
        httr2::req_perform()

    } else if(toupper(request_type) == "GET"){

        query <- httr2::request(base_url = Request_url) |>
          httr2::req_headers('x-tr-applicationid' = apikey) |>
          httr2::req_headers('Content-Type' = 'application/json') |>
          #httr2::req_headers('charset' = 'ISO-8859-1') |>
          #httr2::req_headers('Accept-Encoding' = "gzip") |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_user_agent("RefinitivR (https://github.com/GreenGrassBlueOcean/RefinitivR)") |>
          httr2::req_timeout(5) |>
          httr2::req_perform()

    } else if(toupper(request_type) == "DELETE"){

      query <- httr2::request(base_url = Request_url) |>
        httr2::req_headers('x-tr-applicationid' = apikey) |>
        httr2::req_headers('Content-Type' = 'application/json') |>
        #httr2::req_headers('charset' = 'ISO-8859-1') |>
        #httr2::req_headers('Accept-Encoding' = "gzip") |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_method("DELETE") |>
        httr2::req_user_agent("RefinitivR (https://github.com/GreenGrassBlueOcean/RefinitivR)") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()

    } else if(toupper(request_type) == "PUT"){

      query <- httr2::request(base_url = Request_url) |>
        httr2::req_headers('x-tr-applicationid' = apikey) |>
        httr2::req_headers('Content-Type' = 'application/json') |>
        #httr2::req_headers('charset' = 'ISO-8859-1') |>
        #httr2::req_headers('Accept-Encoding' = "gzip") |>
        httr2::req_body_json(json)|>
        httr2::req_method("PUT")|>
        httr2::req_user_agent("RefinitivR (https://github.com/GreenGrassBlueOcean/RefinitivR)") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()

    }


    if(debug & request_type != "DELETE"){
      message(query$status_code)
    }

    if(request_type != "DELETE"){
    tryresults <-  httr2::resp_body_json(query, check_type = FALSE)
    if("responses" %in% names(tryresults)){
       tryresults <- tryresults$responses[[1]]
    }

    ticket <- NULL
    # Fetches the content from the query
    # Checks for ErrorCode and then aborts after printing message
    if (is.numeric(tryresults$ErrorCode) || is.numeric(tryresults$estimatedDuration)) {
        if (is.numeric(tryresults$ErrorCode) && tryresults$ErrorCode %in% c(2504,500,400)) {
          print(tryresults$ErrorCode)
          Sys.sleep(5)
          counter <- counter + 1
        } else if(is.numeric(tryresults$estimatedDuration)){
          WaitTime <- NULL
          WaitTime <- try(tryresults$estimatedDuration/1000, silent = TRUE)
          ticket <- try(tryresults$ticket, silent = TRUE)
          message(paste("request not ready, server is asking to wait for",WaitTime, "seconds so waiting patiently"))
          if(!is.null(WaitTime) && WaitTime <= 60){
            Sys.sleep(WaitTime)
          } else {
            Sys.sleep(60)
          }# maximize waiting time to 60 seconds, not waiting for more than 60 seconds for server response
          # Check if new json body with ticket should be created for the next request
          if(!is.null(ticket) && !is.null(query$request$body)){
            json <- ConstructTicketJsonBody(ticket = ticket, query = query, debug = debug)
            counter <- counter + 0.5
          } else {
            counter <- counter + 1
          }

        } else {
         stop(paste0("Error code: ", tryresults$ErrorCode, " ", tryresults$ErrorMessage))
     }
    } else {
      results <- tryresults
       break
    }
    } else { break
             results <- NA}
}
  results
}


#' rewrite JSON body in case a waiting ticket is assigned so that the correct json is requested
#'
#' @param query httr2_response
#' @param ticket character hash code
#' @param debug boolean print revised json message with ticket number
#'
#' @return revised json body with ticket hash
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  see tests for examples
#' }
ConstructTicketJsonBody <- function(query, ticket, debug){

  body <- query$request$body

  if("Entity" %in% names(body$data)){
    body$data$Entity$W$requests[[1]] <- list("ticket" = ticket)
  } else {
    body$data <- list("ticket" = ticket)
  }

  if(debug){
    message(body$data)
   }

  return(body$data)
}



#' return element as a list even it is not a list
#'
#' @param x anything
#' @param ElementNames character with length 1
#'
#' @return list
#'
#' @seealso [RefinitivJsonConnect()]
#' @noRd
#'
#' @examples
#' JsonListBuilder(x = "a")
#' JsonListBuilder(x = list("a"))
JsonListBuilder <- function(x){
  if (length(x)==1 & !is.list(x)){
    ReturnList <- list(x)
  } else{
    ReturnList <- x
  }

  return(ReturnList)
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
                                           payload <- list( 'rics' = JsonListBuilder(rics)
                                                          , 'fields' = JsonListBuilder(fields)
                                                          , 'interval' = interval
                                                          , 'calender' = calendar
                                                          , 'corax' = corax
                                                          , 'startdate' = start_date
                                                          , 'enddate' = end_date
                                                          )
                                         json <- json_builder(directions, payload)
                                         return(send_json_request(json))}
                        , get_data = function( instruments, fields, parameters = NULL, SyncFields = FALSE
                                              , debug, raw_output){
                                         payload <- NULL
                                         directions <- 'DataGrid_StandardAsync'
                                         # if(!is.null(parameters)){
                                         #   payload <- list(
                                         #     'requests' = list(
                                         #       list(
                                         #         'instruments' = JsonListBuilder(instruments),
                                         #         'fields' = lapply(fields, function(x){list("name" = x)}),
                                         #         'parameters' = parameters,
                                         #         'layout' = list( "columns" = list(list("item" = "dataitem"))
                                         #                        , "rows" = list(list('item'= 'instrument'), list('item'= 'date'))
                                         #                        )
                                         #       )
                                         #     )
                                         #   )
                                         # } else {
                                         #   payload <- list(
                                         #     'requests' = list(
                                         #       list(
                                         #         'instruments' = JsonListBuilder(instruments),
                                         #         'fields' = lapply(fields, function(x){ list("name" = x)}),
                                         #         'layout' = list( "columns" = list(list("item" = "dataitem"))
                                         #                          , "rows" = list(list('item'= 'instrument'), list('item'= 'date'))
                                         #         )
                                         #       )
                                         #     )
                                         #   )
                                         # }

                                       requests <- list(
                                                 'instruments' = JsonListBuilder(instruments),
                                                 'fields' = lapply(fields, function(x){list("name" = x)}),
                                                 'parameters' = parameters,
                                                 'layout' = if(SyncFields){
                                                                list( "columns" = list(list("item" = "dataitem"))
                                                                    , "rows" = list(list('item'= 'instrument'), list('item'= 'date'))
                                                                    )
                                                                    } else {NULL}

                                               )


                                       requests[sapply(requests, is.null)] <- NULL
                                       payload <- list('requests' = list(requests))

                                       #print(payload)
                                       json <- json_builder(directions, payload)
                                       returnvar <- send_json_request(json)
                                       return(returnvar)
                        }
                       , get_data_rdp = function(universe, fields, parameters = NULL
                                                 ,output = NULL , debug, raw_output){

                         EndPoint = "data/datagrid/beta1/"
                         payload <- NULL

                         if(length(parameters) == 0 ){
                             parameters <- NULL
                        }
                        payload <- list( 'universe'= JsonListBuilder(universe)
                                        , 'fields'= JsonListBuilder(fields)
                                        , 'parameters'=parameters
                                        , 'output'=  output
                                        )
                         payload[sapply(payload, is.null)] <- NULL


                         response <- send_json_request(payload, service = "rdp"
                                                      , EndPoint = EndPoint
                                                      , request_type = "POST")


                         return(response)

                       }
                       , get_symbology = function( symbol, from_symbol_type
                                                 , to_symbol_type, raw_output
                                                 , debug, best_match){
                         payload <- NULL
                         directions <- 'SymbologySearch'
                         payload <- list( 'symbols'= JsonListBuilder(symbol)
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
                         EndPoint = "discovery/search/v1/"
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
                         response$Hits <- lapply(response$Hits, FUN = function(x){as.list(unlist(x, recursive = FALSE))})
                         return_DT <- data.table::rbindlist(response$Hits,fill=TRUE, use.names = TRUE)
                         #
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
                       , get_search_metadata = function(RDP=NULL, searchView){


                         EndPoint <- paste0("discovery/search/v1/metadata/views/",searchView)
                         returnvar <- send_json_request(json=NULL, service = "rdp", request_type = "GET", EndPoint =  EndPoint)

                         return_DT <- data.table::rbindlist(returnvar$Properties, fill = TRUE, use.names = TRUE
                                                            , idcol = "Refinitiv_index")

                         for (i in seq_along(return_DT)) data.table::set(return_DT, i=which(is.na(return_DT[[i]])), j=i, value=FALSE)

                         Properties <-NULL
                         if("Properties" %in% names(return_DT)){
                           return_DT <- return_DT[,Properties := NULL]
                         }

                         return(data.table::setDF(return_DT))

                       }
                       , get_historical_pricing = function( EikonObject, universe
                                                           , interval, start, end
                                                           , adjustments, count, fields
                                                           , sessions
                                                           , debug = FALSE){
                         # construct endpoint ----
                         payload <- list('universe' = paste(universe, collapse = ",")
                                         , 'interval' = interval
                                         , 'start' = start
                                         , 'end' = end
                                         , 'adjustments' = if(is.null(adjustments)){NULL}else{paste(adjustments, collapse = ",")}
                                         , 'count' = if(is.null(count)){NULL}else{as.integer(count)}
                                         , 'fields' = if(is.null(fields)){NULL}else{paste(fields, collapse = ",")}
                                         , 'sessions' = if(is.null(sessions)){NULL}else{paste(sessions, collapse = ",")}
                                         )
                         payload <- payload[!unlist(lapply(payload, is.null))]

                        universeIndex <-  grep(x =  names(payload), pattern = "universe")

                        NonUniverseParameters <- paste0(paste0(names(payload[-universeIndex]),"=",payload[-universeIndex]), collapse = "&")

                        AllParameters <- paste0(payload[[universeIndex]], "?", NonUniverseParameters)


                        EndPoint <- paste0("data/historical-pricing/beta1/views/summaries/")
                        EndPoint <- paste0(EndPoint, AllParameters)

                        # Execute request ----

                        returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint =  EndPoint, debug = debug)



                         return(returnvar)

                       }, get_intraday_custominstrument_pricing = function( EikonObject, universe
                                                            , interval, start, end
                                                            , adjustments, count, fields
                                                            , sessions
                                                            , debug = FALSE){
                         # construct endpoint ----
                         payload <- list('universe' = paste(universe, collapse = ",")
                                         , 'interval' = interval
                                         , 'start' = start
                                         , 'end' = end
                                         , 'adjustments' = if(is.null(adjustments)){NULL}else{paste(adjustments, collapse = ",")}
                                         , 'count' = as.integer(count)
                                         , 'fields' = if(is.null(fields)){NULL}else{paste(fields, collapse = ",")}
                                         , 'sessions' = if(is.null(sessions)){NULL}else{paste(sessions, collapse = ",")}
                                         )
                         payload <- payload[!unlist(lapply(payload, is.null))]

                         universeIndex <-  grep(x =  names(payload), pattern = "universe")

                         NonUniverseParameters <- paste0(paste0(names(payload[-universeIndex]),"=",payload[-universeIndex]), collapse = "&")

                         AllParameters <- paste0(payload[[universeIndex]], "?", NonUniverseParameters)


                         EndPoint <- paste0("data/custom-instruments/v1/intraday-summaries/")
                         EndPoint <- paste0(EndPoint, AllParameters)

                         # Execute request ----

                         returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint =  EndPoint, debug = debug)



                         return(returnvar)

                       }, get_interday_custominstrument_pricing = function( EikonObject, universe
                                                                            , interval, start, end
                                                                            , adjustments, count, fields
                                                                            , sessions
                                                                            , debug = FALSE){
                         # construct endpoint ----
                         payload <- list('universe' = paste(universe, collapse = ",")
                                        , 'interval' = interval
                                        , 'start' = start
                                        , 'end' = end
                                        , 'adjustments' = if(is.null(adjustments)){NULL}else{paste(adjustments, collapse = ",")}
                                        , 'count' = as.integer(count)
                                        , 'fields' = if(is.null(fields)){NULL}else{paste(fields, collapse = ",")}
                                        , 'sessions' = if(is.null(sessions)){NULL}else{paste(sessions, collapse = ",")}
                                        )
                         payload <- payload[!unlist(lapply(payload, is.null))]

                         universeIndex <-  grep(x =  names(payload), pattern = "universe")

                         NonUniverseParameters <- paste0(paste0(names(payload[-universeIndex]),"=",payload[-universeIndex]), collapse = "&")

                         AllParameters <- paste0(payload[[universeIndex]], "?", NonUniverseParameters)


                         EndPoint <- paste0("data/custom-instruments/v1/interday-summaries/")
                         EndPoint <- paste0(EndPoint, AllParameters)

                         # Execute request ----

                         returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint =  EndPoint, debug = debug)



                         return(returnvar)

                       }, get_rdp_streaming_url = function(debug = FALSE){


                         EndPoint <- "streaming/pricing/v1/"

                         payload <- NULL


                         response <- send_json_request(payload, service = "rdp"
                                                       , EndPoint = EndPoint
                                                       , request_type = "GET"
                                                       , debug = debug
                                                       #, url = "http://localhost:9000/api/rdp/data/custom-instruments/v1/instruments"
                         )

                       }, create_custom_instrument = function( symbol = NULL
                                                             , formula = NULL
                                                             , type = NULL
                                                             , basket = NULL
                                                             , udc = NULL
                                                             , currency = NULL
                                                             , instrumentName = NULL
                                                             , exchangeName = NULL
                                                             , holidays = NULL
                                                             , timeZone = NULL
                                                             , description = NULL
                                                             , debug = FALSE
                                                             ){



                         EndPoint <- "data/custom-instruments/v1/instruments"

                         payload <- list( "symbol" = symbol
                                        , "formula" = formula
                                        , "type" =  type      # formula, basket, udc
                                        , "basket" = basket
                                        , "udc" = udc
                                        , "currency" = currency
                                        , "instrumentName" = instrumentName
                                        , "exchangeName" = exchangeName
                                        , "holidays" = holidays
                                        , "timeZone" = timeZone
                                        , "description" = description
                                        )

                         payload[sapply(payload, is.null)] <- NULL


                         response <- send_json_request(payload, service = "rdp"
                                                       , EndPoint = EndPoint
                                                       , request_type = "POST"
                                                       , debug = debug
                                                       #, url = "http://localhost:9000/api/rdp/data/custom-instruments/v1/instruments"
                                                       )

                         }, search_custom_instrument = function(debug = FALSE){


                           EndPoint <- "data/custom-instruments/v1/search"

                           payload <- NULL


                           response <- send_json_request(payload, service = "rdp"
                                                         , EndPoint = EndPoint
                                                         , request_type = "GET"
                                                         , debug = debug
                                                         #, url = "http://localhost:9000/api/rdp/data/custom-instruments/v1/instruments"
                           )

                         }, manage_custom_instrument = function( symbol =  NULL  #custom symbol!
                                                             , Id = NULL
                                                             , operation = c("GET", "UPDATE", "DELETE")
                                                             , type = NULL
                                                             , formula = NULL
                                                             , basket = NULL
                                                             , udc = NULL
                                                             , currency = NULL
                                                             , instrumentName = NULL
                                                             , exchangeName = NULL
                                                             , holidays = NULL
                                                             , timeZone = NULL
                                                             , description = NULL
                                                             , UUID = getOption(".RefinitivUUID")
                                                             , debug = FALSE){

                         if(!is.null(symbol) & !is.null(Id) ){
                           stop("supply either symbol or Id to get_customInstrument")
                         }


                        if( !(length(operation) == 1L && toupper(operation) %in% c("GET", "UPDATE", "DELETE"))){
                          stop("parameter operation should be length 1 and be either GET, UPDATE or DELETE")
                        }

                         payload <- NULL
                         EndPoint <- paste0("data/custom-instruments/v1/instruments/")
                         if(!is.null(symbol)){
                           EndPoint <- paste0(EndPoint, symbol)
                         } else {
                           EndPoint <- paste0(EndPoint, Id)
                         }


                        if(identical(toupper(operation), "UPDATE")){
                          operation <- "PUT"
                          Arglist <- as.list(match.call(expand.dots=FALSE))
                          Arglist[[1]] <- Arglist[["UUID"]] <- Arglist[["debug"]] <-
                            Arglist[["operation"]] <- NULL
                          Arglist <- lapply(X = Arglist, FUN = function(x){eval(x, envir=sys.frame(-3))})
                          # perform get request to obtain missing data for put payload message
                          GET_data <- send_json_request( payload, service = "rdp"
                                                       , request_type = "GET"
                                                       , EndPoint =  EndPoint
                                                       , debug = debug)

                          Update_payload <- Arglist
                          Update_payload[sapply(Update_payload, is.null)] <- NULL

                          NotChangingPayload <- GET_data[names(GET_data) %in% setdiff(names(GET_data), names(Update_payload))]
                          payload <- c(NotChangingPayload, Update_payload)


                        }

                        # Execute request ----
                        returnvar <- send_json_request( payload, service = "rdp"
                                                      , request_type = operation
                                                      , EndPoint =  EndPoint
                                                      , debug = TRUE)




                       }, get_news_headlines = function(query = NULL, count = 20L
                                             , repository = NULL
                                             , date_from = NULL
                                             , date_to = NULL
                                             , raw_output = FALSE
                                             , debug = FALSE){                       #

                         EndPoint <- paste0("News_Headlines")

                         payload = list( "number" = as.character(count)
                                       , "query" = query
                                       , "repository" = paste0(repository, collapse = ",")
                                       , "productName" = getOption(".EikonApiKey")
                                       , "attributionCode" = ""
                                       , "dateFrom" = date_from
                                       , "dateTo" = date_to
                                       )
                         payload[sapply(payload, is.null)] <- NULL

                         json <- json_builder(directions = EndPoint,payload )

                         returnvar <- send_json_request( json, service = "udf"
                                                         , request_type = "POST"
                                                         , EndPoint =  NULL
                                                         , debug = debug
                         )



                       }, get_news_story = function(story_id = NULL, raw_output = FALSE, debug=FALSE){

                         EndPoint <- paste0("News_Story")

                         payload = list( "storyId" = story_id
                                       , "attributionCode" = ""
                                       , "productName" = getOption(".EikonApiKey")
                                       )


                         payload[sapply(payload, is.null)] <- NULL

                         json <- json_builder(directions = EndPoint,payload )

                         returnvar <- send_json_request( json, service = "udf"
                                                         , request_type = "POST"
                                                         , EndPoint =  NULL
                                                         , debug = debug
                         )




                       }


                       #
                       # "news": {
                       #   "url": "/data/news/v1",
                       #   "underlying-platform": "udf",
                       #   "endpoints": {
                       #     "headlines": "/headlines",
                       #     "stories": "/stories",
                       #     "top-news": "/top-news",
                       #     "images": "/images",
                       #     "online-reports": "/online-reports",
                       #   },

                       # ek.get_news_headlines('IBM.N', count=100)
                       #
                       # def get_news_headlines(
                       #   query=None,
                       #   count=10,
                       #   repository="NewsWire",
                       #   date_from=None,
                       #   date_to=None,
                       #   raw_output=False,
                       #   debug=False,
                       # ):

                       # Request to http://localhost:9060/api/udf :{'Entity': {'E': 'News_Headlines', 'W': {'number': '10', 'query': 'NVDA.O', 'repository': 'NewsWire', 'productName': '268cffc4666a45ff95c0c08ef56a03682a3f1a5c', 'attributionCode': ''}}, 'ID': '123'}











                       # , get_history = function(RDP, universe=NULL, fields=NULL, parameters=NULL,interval=NULL, start=NULL, end=NULL, adjustments=NULL, count = NULL
                       #                          , use_field_names_in_headers = NULL){
                       #   if(is.null(use_field_names_in_headers)){
                       #     use_field_names_in_headers <- FALSE
                       #   }
                       #
                       #   payload <- list( 'universe' = universe
                       #                  , 'fields'= fields
                       #                  , 'parameters'= parameters
                       #                  , 'interval' = interval
                       #                  , 'start'= start
                       #                  , 'end' = end
                       #                  , 'adjustments' = adjustments
                       #                  , 'count' = count
                       #                  )
                       #
                       #   payload[sapply(payload, is.null)] <- NULL
                       #
                       #   EndPoint <- "data/datagrid/beta1/"
                       #   response <- send_json_request(jsonDataGridConstructor(payload), service = "rdp", request_type = "POST", EndPoint =  EndPoint)
                       #
                       #   Data_DT <- data.table::rbindlist(response$data)
                       #
                       #   if(use_field_names_in_headers){
                       #     colnames <- unlist(lapply(response$headers, FUN = function(x)(x[["name"]])))
                       #    } else {
                       #      colnames <- unlist(lapply(response$headers, FUN = function(x)(x[["title"]])))
                       #    }
                       #
                       #     data.table::setnames(x = Data_DT, new = colnames)
                       #
                       #     return(Data_DT)
                       #
                       #
                       #
                       #
                       # }


                       )

  return(JSON_EK)
}

