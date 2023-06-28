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
#' jsonDataGridConstructor(payload)
jsonDataGridConstructor <- function(payload){
  #
  # lapply(list("TR.RICCode"), \(x) list("name" = x))
  #
  # json <- "{ \"universe\": [ \"TRI.N\", \"IBM.N\" ], \"fields\": [ \"TR.Revenue\", \"TR.GrossProfit\" ] }"
  Escaper <- function(x){
    if(length(x) !=1 && !is.character(x)){
      stop("function escaper can only be used for single character")
    }

    return(paste0("\"",x,"\""))
  }

  body <- paste("{", paste(lapply( X = 1: length(payload)
                                   , function(x, payload){if(names(payload)[x] != "parameters"){
                                                           return(paste(Escaper(names(payload)[x]), ": [", paste(lapply(X = payload[[x]], Escaper), collapse = ",") ,"]"))
                                                          } else {
                                                            parameterlist <- lapply(X = 1:length(payload[x][[1]])
                                                                                  , FUN = function(x, parameters){paste(Escaper(names(parameters)[x]), ":" , Escaper(parameters[x]))}
                                                                                  , parameters = payload[x][[1]])
                                                            parameterstring <- paste(Escaper(names(payload)[x]),  ": {", paste(parameterlist, collapse = " , "), "}")
                                                            return(parameterstring)

                                     }}
                                   , payload = payload) , collapse = " , ") , "}" )

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
send_json_request <- function(json, service = "eikon", debug = FALSE, request_type = "POST", EndPoint = NULL, url = NULL) {

  # 0. helper functions ----
  if(is.null(url)){
    url <- Construct_url(service=service, EndPoint = EndPoint)
  }

  # 2. main function ----
  counter <- 0
  results <- data.frame()
  while (TRUE & counter < 2){
    if(toupper(request_type) == "POST"){
      query <- httr::POST( url =  url
                            , httr::add_headers(
                               'Content-Type' = 'application/json',
                               'x-tr-applicationid' = getOption(".EikonApiKey"))
                            , body = json
                            , encode = "json"
                            )

    } else if(toupper(request_type) == "GET"){
        query <- httr::GET( url = url
                          , httr::add_headers(
                                'Content-Type' = 'application/json',
                                'x-tr-applicationid' = getOption(".EikonApiKey"))
                          , encode = "json"
                          , httr::timeout(5)
                          )
    }

    tryresults <- httr::content(query)
    if("responses" %in% names(tryresults)){
       tryresults <- tryresults$responses[[1]]
    }

    # Fetches the content from the query
    # Checks for ErrorCode and then aborts after printing message
    if (is.numeric(tryresults$ErrorCode) || is.numeric(tryresults$estimatedDuration)) {
        if (is.numeric(tryresults$ErrorCode) && tryresults$ErrorCode %in% c(2504,500,400)) {
          print(tryresults$ErrorCode)
          Sys.sleep(5)
          counter <- counter + 1
        } else if(is.numeric(tryresults$estimatedDuration)){
          WaitTime <- tryresults$estimatedDuration/1000
          print(paste("request not ready, server is asking to wait for",WaitTime, "seconds so waiting patiently"))
          Sys.sleep(WaitTime)
          counter <- counter + 1
        } else {
         stop(paste0("Error code: ", tryresults$ErrorCode, " ", tryresults$ErrorMessage))
     }
    } else {
      results <- tryresults
       break
    }
}

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
                                         return(returnvar)
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


                         EndPoint <- paste0("discovery/search/v1/metadata/views/",searchView)
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
                       , get_history = function(RDP, universe=NULL, fields=NULL, parameters=NULL,interval=NULL, start=NULL, end=NULL, adjustments=NULL, count = NULL
                                                , use_field_names_in_headers = NULL){
                         if(is.null(use_field_names_in_headers)){
                           use_field_names_in_headers <- FALSE
                         }

                         payload <- list( 'universe' = universe
                                        , 'fields'= fields
                                        , 'parameters'= parameters
                                        , 'interval' = interval
                                        , 'start'= start
                                        , 'end' = end
                                        , 'adjustments' = adjustments
                                        , 'count' = count
                                        )

                         payload[sapply(payload, is.null)] <- NULL

                         EndPoint <- "data/datagrid/beta1/"
                         response <- send_json_request(jsonDataGridConstructor(payload), service = "rdp", request_type = "POST", EndPoint =  EndPoint)

                         Data_DT <- data.table::rbindlist(response$data)

                         if(use_field_names_in_headers){
                           colnames <- unlist(lapply(response$headers, FUN = function(x)(x[["name"]])))
                          } else {
                            colnames <- unlist(lapply(response$headers, FUN = function(x)(x[["title"]])))
                          }

                           data.table::setnames(x = Data_DT, new = colnames)

                           return(Data_DT)




                       }


                       )

  return(JSON_EK)
}

