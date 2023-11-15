#' Show all available custom instruments that have been created
#'
#' @param RDObject Refinitiv Data connection object, defaults to RefinitivJsonConnect()
#' @param debug show api calls defaults to FALSE
#'
#' @return a list of custom Instruments created with all parameters
#' @export
#'
#' @examples
#' \dontrun{
#' test <- get_rdp_streaming_url()
#' }
get_rdp_streaming_url <- function(RDObject = RefinitivJsonConnect(), debug = TRUE){

  handshake <- rd_handshake()
  Request <- RDObject$get_rdp_streaming_url( debug = debug)
  EndPoint <- "streaming/pricing/v1/"
  payload <- NULL
  response <- send_json_request(payload, service = "rdp"
                                , EndPoint = EndPoint
                                , request_type = "GET"
                                , debug = debug
                                )

  return(Request)

}

#' Get bearer key from terminal
#'
#' @param debug debug parameter print urls for json requests
#'
#' @return list with the following fields \itemize{
#'  \item(access_token)(key)
#'  \item(expires_in)(unknown what this means)
#'  \item(token_type)(type of token (bearer))
#' }
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#'  rd_handshake()
#' }
rd_handshake <- function(debug = FALSE){

  payload <- list( 'AppKey' = 'DEFAULT_WORKSPACE_APP_KEY'
                 , 'AppScope' = 'trapi'
                 , 'ApiVersion'= '1'
                 , 'LibraryName' = 'RDP Python Library'
                 , 'LibraryVersion'= '1.3.1'
                 )


  response <- send_json_request( payload
                               , request_type = "POST"
                               , debug = debug
                               , apikey = 'DEFAULT_WORKSPACE_APP_KEY'
                               , url = "http://localhost:9060/api/handshake"
                               )
  return(response)
}


#' Check if refinitiv proxy url is alive
#'
#' @param port 9060 or 9000
#' @param debug boolean TRUE or False
#'
#' @return api status code and version
#' @export
#'
#' @examples
#' \dontrun{
#' test <- rd_check_proxy_url(port = 9000)
#' test <- rd_check_proxy_url(port = 9060)
#' }
rd_check_proxy_url <- function(port = 9060, debug = TRUE){

  response <- send_json_request( request_type = "GET"
                               , debug = debug
                               , apikey = 'DEFAULT_WORKSPACE_APP_KEY'
                               , url = paste0("http://localhost:", port, "/api/status")
                               )


}





#' Create an OMM stream when an Eikon terminal is running
#'
#' @param name character  RIC to retrieve item stream.
#' @param domain character Specify item stream domain (MarketPrice, MarketByPrice, ...), defaults to MarketPrice
#' @param service Specify the service to subscribe on. NULL
#' @param fields character vector Specify the fields to retrieve.
#' @param extended_params NULL Specify optional params
#'
#' @return OMM stream object
#' @export
#'
#' @import R6
#'
#' @details
#' Note that field names, depend on exchange and on order book label
#'
#' \itemize{
#'  \item(domain = MarketByPrice [level2] )(ORDER_PRC, ORDER_SIDE, ACC_SIZE)
#'  \item(domain = MarketPrice)(BEST_BID1, BEST_BSIZ1, BEST_ASK1, BEST_ASIZ1)
#' }
#'
#'
#'
#' @examples
#'  \dontrun{
#' OMM_ws <- create_OMM_Stream()
#' EUR_stream <- OMM_ws$new(name = "EUR=", fields = c("BID","ASK","OPEN_PRC"))
#' # start stream
#' EUR_stream$connect()
#' # stop stream
#' EUR_stream$close()
#' }
create_OMM_Stream <- function(name = "EUR="
                             #, api
                             , domain = "MarketPrice"
                             , service = NULL
                             , fields = c("BID","ASK","OPEN_PRC")
                             , extended_params = NULL
                             ){

  # check if later and websocket package are availble
  if(!requireNamespace("later", quietly = TRUE)){
    stop("Please install later package")
  }

  if(!requireNamespace("websocket", quietly = TRUE)){
    stop("Please install websocket package")
  }

  # build_json_list_string(c("BID","ASK","OPEN_PRC"))
  build_json_list_string <- function(fieldsvector){
    return(paste0("[\"",paste0(fieldsvector, collapse = "\",\""), "\"]"))
  }

  # Construct headers
  headers <- list( 'User-Agent' = "R"
                 , 'x-tr-applicationid' = 'DEFAULT_WORKSPACE_APP_KEY'
                 , 'Authorization' = paste('Bearer',  rd_handshake()$access_token)
                 )

 # Construct login request
 Create_login_request = function() {
   login_json_string = "{\"ID\":1,\"Domain\":\"Login\",\"Key\":{\"Elements\":{\"AppKey\":\"<APP_Key>\",\"Authorization\":\"<AUTHO>\",\"ApplicationId\":\"<APP_ID>\",\"Position\":\"<POSITION>\"}}}" |>
   gsub(pattern =  "<APP_Key>", replacement = "DEFAULT_WORKSPACE_APP_KEY") |>
   gsub(pattern = "<AUTHO>",  replacement = paste('Bearer', rd_handshake()$access_token)) |>
   gsub(pattern = "<APP_ID>", replacement = 256) |>
   gsub(pattern = "<POSITION>", replacement = paste0('127.0.0.1/',as.character(Sys.info()["nodename"])))
}

 Create_Stream_request <- function(domain, name, fields, snapshot){
   request_json_string <- "{\"ID\":2,\"Domain\":\"<DOMAIN>\",\"Streaming\":<STREAM>,\"Key\":{\"Name\":\"<INSTRUMENT>\"},\"View\":<VIEW>}" |>
    gsub(pattern =  "<DOMAIN>", replacement = domain) |>
    gsub(pattern =  "<INSTRUMENT>", replacement = name) |>
    gsub(pattern =  "<VIEW>", replacement = build_json_list_string(fields)) |>
    gsub(pattern =  "<STREAM>", replacement = tolower(as.character(!snapshot)))
  }



 poll_until_connected <- function(ws, timeout = 5) {
   connected <- FALSE
   end <- Sys.time() + timeout
   while (!connected && Sys.time() < end) {
     # Need to run the event loop for websocket to complete connection.
     later::run_now(0.1)
     ready_state <- ws$readyState()
     if (ready_state == 0L) {
       # 0 means we're still trying to connect.
       # For debugging, indicate how many times we've done this.
       cat(".")
     } else if (ready_state == 1L) {
       connected <- TRUE
     } else {
       break
     }
   }

   if (!connected) {
     stop("Unable to establish websocket connection.")
   }
 }



 self <- private <- NULL
 OMM_Stream <- R6::R6Class( "OMM_Stream"
                          , public = list( name = name
                                         ,  api = NULL
                                         ,  domain = domain
                                         ,  service = service
                                         ,  fields = fields
                                         ,  extended_params = extended_params
                                         ,  initialize = function( name = NULL
                                                                 , api = NULL
                                                                 , domain = NULL
                                                                 , service = NULL
                                                                 , fields = NULL
                                                                 , extended_params = NULL
                                                                 , url = "ws://localhost:9060/api/rdp/streaming/pricing/v1/WebSocket"
                                                                 , protocols = "tr_json2"
                                                                 , headers = list( 'User-Agent' = "R"
                                                                                 , 'x-tr-applicationid' = 'DEFAULT_WORKSPACE_APP_KEY'
                                                                                 , 'Authorization' = paste('Bearer',  rd_handshake()$access_token)
                                                                                 )
                                                                , autoConnect = FALSE
                                                           ){
                       private$.ws <- websocket::WebSocket$new(url = url, protocols = protocols, headers = headers, autoConnect = FALSE)
                       private$.ws$onError(function(event) {
                         cat("Client failed to connect: ", event$message, "\n")
                       })
                       private$.ws$onMessage(function(event){
                         private$.process_message(event)
                       })
                       self$name <- name
                       self$api <- api
                       self$domain <- domain
                       self$service <- service
                       self$fields <- fields
                       self$extended_params <- extended_params
                       private$.fig <- NULL
                     },
                       get_snapshot = function() {
                         private$.snapshot <- TRUE
                         private$.ConnectandAskforstream( domain = domain
                                                        , name = name
                                                        , fields = fields
                                                        )

                     },
                       get_stream = function(){
                          private$.snapshot <- FALSE
                          private$.ConnectandAskforstream( domain = domain
                                                           , name = name
                                                           , fields = fields
                          )
                     },
                      plot_stream = function(xrange = 30,  ylim = c(1.05,1.06), Plotvariable = "Fields.BID"){
                       private$.snapshot <- FALSE
                       private$.fig <- "test"

                        #check if shiny package is available
                        if (!requireNamespace("shiny", quietly = TRUE)) {
                          stop("shiny package is not available")
                        }

                        ui <- shiny::shinyServer(shiny::fluidPage(
                          shiny::plotOutput("first_column")
                        ))

                        private$.ConnectandAskforstream( domain = domain
                                                         , name = name
                                                         , fields = fields
                        )

                        server <- shiny::shinyServer(function(input, output, session){

                          # Plot the 30 most recent values
                          output$first_column <- shiny::renderPlot({
                            shiny::invalidateLater(1000, session)
                            plot( x = 1:xrange
                                , y = private$.StreamLog[1:xrange,][[Plotvariable]]
                                , ylim= c( 0.999 * max(0, min(private$.StreamLog[1:xrange,][[Plotvariable]]), na.rm = T)
                                         , 1.001 * max(0, max(private$.StreamLog[1:xrange,][[Plotvariable]]), na.rm = T))
                                , las=1
                                , type="l"
                                , xlab="ticks"
                                , ylab=Plotvariable
                                )
                          })
                        })

                       shiny::shinyApp(ui=ui,server=server)

                     },
                      close = function(){
                       private$.ws$close()
                     }

                     )
                     , private = list( .snapshot = FALSE
                                     , .MessageCount = 0L
                                     , .ws = NULL
                                     , .fig = NULL
                                     , .StreamLog = data.table::data.table()
                                     , .DynamicYaxisLimits = c(0,0)
                                     , .ConnectandAskforstream = function(domain, name, fields, snapshot) {
                                              private$.ws$connect()
                                              poll_until_connected(ws = private$.ws)
                                              private$.ws$send(Create_login_request())
                                        }
                                     , .process_message = function(event){
                                          private$.MessageCount <- private$.MessageCount + 1
                                          StreamMessage <- jsonlite::fromJSON(event$data) |> data.table::as.data.table()
                                          Type <- NULL
                                          message_type <- StreamMessage[, Type]
                                          if(message_type == "Refresh"){
                                            if('Domain' %in% names(StreamMessage)){
                                                Domain <- NULL
                                                message_domain <- StreamMessage[, Domain]
                                            if(message_domain == "Login"){
                                             cat("in login\n")
                                             #cat(paste(capture.output(print(StreamMessage)), collapse = "\n"))
                                             # ask for data after loging is succesfull
                                             cat("before sending request\n")
                                             private$.ws$send(Create_Stream_request(domain = domain, name = name, fields = fields, snapshot = private$.snapshot))
                                             cat("send data request\n")
                                            }
                                          }
                                       } else if(message_type == "Update"){
                                         private$.StreamLog <- data.table::rbindlist(list(StreamMessage,private$.StreamLog))
                                       } else if(message_type == "Ping"){
                                         private$.ws$send("{\"Type\":\"Pong\"}")
                                       }
                                      if(private$.snapshot && private$.MessageCount > 2L ){
                                        private$.ws$close()
                                      }
                                     }
                                     )


)

return(OMM_Stream)
}





#' Create a RDP stream when an Eikon terminal is running
#'
#' @param service string optional name of RDP service
#' @param universe RIC to retrieve item stream.
#' @param view data fields to retrieve item stream
#' @param parameters extra parameters to retrieve item stream.
#' @param api specific name of RDP streaming defined in config file. 'streaming/trading-analytics/redi'
#' @param extended_parameters defaults to NULL Specify optional params
#'
#' @return RDP Stream object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- create_RDP_Stream()
#' test$connect()
#' test$close()
#' }
create_RDP_Stream <- function(service
                             ,universe
                             ,view
                             ,parameters
                             ,api
                             ,extended_parameters){

  # service: string, optional
  # name of RDP service
  #
  # universe: list
  # RIC to retrieve item stream.
  #
  # view: list
  # data fields to retrieve item stream
  #
  # parameters: dict
  # extra parameters to retrieve item stream.
  #
  # api: string
  # specific name of RDP streaming defined in config file. i.e.
  # 'streaming/trading-analytics/redi'
  #
  # extended_params: dict, optional
  # Specify optional params
  # Default: None
  #



  handshake <- rd_handshake()

  # Construct headers
  headers <- list( 'User-Agent' = "R"
                   , 'x-tr-applicationid' = 'DEFAULT_WORKSPACE_APP_KEY'
                   , 'Authorization' = paste('Bearer', handshake$access_token)
  )


  #construct websocket
  ws <- websocket::WebSocket$new( url =  "ws://localhost:9060/api/rdp/streaming/quantitative-analytics/beta1/financial-contracts/WebSocket"
                                  , protocols = "rdp_streaming"
                                  , headers = headers
                                  , autoConnect = F
  )

  Create_login_request = function() {
    login_json_string = "{\"method\": \"<METHOD>\", \"streamID\": \"<STREAMID>\", \"appKey\": \"<APPKEY>\", \"authorization\": \"<AUTHO>\"}" |>
      gsub(pattern = "<METHOD>", replacement = "Auth") |>
      gsub(pattern = "<APPKEY>", replacement = "DEFAULT_WORKSPACE_APP_KEY") |>
      gsub(pattern = "<AUTHO>",  replacement = paste('Bearer', handshake$access_token)) |>
      gsub(pattern = "<STREAMID>", replacement = "2")
  }


  Create_Stream_request <- function(universe,view,parameters,extended_parameters){
  return(paste0("{\"streamID\": \"5\" , \"method\": \"Subscribe\""
               ,", \"universe\": {\"instrumentType\": \"FxCross\""
               ,",\"instrumentDefinition\": {\"instrumentTag\": \"USDAUD\", \"fxCrossType\": \"FxSpot\", \"fxCrossCode\": \"USDAUD\"}}"
               ,", \"view\": [\"InstrumentTag\", \"FxSpot_BidMidAsk\", \"ErrorCode\", \"Ccy1SpotDate\", \"Ccy2SpotDate\"]"
               ,", \"marketData\": {\"fxSpots\": [{\"spotDefinition\": {\"fxCrossCode\": \"AUDUSD\", \"Source\": \"Composite\"}}]}}"
               ))
  }




  ws$onMessage(function(event) {
    d <- event$data
    cat(" Message ", d, "\n")  })
  ws$onOpen(function(event) {
    ws$send(Create_login_request())
    Sys.sleep(4)
    ws$send(Create_Stream_request())
  })
  ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })

  #



return(ws)



}

