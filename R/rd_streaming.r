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
                               , url = paste0(getOption("refinitiv_base_url"), ":", port, "/api/status")
                               )


}





# Old streaming functions removed - see new implementation in:
# - R/rd_streaming_manager.R (StreamManager class)
# - R/rd_streaming_definition.R (StreamDefinition classes)
# - R/rd_streaming_stream.R (Stream class)
# - R/rd_streaming_api.R (High-level API functions)

