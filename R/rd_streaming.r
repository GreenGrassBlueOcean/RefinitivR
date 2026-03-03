#' Check if the LSEG Workspace proxy is alive
#'
#' @param port Proxy port (default 9000).
#' @param debug Logical; if \code{TRUE}, prints request details.
#'
#' @return API status code and version.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' test <- rd_check_proxy_url(port = 9000)
#' }
rd_check_proxy_url <- function(port = 9000L, debug = TRUE) {
  response <- send_json_request(
    request_type = "GET",
    debug = debug,
    apikey = "DEFAULT_WORKSPACE_APP_KEY",
    url = paste0(getOption("refinitiv_base_url"), ":", port, "/api/status")
  )
}


# Old streaming functions removed - see new implementation in:
# - R/rd_streaming_manager.R (StreamManager class)
# - R/rd_streaming_definition.R (StreamDefinition classes)
# - R/rd_streaming_stream.R (Stream class)
# - R/rd_streaming_api.R (High-level API functions)
