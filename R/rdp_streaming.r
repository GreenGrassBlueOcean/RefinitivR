#' Define a streaming prices object for use in the get_snapshot function
#'
#' @param rdpObject refinitiv dataplatform object result from a call to RDPConnect
#' @param universe list of instruments to be subscribed
#' @param fields list of fields to be returned by the stream
#'
#' @return a python stream connection object
#' @seealso rdp_get_snapshot
#' @export
#'
#' @examples
#' \dontrun{
#' rdp <- RDPConnect("your key")
#' streamObject <- rdp_streaming_prices(rdp, universe = c("EUR=","JPY="), fields = c('DSPLY_NAME', 'BID', 'ASK'))
#' EquityStream <- rdp_streaming_prices(rdp, universe = c("FLOW.AS"), fields = c('DSPLY_NAME', 'BID', 'ASK'))
#' }
rdp_streaming_prices <- function(rdpObject, universe = list("EUR=","JPY=", "GBP=", "HKD="), fields=list('DSPLY_NAME', 'BID', 'ASK', 'CF_TIME')){
  StreamPObject <- rdp$StreamingPrices(universe = universe,  fields = fields)
  return(StreamPObject)
}


#' Retrieve a real-time snapshot from a previously defined stream object
#'
#' @param StreamingPricesObject an object as defined by
#'
#' @return a data.table with snapshot prices
#' @export
#'
#' @examples
#' \dontrun{
#' rdp <- RDPConnect("your key")
#' streamObject <- rdp_streaming_prices(rdp, universe = c("EUR=","JPY="), fields = c('DSPLY_NAME', 'BID', 'ASK', 'CF_TIME'))
#' rdp_get_snapshot(streamObject)
#' }
rdp_get_snapshot <- function(StreamingPricesObject){

  #open object
  StreamingPricesObject$open()
  #take snapshot
  py_snapshot <- StreamingPricesObject$get_snapshot()
  # convert to R logic
  r_data.table <- data.table::as.data.table(reticulate::py_to_r(py_snapshot$to_dict()))

  # close connection
  StreamingPricesObject$close()

  #return to R
  return(r_data.table)

}
