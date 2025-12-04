#' Get streaming data from Refinitiv
#'
#' @description
#' High-level function to create and configure a streaming data connection.
#' This function provides a simple interface for real-time market data streaming
#' with event-driven callbacks.
#'
#' @param universe Character vector of instrument RICs to stream
#' @param fields Character vector of field names to retrieve
#' @param on_refresh Optional function to call on refresh events. 
#'   Function signature: `function(stream, instrument, fields)`
#' @param on_update Optional function to call on update events.
#'   Function signature: `function(stream, instrument, fields)`
#' @param on_error Optional function to call on error events.
#'   Function signature: `function(stream, error_message)`
#' @param RDObject Optional RD connection object (uses active session if not provided)
#' @param stream_type Type of stream ("pricing" or "analytics", default: "pricing")
#' @param domain Domain for OMM streams (default: "MarketPrice")
#' @param parameters Optional parameters list
#'
#' @return Stream object with methods:
#'   - `open()` - Start streaming
#'   - `close()` - Stop streaming
#'   - `on_refresh(callback)` - Register refresh callback
#'   - `on_update(callback)` - Register update callback
#'   - `on_error(callback)` - Register error callback
#'   - `get_latest_data(instrument)` - Get current snapshot
#'   - `get_data_history()` - Get buffered historical data
#'   - `plot_live(field, instrument, ...)` - Create live Shiny plot
#'   - `get_summary(instrument)` - Get summary statistics
#'   - `clear_history()` - Clear data history buffer
#'   - `is_open()` - Check if stream is open
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple usage with callbacks
#' stream <- rd_get_streaming_data(
#'   universe = c("EUR=", "GBP=", "JPY="),
#'   fields = c("BID", "ASK", "DSPLY_NAME"),
#'   on_update = function(stream, instrument, fields) {
#'     cat("Update for", instrument, ":\n")
#'     print(fields)
#'   }
#' )
#' 
#' # Start streaming
#' stream$open()
#' 
#' # Later: stop streaming
#' stream$close()
#' }
#'
#' \dontrun{
#' # With session integration
#' RD <- RDConnect(application_id = "your_key")
#' stream <- rd_get_streaming_data(
#'   universe = "AAPL.O",
#'   fields = c("BID", "ASK"),
#'   RDObject = RD
#' )
#' stream$open()
#' }
#'
#' \dontrun{
#' # Live plotting
#' stream <- rd_get_streaming_data(
#'   universe = "EUR=",
#'   fields = c("BID", "ASK")
#' )
#' stream$open()
#' 
#' # Create and run live plot
#' app <- stream$plot_live(field = "BID")
#' shiny::runApp(app)  # Opens in browser
#' }
#'
#' \dontrun{
#' # Get summary statistics
#' stream <- rd_get_streaming_data(
#'   universe = c("EUR=", "GBP="),
#'   fields = c("BID", "ASK")
#' )
#' stream$open()
#' Sys.sleep(10)  # Collect some data
#' summary <- stream$get_summary()
#' print(summary)
#' }
rd_get_streaming_data <- function(
  universe,
  fields,
  on_refresh = NULL,
  on_update = NULL,
  on_error = NULL,
  RDObject = NULL,
  stream_type = "pricing",
  domain = "MarketPrice",
  parameters = NULL
) {
  # Validate inputs
  validate_streaming_params(universe, fields)
  
  # Check dependencies
  if (!requireNamespace("websocket", quietly = TRUE)) {
    stop("Please install 'websocket' package: install.packages('websocket')")
  }
  
  if (!requireNamespace("later", quietly = TRUE)) {
    stop("Please install 'later' package: install.packages('later')")
  }
  
  # Create stream definition
  if (stream_type == "pricing") {
    stream_def <- rd_streaming_pricing$Definition$new(
      universe = universe,
      fields = fields,
      parameters = parameters,
      domain = domain
    )
  } else {
    stop(paste("Stream type", stream_type, "not yet implemented"))
  }
  
  # Create stream
  stream <- stream_def$get_stream()
  
  # Register callbacks if provided
  if (!is.null(on_refresh)) {
    stream$on_refresh(on_refresh)
  }
  
  if (!is.null(on_update)) {
    stream$on_update(on_update)
  }
  
  if (!is.null(on_error)) {
    stream$on_error(on_error)
  }
  
  return(stream)
}

# rd_streaming_pricing is defined in rd_streaming_definition.R
# This file provides the high-level API function

