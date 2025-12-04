#' Debug streaming connection
#'
#' @description
#' Helper function to debug streaming issues by showing what messages are received
#'
#' @param stream Stream object
#' @param verbose Show detailed message information
#' @return List with debug information
#' @export
rd_streaming_debug <- function(stream, verbose = FALSE) {
  if (!inherits(stream, "Stream")) {
    stop("stream must be a Stream object")
  }
  
  manager <- stream$.__enclos_env__$private$.manager
  ws <- manager$.__enclos_env__$private$.ws
  
  debug_info <- list(
    stream_open = stream$is_open(),
    connection_state = manager$get_connection_state(),
    logged_in = manager$.__enclos_env__$private$.logged_in,
    stream_id = stream$.__enclos_env__$private$.stream_id,
    websocket_ready = if (!is.null(ws)) ws$readyState() else NULL,
    data_history_rows = nrow(stream$get_data_history()),
    latest_data_count = length(stream$get_latest_data()),
    pending_subscriptions = length(manager$.__enclos_env__$private$.pending_subscriptions),
    active_streams = length(manager$.__enclos_env__$private$.streams)
  )
  
  if (verbose) {
    debug_info$definition <- list(
      universe = stream$get_definition()$get_universe(),
      fields = stream$get_definition()$get_fields()
    )
    debug_info$latest_data <- stream$get_latest_data()
    debug_info$data_history_sample <- if (nrow(stream$get_data_history()) > 0) {
      head(stream$get_data_history(), 5)
    } else {
      NULL
    }
  }
  
  return(debug_info)
}


