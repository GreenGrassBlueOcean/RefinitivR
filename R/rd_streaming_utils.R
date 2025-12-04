# Helper functions for streaming functionality

#' Poll until WebSocket is connected
#'
#' @param ws WebSocket object
#' @param timeout Timeout in seconds
#' @return TRUE if connected, stops with error if not
#' @noRd
poll_until_connected <- function(ws, timeout = 5) {
  if (!requireNamespace("later", quietly = TRUE)) {
    stop("Please install 'later' package for streaming functionality")
  }
  
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
  
  return(TRUE)
}

#' Build JSON array string from vector
#'
#' @param fieldsvector Character vector of field names
#' @return JSON array string
#' @noRd
build_json_list_string <- function(fieldsvector) {
  return(paste0("[\"", paste0(fieldsvector, collapse = "\",\""), "\"]"))
}

#' Get WebSocket URL for streaming endpoint
#'
#' @param stream_type Type of stream ("pricing", "analytics", etc.)
#' @param port Port number (optional, uses option streaming_port if not provided)
#' @return WebSocket URL
#' @noRd
get_streaming_url <- function(stream_type = "pricing", port = NULL) {
  base_url <- getOption("refinitiv_base_url")
  if (is.null(base_url)) {
    base_url <- "http://localhost"
  }
  
  # Get port from options if not provided
  if (is.null(port)) {
    port <- getOption("streaming_port")
    if (is.null(port)) {
      # Fallback: try eikon_port, then rdp_port, then default
      port <- getOption("eikon_port")
      if (is.null(port)) {
        port <- getOption("rdp_port", 9060L)
      }
    }
  }
  
  # Convert http to ws
  ws_base <- gsub("^http", "ws", base_url)
  
  # Map stream types to endpoints
  endpoints <- list(
    pricing = "api/rdp/streaming/pricing/v1/WebSocket",
    analytics = "api/rdp/streaming/quantitative-analytics/beta1/financial-contracts/WebSocket"
  )
  
  endpoint <- endpoints[[stream_type]]
  if (is.null(endpoint)) {
    stop(paste("Unknown stream type:", stream_type))
  }
  
  return(paste0(ws_base, ":", port, "/", endpoint))
}

#' Get WebSocket protocol for stream type
#'
#' @param stream_type Type of stream
#' @return Protocol string
#' @noRd
get_streaming_protocol <- function(stream_type = "pricing") {
  protocols <- list(
    pricing = "tr_json2",
    analytics = "rdp_streaming"
  )
  
  protocol <- protocols[[stream_type]]
  if (is.null(protocol)) {
    return("tr_json2")  # Default
  }
  
  return(protocol)
}

#' Create login request for OMM/tr_json2 protocol
#'
#' @param app_key Application key
#' @param access_token Access token
#' @return JSON string
#' @noRd
create_omm_login_request <- function(app_key = "DEFAULT_WORKSPACE_APP_KEY", access_token) {
  position <- paste0('127.0.0.1/', as.character(Sys.info()["nodename"]))
  
  login_json_string <- paste0(
    '{"ID":1,"Domain":"Login","Key":{"Elements":{"AppKey":"', app_key,
    '","Authorization":"Bearer ', access_token,
    '","ApplicationId":256,"Position":"', position, '"}}}'
  )
  
  return(login_json_string)
}

#' Create stream subscription request for OMM/tr_json2 protocol
#'
#' @param domain Domain (e.g., "MarketPrice")
#' @param universe Character vector of instrument RICs
#' @param fields Character vector of field names
#' @param streaming Logical, TRUE for streaming, FALSE for snapshot
#' @return JSON string
#' @noRd
create_omm_stream_request <- function(domain, universe, fields, streaming = TRUE, stream_id = NULL) {
  # For now, handle single instrument (will extend to multiple)
  if (length(universe) > 1) {
    warning("Multiple instruments not yet fully supported, using first instrument")
    universe <- universe[1]
  }
  
  # Ensure universe is a single character value
  universe <- as.character(universe)[1]
  
  fields_json <- build_json_list_string(fields)
  
  # Use provided stream_id or default to 2
  # Note: ID should be unique for each request
  request_id <- if (!is.null(stream_id)) as.integer(stream_id) else 2L
  
  # Build the request JSON
  request_json <- paste0(
    '{"ID":', request_id, ',"Domain":"', domain,
    '","Streaming":', tolower(as.character(streaming)),
    ',"Key":{"Name":"', universe,
    '"},"View":', fields_json, '}'
  )
  
  return(request_json)
}

#' Create headers for WebSocket connection
#'
#' @param access_token Access token
#' @return List of headers
#' @noRd
create_streaming_headers <- function(access_token) {
  return(list(
    'User-Agent' = "R",
    'x-tr-applicationid' = 'DEFAULT_WORKSPACE_APP_KEY',
    'Authorization' = paste('Bearer', access_token)
  ))
}

#' Validate streaming parameters
#'
#' @param universe Character vector of instrument RICs
#' @param fields Character vector of field names
#' @return TRUE if valid, stops with error if not
#' @noRd
validate_streaming_params <- function(universe, fields) {
  if (is.null(universe) || length(universe) == 0) {
    stop("universe must be a non-empty character vector")
  }
  
  if (!is.character(universe)) {
    stop("universe must be a character vector")
  }
  
  if (is.null(fields) || length(fields) == 0) {
    stop("fields must be a non-empty character vector")
  }
  
  if (!is.character(fields)) {
    stop("fields must be a character vector")
  }
  
  return(TRUE)
}

