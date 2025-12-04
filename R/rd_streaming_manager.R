#' StreamManager - Centralized WebSocket connection management
#'
#' @description
#' Manages WebSocket connections for streaming data, handling authentication,
#' message routing, and connection lifecycle.
#'
#' @import R6
#' @importFrom jsonlite fromJSON
#' @noRd
StreamManager <- R6::R6Class("StreamManager",
  public = list(
    #' @description Initialize StreamManager
    #' @param base_url Base URL for WebSocket (optional, uses options if NULL)
    #' @param port Port number (optional, uses option streaming_port if not provided)
    #' @param stream_type Type of stream ("pricing", "analytics")
    initialize = function(base_url = NULL, port = NULL, stream_type = "pricing") {
      private$.base_url <- base_url
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
      private$.port <- port
      private$.stream_type <- stream_type
      private$.connection_state <- "disconnected"
      private$.streams <- list()
      private$.message_handlers <- list()
      private$.ws <- NULL
      private$.stream_id_counter <- 0L
      private$.logged_in <- FALSE
      private$.pending_subscriptions <- list()
    },
    
    #' @description Connect to WebSocket endpoint
    #' @param force Force reconnection even if already connected
    #' @param debug Enable debug logging (default: FALSE)
    #' @return TRUE if successful
    connect = function(force = FALSE, debug = FALSE) {
      if (!requireNamespace("websocket", quietly = TRUE)) {
        stop("Please install 'websocket' package for streaming functionality")
      }
      
      if (private$.connection_state == "connected" && !force) {
        if (debug) message("[DEBUG] Already connected, skipping")
        return(TRUE)
      }
      
      if (debug) message("[DEBUG] Starting connection process...")
      
      # Get access token
      if (debug) message("[DEBUG] Getting access token via rd_handshake()...")
      handshake <- rd_handshake()
      access_token <- handshake$access_token
      if (debug) message("[DEBUG] Access token obtained, length: ", nchar(access_token))
      
      # Get URL and protocol
      url <- get_streaming_url(private$.stream_type, private$.port)
      protocol <- get_streaming_protocol(private$.stream_type)
      headers <- create_streaming_headers(access_token)
      
      if (debug) {
        message("[DEBUG] WebSocket URL: ", url)
        message("[DEBUG] Protocol: ", protocol)
        message("[DEBUG] Headers: ", paste(names(headers), collapse = ", "))
      }
      
      # Create WebSocket connection
      if (debug) message("[DEBUG] Creating WebSocket object...")
      private$.ws <- websocket::WebSocket$new(
        url = url,
        protocols = protocol,
        headers = headers,
        autoConnect = FALSE
      )
      
      # Set up event handlers
      if (debug) message("[DEBUG] Setting up WebSocket event handlers...")
      private$.setup_websocket_handlers(debug = debug)
      
      # Connect
      if (debug) message("[DEBUG] Connecting to WebSocket...")
      private$.ws$connect()
      
      # Poll until connected
      if (debug) message("[DEBUG] Polling until connected...")
      poll_until_connected(private$.ws, timeout = 10)
      if (debug) message("[DEBUG] WebSocket connected!")
      
      # Send login request
      if (private$.stream_type == "pricing") {
        login_request <- create_omm_login_request(access_token = access_token)
        if (debug) {
          message("[DEBUG] Sending login request:")
          message("[DEBUG] ", login_request)
        }
        private$.ws$send(login_request)
        
        # Wait for login response and process events
        # Give up to 3 seconds for login to complete
        if (debug) message("[DEBUG] Waiting for login response (up to 3 seconds)...")
        login_timeout <- Sys.time() + 3
        message_count <- 0L
        if (requireNamespace("later", quietly = TRUE)) {
          while (!private$.logged_in && Sys.time() < login_timeout) {
            later::run_now(0.1)
            Sys.sleep(0.1)
            message_count <- message_count + 1L
            if (debug && message_count %% 10 == 0) {
              message("[DEBUG] Still waiting for login... (", message_count * 0.1, "s elapsed)")
            }
          }
        } else {
          Sys.sleep(1)
        }
        
        if (debug) {
          if (private$.logged_in) {
            message("[DEBUG] Login successful! (detected via message)")
          } else {
            message("[DEBUG] Login response not detected, using fallback")
          }
        }
        
        # If login didn't complete, try subscribing anyway (some servers don't send explicit login response)
        if (!private$.logged_in && length(private$.pending_subscriptions) > 0) {
          if (debug) message("[DEBUG] Using fallback: assuming login succeeded")
          # Assume login succeeded and subscribe
          private$.logged_in <- TRUE
          for (stream_id in names(private$.pending_subscriptions)) {
            stream_def <- private$.pending_subscriptions[[stream_id]]
            if (!is.null(stream_def)) {
              request <- stream_def$to_request(streaming = TRUE, stream_id = as.integer(stream_id))
              if (debug) {
                message("[DEBUG] Sending subscription request for stream ", stream_id, ":")
                message("[DEBUG] ", request)
              }
              if (!is.null(private$.ws)) {
                private$.ws$send(request)
              }
            }
          }
          private$.pending_subscriptions <- list()
        }
      }
      
      private$.connection_state <- "connected"
      if (debug) message("[DEBUG] Connection process complete. State: ", private$.connection_state)
      return(TRUE)
    },
    
    #' @description Disconnect from WebSocket
    disconnect = function() {
      if (!is.null(private$.ws)) {
        tryCatch({
          private$.ws$close()
        }, error = function(e) {
          warning("Error closing WebSocket: ", e$message)
        })
        private$.ws <- NULL
      }
      private$.connection_state <- "disconnected"
      private$.streams <- list()
      private$.message_handlers <- list()
    },
    
    #' @description Subscribe to a stream
    #' @param stream_def StreamDefinition object
    #' @return Stream ID
    subscribe = function(stream_def) {
      # Ensure connection is established first
      if (private$.connection_state != "connected") {
        # Get debug setting
        debug <- getOption("refinitiv_streaming_debug", FALSE)
        self$connect(debug = debug)
      }
      
      # Generate stream ID
      # Start at 2 because ID 1 is used for login
      if (private$.stream_id_counter == 0L) {
        private$.stream_id_counter <- 2L
      } else {
        private$.stream_id_counter <- private$.stream_id_counter + 1L
      }
      stream_id <- private$.stream_id_counter
      
      # Store stream definition
      private$.streams[[as.character(stream_id)]] <- stream_def
      
      # For pricing streams, we need to wait for login
      debug <- getOption("refinitiv_streaming_debug", FALSE)
      
      if (private$.stream_type == "pricing") {
        # Wait a bit for login to complete if it's in progress
        # Check up to 2 seconds
        login_wait_timeout <- Sys.time() + 2
        while (!private$.logged_in && Sys.time() < login_wait_timeout) {
          if (requireNamespace("later", quietly = TRUE)) {
            later::run_now(0.1)
          }
          Sys.sleep(0.1)
        }
        
        # If still not logged in, queue the subscription
        if (!private$.logged_in) {
          if (debug) message("[DEBUG] Login not complete, queueing subscription for stream ", stream_id)
          private$.pending_subscriptions[[as.character(stream_id)]] <- stream_def
          return(stream_id)
        }
      }
      
      # If logged in (or not pricing stream), send subscription immediately
      if (private$.logged_in || private$.stream_type != "pricing") {
        request <- stream_def$to_request(streaming = TRUE, stream_id = stream_id)
        if (debug) {
          message("[DEBUG] Sending subscription immediately (login complete) for stream ", stream_id, ":")
          message("[DEBUG] ", request)
        }
        if (!is.null(private$.ws)) {
          private$.ws$send(request)
          # Process events to ensure message is sent
          if (requireNamespace("later", quietly = TRUE)) {
            later::run_now(0.2)
          } else {
            Sys.sleep(0.2)
          }
        } else {
          if (debug) message("[DEBUG] WARNING: WebSocket is NULL, cannot send subscription")
        }
        return(stream_id)
      }
      
      # Final fallback: try sending anyway
      if (debug) message("[DEBUG] Fallback: sending subscription without login check")
      request <- stream_def$to_request(streaming = TRUE, stream_id = stream_id)
      if (!is.null(private$.ws)) {
        private$.ws$send(request)
        if (requireNamespace("later", quietly = TRUE)) {
          later::run_now(0.1)
        }
      }
      
      return(stream_id)
    },
    
    #' @description Unsubscribe from a stream
    #' @param stream_id Stream ID to unsubscribe
    unsubscribe = function(stream_id) {
      stream_id_char <- as.character(stream_id)
      if (stream_id_char %in% names(private$.streams)) {
        private$.streams[[stream_id_char]] <- NULL
      }
    },
    
    #' @description Get connection state
    #' @return Connection state string
    get_connection_state = function() {
      return(private$.connection_state)
    },
    
    #' @description Register message handler for a stream
    #' @param stream_id Stream ID
    #' @param handler Function to handle messages
    register_handler = function(stream_id, handler) {
      stream_id_char <- as.character(stream_id)
      if (!stream_id_char %in% names(private$.message_handlers)) {
        private$.message_handlers[[stream_id_char]] <- list()
      }
      private$.message_handlers[[stream_id_char]] <- c(
        private$.message_handlers[[stream_id_char]],
        list(handler)
      )
    },
    
    #' @description Unregister message handler for a stream
    #' @param stream_id Stream ID
    unregister_handler = function(stream_id) {
      stream_id_char <- as.character(stream_id)
      if (stream_id_char %in% names(private$.message_handlers)) {
        private$.message_handlers[[stream_id_char]] <- NULL
      }
    },
    
    #' @description Get WebSocket object (for advanced usage)
    #' @return WebSocket object or NULL
    get_websocket = function() {
      return(private$.ws)
    }
  ),
  
  private = list(
    .base_url = NULL,
    .port = 9060,
    .stream_type = "pricing",
    .connection_state = "disconnected",
    .ws = NULL,
    .streams = list(),
    .message_handlers = list(),
    .stream_id_counter = 0L,
    .logged_in = FALSE,
    .pending_subscriptions = list(),
    
    .setup_websocket_handlers = function(debug = FALSE) {
      # Error handler
      private$.ws$onError(function(event) {
        if (debug) message("[DEBUG] WebSocket error: ", event$message)
        warning("WebSocket error: ", event$message)
        private$.connection_state <- "error"
      })
      
      # Message handler
      private$.ws$onMessage(function(event) {
        if (debug) {
          message("[DEBUG] ===== RAW MESSAGE RECEIVED =====")
          message("[DEBUG] Message length: ", nchar(event$data))
          message("[DEBUG] Message preview (first 500 chars): ", substr(event$data, 1, 500))
        }
        private$.process_message(event, debug = debug)
      })
      
      # Close handler
      private$.ws$onClose(function(event) {
        if (debug) message("[DEBUG] WebSocket closed")
        private$.connection_state <- "disconnected"
      })
      
      # Open handler
      private$.ws$onOpen(function(event) {
        if (debug) message("[DEBUG] WebSocket opened")
      })
    },
    
    .process_message = function(event, debug = FALSE) {
      tryCatch({
        # Check if event has data
        if (is.null(event) || is.null(event$data) || length(event$data) == 0) {
          if (debug) message("[DEBUG] Event has no data, skipping")
          return()
        }
        
        if (debug) message("[DEBUG] Parsing JSON message...")
        # Parse JSON message
        message_data <- jsonlite::fromJSON(event$data, simplifyDataFrame = FALSE)
        
        # Check if parsing succeeded
        if (is.null(message_data) || length(message_data) == 0) {
          if (debug) message("[DEBUG] Parsed message is empty, skipping")
          return()
        }
        
        # Handle array messages - OMM protocol can send arrays
        # If message_data is an unnamed list with one element, it's an array
        if (is.list(message_data) && length(message_data) == 1 && 
            (is.null(names(message_data)) || length(names(message_data)) == 0)) {
          # This is a list with one element (array was parsed)
          message_data <- message_data[[1]]
          if (debug) message("[DEBUG] Message was an array, extracted first element")
        }
        
        if (debug) {
          message("[DEBUG] Parsed message structure:")
          if (!is.null(names(message_data))) {
            message("[DEBUG]   Top-level keys: ", paste(names(message_data), collapse = ", "))
          }
          message("[DEBUG]   Message type: ", class(message_data))
          if (is.list(message_data)) {
            message("[DEBUG]   Message structure: ", paste(capture.output(str(message_data, max.level = 2)), collapse = "\n"))
          }
        }
        
        # Handle different message types
        message_type <- NULL
        if (!is.null(message_data$Type) && length(message_data$Type) > 0) {
          message_type <- message_data$Type
          if (debug) message("[DEBUG] Found message Type: ", message_type)
        } else if (!is.null(message_data$type) && length(message_data$type) > 0) {
          message_type <- message_data$type
          if (debug) message("[DEBUG] Found message type: ", message_type)
        }
        
        if (is.null(message_type) || length(message_type) == 0) {
          if (debug) {
            if (!is.null(names(message_data))) {
              message("[DEBUG] No message type found. Available keys: ", paste(names(message_data), collapse = ", "))
            } else {
              message("[DEBUG] No message type found. Message is not a named list.")
            }
            # Still try to process if it might be a login response
            if (!is.null(message_data$Domain)) {
              message("[DEBUG] Found Domain field: ", message_data$Domain)
            }
            if (!is.null(message_data$ID) || !is.null(message_data$id)) {
              msg_id <- if (!is.null(message_data$ID)) message_data$ID else message_data$id
              message("[DEBUG] Found ID field: ", msg_id)
            }
          }
          return()
        }
        
        # Convert to character for comparison
        message_type <- as.character(message_type)[1]
        if (debug) message("[DEBUG] Processing message type: ", message_type)
        
        if (message_type == "Refresh") {
          if (debug) message("[DEBUG] Handling Refresh message...")
          private$.handle_refresh(message_data, debug = debug)
        } else if (message_type == "Update") {
          if (debug) message("[DEBUG] Handling Update message...")
          private$.handle_update(message_data)
        } else if (message_type == "Ping") {
          if (debug) message("[DEBUG] Handling Ping message...")
          private$.handle_ping()
        } else if (message_type == "Status") {
          if (debug) message("[DEBUG] Handling Status message...")
          private$.handle_status(message_data)
        } else {
          if (debug) message("[DEBUG] Unknown message type, routing to handlers...")
          # Unknown message type - route to all handlers
          private$.route_message(message_data)
        }
      }, error = function(e) {
        # More informative error
        error_msg <- as.character(e$message)[1]
        if (debug) {
          message("[DEBUG] ERROR processing message: ", error_msg)
          message("[DEBUG] Error traceback: ", paste(capture.output(traceback()), collapse = "\n"))
        }
        # Only warn if it's not a common parsing issue
        if (!grepl("argument is of length zero|subscript out of bounds", error_msg, ignore.case = TRUE)) {
          warning("Error processing message: ", error_msg)
        }
      })
    },
    
    .handle_refresh = function(message_data, debug = FALSE) {
      if (debug) message("[DEBUG] ===== HANDLING REFRESH MESSAGE =====")
      
      # Check if this is a login refresh
      # Login response can come in different formats
      domain <- NULL
      if (!is.null(message_data$Domain) && length(message_data$Domain) > 0) {
        domain <- as.character(message_data$Domain)[1]
        if (debug) message("[DEBUG] Refresh message Domain: ", domain)
      } else {
        if (debug) message("[DEBUG] Refresh message has no Domain field")
      }
      
      # Also check if message has ID=1 (login request ID)
      message_id <- NULL
      if (!is.null(message_data$ID)) {
        message_id <- message_data$ID
        if (debug) message("[DEBUG] Refresh message ID: ", message_id)
      } else if (!is.null(message_data$id)) {
        message_id <- message_data$id
        if (debug) message("[DEBUG] Refresh message id: ", message_id)
      } else {
        if (debug) message("[DEBUG] Refresh message has no ID field")
      }
      
      # Check State field (login success indicator)
      state_info <- NULL
      if (!is.null(message_data$State)) {
        state_info <- message_data$State
        if (debug) {
          if (is.list(state_info)) {
            message("[DEBUG] Refresh message State: ", paste(names(state_info), collapse = ", "))
            if (!is.null(state_info$Stream)) message("[DEBUG]   State$Stream: ", state_info$Stream)
            if (!is.null(state_info$Data)) message("[DEBUG]   State$Data: ", state_info$Data)
          } else {
            message("[DEBUG] Refresh message State: ", state_info)
          }
        }
      }
      
      # Login successful if domain is "Login" OR if it's a response to our login request (ID=1)
      is_login_response <- (!is.null(domain) && domain == "Login") || 
                          (!is.null(message_id) && !is.na(as.numeric(message_id)) && as.numeric(message_id) == 1)
      
      if (debug) {
        message("[DEBUG] Is login response? ", is_login_response)
        message("[DEBUG] Currently logged in? ", private$.logged_in)
      }
      
      if (is_login_response && !private$.logged_in) {
        if (debug) message("[DEBUG] *** LOGIN SUCCESSFUL! ***")
        # Login successful
        private$.logged_in <- TRUE
        
        # Small delay to ensure login is fully processed
        if (requireNamespace("later", quietly = TRUE)) {
          later::run_now(0.2)
        } else {
          Sys.sleep(0.2)
        }
        
        # Now subscribe to all pending streams AND any streams already registered
        all_stream_ids <- unique(c(names(private$.pending_subscriptions), names(private$.streams)))
        if (debug) {
          message("[DEBUG] Subscribing to ", length(all_stream_ids), " stream(s)...")
          message("[DEBUG] Pending subscriptions: ", length(private$.pending_subscriptions))
          message("[DEBUG] Active streams: ", length(private$.streams))
        }
        
        if (length(all_stream_ids) > 0) {
          for (stream_id in all_stream_ids) {
            # Get from pending first, then from active streams
            stream_def <- private$.pending_subscriptions[[stream_id]]
            if (is.null(stream_def)) {
              stream_def <- private$.streams[[stream_id]]
            }
            if (!is.null(stream_def)) {
              request <- stream_def$to_request(streaming = TRUE, stream_id = as.integer(stream_id))
              if (debug) {
                message("[DEBUG] Sending subscription for stream ", stream_id, ":")
                message("[DEBUG] ", request)
              }
              if (!is.null(private$.ws)) {
                private$.ws$send(request)
                # Give a moment for the request to be sent
                if (requireNamespace("later", quietly = TRUE)) {
                  later::run_now(0.1)
                } else {
                  Sys.sleep(0.1)
                }
              }
            }
          }
        } else {
          if (debug) message("[DEBUG] No streams to subscribe - subscriptions may be added later")
        }
        
        # Clear pending subscriptions
        private$.pending_subscriptions <- list()
        if (debug) message("[DEBUG] Login and subscription complete!")
        return()
      }
      
      # Route to stream handlers
      if (debug) message("[DEBUG] Routing refresh message to stream handlers...")
      private$.route_message(message_data, message_type = "refresh")
    },
    
    .handle_update = function(message_data) {
      private$.route_message(message_data, message_type = "update")
    },
    
    .handle_ping = function() {
      # Respond to ping
      if (!is.null(private$.ws)) {
        private$.ws$send('{"Type":"Pong"}')
      }
    },
    
    .handle_status = function(message_data) {
      # Handle status messages (errors, etc.)
      debug <- getOption("refinitiv_streaming_debug", FALSE)
      
      if (!is.null(message_data$State)) {
        state <- message_data$State
        status_text <- if (is.list(state) && !is.null(state$Text)) state$Text else ""
        status_code <- if (is.list(state) && !is.null(state$Code)) state$Code else ""
        stream_state <- if (is.list(state) && !is.null(state$Stream)) state$Stream else ""
        
        if (debug) {
          message("[DEBUG] Status message - Code: ", status_code, " | Stream: ", stream_state, " | Text: ", status_text)
        }
        
        # Check if this is a duplicate request error - might mean subscription already exists
        if (grepl("DuplicateOpenRequest|already exists", status_text, ignore.case = TRUE)) {
          if (debug) {
            message("[DEBUG] Duplicate request detected - subscription may already be active, ignoring")
          }
          # Don't treat this as a fatal error - subscription might already be working
          return()
        }
        
        # Only warn for actual errors
        if (stream_state == "Closed" || stream_state == "NonStreaming") {
          if (!grepl("DuplicateOpenRequest", status_text, ignore.case = TRUE)) {
            warning("Stream status: ", stream_state)
          }
        }
        if (!is.null(status_text) && nchar(status_text) > 0 && 
            !grepl("Ok|Open|DuplicateOpenRequest", status_text, ignore.case = TRUE)) {
          warning("Stream status: ", status_text)
        }
      }
      private$.route_message(message_data, message_type = "status")
    },
    
    .route_message = function(message_data, message_type = NULL) {
      # Route message to registered handlers
      # Get message ID to route to specific stream
      message_id <- NULL
      if (!is.null(message_data$ID)) {
        message_id <- as.character(message_data$ID)
      } else if (!is.null(message_data$id)) {
        message_id <- as.character(message_data$id)
      }
      
      # If we have a message ID, route to specific handler
      if (!is.null(message_id) && message_id %in% names(private$.message_handlers)) {
        handlers <- private$.message_handlers[[message_id]]
        for (handler in handlers) {
          tryCatch({
            handler(message_data, message_type)
          }, error = function(e) {
            warning("Error in message handler: ", e$message)
          })
        }
        return()
      }
      
      # Fallback: route to all handlers (for messages without ID)
      for (stream_id in names(private$.message_handlers)) {
        handlers <- private$.message_handlers[[stream_id]]
        for (handler in handlers) {
          tryCatch({
            handler(message_data, message_type)
          }, error = function(e) {
            warning("Error in message handler: ", e$message)
          })
        }
      }
    }
  )
)

