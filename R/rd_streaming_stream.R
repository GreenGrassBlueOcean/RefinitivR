#' Stream - Active streaming instance
#'
#' @description
#' Represents an active streaming connection with callback support
#' and data management.
#'
#' @import R6
#' @importFrom data.table data.table rbindlist
#' @noRd
Stream <- R6::R6Class("Stream",
  public = list(
    #' @description Initialize Stream
    #' @param definition StreamDefinition object
    #' @param manager StreamManager object
    initialize = function(definition, manager) {
      private$.definition <- definition
      private$.manager <- manager
      private$.stream_id <- NULL
      private$.is_open <- FALSE
      private$.latest_data <- list()
      private$.data_history <- data.table::data.table()
      
      # Initialize callbacks
      private$.callbacks <- list(
        refresh = list(),
        update = list(),
        error = list()
      )
    },
    
    #' @description Open stream (start streaming)
    #' @return self (for chaining)
    open = function() {
      if (private$.is_open) {
        warning("Stream is already open")
        return(self)
      }
      
      # Get debug setting
      debug <- getOption("refinitiv_streaming_debug", FALSE)
      
      # Ensure manager is connected
      if (private$.manager$get_connection_state() != "connected") {
        private$.manager$connect(debug = debug)
      }
      
      # Register message handler first
      handler <- function(message_data, message_type = NULL) {
        private$.handle_message(message_data, message_type)
      }
      
      # Subscribe to stream - this will wait for login if needed
      if (debug) message("[DEBUG] Calling subscribe() for stream definition...")
      private$.stream_id <- private$.manager$subscribe(private$.definition)
      if (debug) message("[DEBUG] Subscribe returned stream_id: ", private$.stream_id)
      
      # Register handler
      private$.manager$register_handler(private$.stream_id, handler)
      if (debug) message("[DEBUG] Handler registered for stream_id: ", private$.stream_id)
      
      # If login is complete but subscription wasn't sent (race condition), send it now
      if (private$.manager$.__enclos_env__$private$.logged_in) {
        # Check if subscription was actually sent by trying to send it again
        # (The subscribe method should have sent it, but let's ensure)
        stream_def <- private$.definition
        request <- stream_def$to_request(streaming = TRUE, stream_id = private$.stream_id)
        ws <- private$.manager$.__enclos_env__$private$.ws
        if (!is.null(ws)) {
          # Only send if we haven't already subscribed
          # We can't easily check this, so we'll rely on the subscribe method
          # But ensure the event loop processes any pending messages
          if (requireNamespace("later", quietly = TRUE)) {
            later::run_now(0.2)
          }
        }
      }
      
      private$.is_open <- TRUE
      
      # Start event loop processing if later is available
      # This ensures messages are processed even when R is busy
      if (requireNamespace("later", quietly = TRUE)) {
        # Schedule periodic event loop processing
        private$.start_event_loop()
      }
      
      return(self)
    },
    
    #' @description Close stream (stop streaming)
    #' @return self (for chaining)
    close = function() {
      if (!private$.is_open) {
        return(self)
      }
      
      debug <- getOption("refinitiv_streaming_debug", FALSE)
      if (debug) message("[DEBUG] Closing stream...")
      
      # Mark as closed FIRST to prevent any new messages from being processed
      private$.is_open <- FALSE
      
      # Stop event loop
      private$.event_loop_active <- FALSE
      
      # Remove handler from manager IMMEDIATELY to stop routing messages
      if (!is.null(private$.stream_id)) {
        if (debug) message("[DEBUG] Removing handler for stream_id: ", private$.stream_id)
        private$.manager$unregister_handler(private$.stream_id)
      }
      
      # Send unsubscribe request to server if we have a stream_id
      if (!is.null(private$.stream_id)) {
        # Send close request for this stream
        ws <- private$.manager$.__enclos_env__$private$.ws
        if (!is.null(ws)) {
          # Send close request - OMM protocol uses Close message
          close_request <- paste0(
            '{"ID":', private$.stream_id, 
            ',"Domain":"MarketPrice","Key":{"Name":"', 
            private$.definition$get_universe()[1], 
            '"},"Type":"Close"}'
          )
          if (debug) {
            message("[DEBUG] Sending unsubscribe request: ", close_request)
          }
          tryCatch({
            ws$send(close_request)
            # Give a moment for the request to be sent
            if (requireNamespace("later", quietly = TRUE)) {
              later::run_now(0.1)
            }
          }, error = function(e) {
            if (debug) message("[DEBUG] Error sending unsubscribe: ", e$message)
          })
        }
        
        # Unsubscribe from manager
        private$.manager$unsubscribe(private$.stream_id)
        private$.stream_id <- NULL
      }
      
      if (debug) message("[DEBUG] Stream closed")
      return(self)
    },
    
    #' @description Register refresh callback
    #' @param callback Function with signature (stream, instrument, fields)
    #' @return self (for chaining)
    on_refresh = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function")
      }
      private$.callbacks$refresh <- c(private$.callbacks$refresh, list(callback))
      return(self)
    },
    
    #' @description Register update callback
    #' @param callback Function with signature (stream, instrument, fields)
    #' @return self (for chaining)
    on_update = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function")
      }
      private$.callbacks$update <- c(private$.callbacks$update, list(callback))
      return(self)
    },
    
    #' @description Register error callback
    #' @param callback Function with signature (stream, error_message)
    #' @return self (for chaining)
    on_error = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function")
      }
      private$.callbacks$error <- c(private$.callbacks$error, list(callback))
      return(self)
    },
    
    #' @description Get latest data snapshot
    #' @param instrument Optional instrument RIC (returns all if NULL)
    #' @return List or data.frame with latest data
    get_latest_data = function(instrument = NULL) {
      if (is.null(instrument)) {
        return(private$.latest_data)
      }
      
      if (instrument %in% names(private$.latest_data)) {
        return(private$.latest_data[[instrument]])
      }
      
      return(NULL)
    },
    
    #' @description Get data history (buffered)
    #' @return data.table with historical updates
    get_data_history = function() {
      return(private$.data_history)
    },
    
    #' @description Check if stream is open
    #' @return Logical
    is_open = function() {
      return(private$.is_open)
    },
    
    #' @description Get stream definition
    #' @return StreamDefinition object
    get_definition = function() {
      return(private$.definition)
    },
    
    #' @description Create a live Shiny plot for streaming data
    #' @param field Field name to plot (e.g., "BID", "ASK")
    #' @param instrument Instrument RIC to plot (uses first if NULL)
    #' @param xrange Number of recent data points to display
    #' @param update_interval Update interval in milliseconds (default 1000)
    #' @param auto_ylim Automatically adjust y-axis limits (default TRUE)
    #' @param ylim Manual y-axis limits (used if auto_ylim is FALSE)
    #' @return Shiny app object (call with shiny::runApp() or return from function)
    plot_live = function(field = NULL, instrument = NULL, xrange = 30, 
                        update_interval = 1000, auto_ylim = TRUE, ylim = NULL) {
      if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Please install 'shiny' package: install.packages('shiny')")
      }
      
      # Determine field to plot
      if (is.null(field)) {
        fields <- private$.definition$get_fields()
        if (length(fields) == 0) {
          stop("No fields available to plot")
        }
        field <- fields[1]
        message(paste("No field specified, using first field:", field))
      }
      
      # Determine instrument to plot
      if (is.null(instrument)) {
        universe <- private$.definition$get_universe()
        if (length(universe) == 0) {
          stop("No instruments available to plot")
        }
        instrument <- universe[1]
        if (length(universe) > 1) {
          message(paste("Multiple instruments available, plotting first:", instrument))
        }
      }
      
      # Store reference to stream for Shiny app
      stream_ref <- self
      field_ref <- field
      instrument_ref <- instrument
      xrange_ref <- xrange
      auto_ylim_ref <- auto_ylim
      ylim_ref <- ylim
      
      # Create UI
      ui <- shiny::fluidPage(
        shiny::titlePanel(paste("Live Stream:", instrument_ref, "-", field_ref)),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::h4("Stream Info"),
            shiny::verbatimTextOutput("latest_value"),
            shiny::hr(),
            shiny::h4("Controls"),
            shiny::sliderInput("xrange", "Data Points to Show:", 
                              min = 10, max = 200, value = xrange_ref, step = 10),
            shiny::checkboxInput("auto_ylim", "Auto Y-axis", value = auto_ylim_ref),
            shiny::conditionalPanel(
              condition = "!input.auto_ylim",
              shiny::numericInput("ylim_min", "Y-axis Min:", value = NULL),
              shiny::numericInput("ylim_max", "Y-axis Max:", value = NULL)
            )
          ),
          shiny::mainPanel(
            shiny::plotOutput("live_plot", height = "600px")
          )
        )
      )
      
      # Create server
      server <- function(input, output, session) {
        # Plot output
        output$live_plot <- shiny::renderPlot({
          shiny::invalidateLater(update_interval, session)
          
          # Get data history - need to access it safely
          tryCatch({
            history <- stream_ref$get_data_history()
          }, error = function(e) {
            history <- data.table::data.table()
          })
          
          if (is.null(history) || nrow(history) == 0) {
            plot(1, type = "n", xlab = "Time", ylab = field_ref, 
                 main = paste("Waiting for data...", instrument_ref),
                 sub = paste("Stream status:", ifelse(stream_ref$is_open(), "Open", "Closed")))
            return()
          }
          
          # Filter for instrument and get recent data
          # Use data.table syntax for filtering
          if (nrow(history) > 0 && "instrument" %in% names(history)) {
            inst_data <- history[history$instrument == instrument_ref, ]
          } else {
            inst_data <- history
          }
          
          if (nrow(inst_data) == 0) {
            plot(1, type = "n", xlab = "Time", ylab = field_ref,
                 main = paste("No data for", instrument_ref),
                 sub = paste("Total history rows:", nrow(history)))
            return()
          }
          
          # Get field data
          if (!field_ref %in% names(inst_data)) {
            plot(1, type = "n", xlab = "Time", ylab = field_ref,
                 main = paste("Field", field_ref, "not found in data"),
                 sub = paste("Available fields:", paste(setdiff(names(inst_data), c("timestamp", "instrument", "message_type")), collapse = ", ")))
            return()
          }
          
          # Get number of points from slider or default
          n_points <- if (!is.null(input$xrange)) input$xrange else xrange_ref
          n_points <- min(n_points, nrow(inst_data))
          recent_data <- tail(inst_data, n_points)
          
          # Extract values and timestamps - handle both data.table and data.frame
          if (data.table::is.data.table(recent_data)) {
            values <- recent_data[[field_ref]]
            timestamps <- recent_data[["timestamp"]]
          } else {
            values <- recent_data[, field_ref]
            timestamps <- recent_data[, "timestamp"]
          }
          
          # Convert to numeric and remove NAs
          values <- as.numeric(values)
          valid_idx <- !is.na(values)
          values <- values[valid_idx]
          timestamps <- timestamps[valid_idx]
          
          if (length(values) == 0 || length(timestamps) == 0) {
            plot(1, type = "n", xlab = "Time", ylab = field_ref,
                 main = paste("No numeric data for", field_ref))
            return()
          }
          
          # Convert timestamps to POSIXct if needed
          if (!inherits(timestamps, "POSIXct")) {
            timestamps <- as.POSIXct(timestamps)
          }
          
          # Determine y-axis limits
          use_auto_ylim <- if (!is.null(input$auto_ylim)) input$auto_ylim else auto_ylim_ref
          
          if (use_auto_ylim && length(values) > 0) {
            y_min <- min(values, na.rm = TRUE) * 0.999
            y_max <- max(values, na.rm = TRUE) * 1.001
            if (y_min == y_max) {
              y_min <- y_min - abs(y_min) * 0.01
              y_max <- y_max + abs(y_max) * 0.01
            }
          } else if (!is.null(input$ylim_min) && !is.null(input$ylim_max) && 
                    !is.na(input$ylim_min) && !is.na(input$ylim_max)) {
            y_min <- input$ylim_min
            y_max <- input$ylim_max
          } else if (!is.null(ylim_ref)) {
            y_min <- ylim_ref[1]
            y_max <- ylim_ref[2]
          } else {
            y_min <- min(values, na.rm = TRUE)
            y_max <- max(values, na.rm = TRUE)
          }
          
          # Plot with timestamps on x-axis
          plot(timestamps, values,
               type = "l", lwd = 2,
               xlab = "Time", ylab = field_ref,
               main = paste("Live Stream:", instrument_ref, "-", field_ref),
               ylim = c(y_min, y_max),
               las = 1,
               col = "blue")
          
          # Add grid
          grid()
          
          # Add latest value point
          if (length(values) > 0 && length(timestamps) > 0) {
            points(timestamps[length(timestamps)], values[length(values)], 
                   col = "red", pch = 19, cex = 1.5)
          }
          
          # Add value labels for last few points
          if (length(values) >= 3 && length(timestamps) >= 3) {
            last_3 <- tail(values, 3)
            last_3_times <- tail(timestamps, 3)
            text(last_3_times, last_3, labels = round(last_3, 4), 
                 pos = 3, cex = 0.7, col = "darkblue")
          }
          
          # Add statistics text
          if (length(values) > 0) {
            stats_text <- paste(
              "Min:", round(min(values, na.rm = TRUE), 4),
              "| Max:", round(max(values, na.rm = TRUE), 4),
              "| Current:", round(values[length(values)], 4)
            )
            mtext(stats_text, side = 3, line = 0.5, cex = 0.8, col = "gray50")
          }
        })
        
        # Latest value output
        output$latest_value <- shiny::renderText({
          shiny::invalidateLater(update_interval, session)
          latest <- stream_ref$get_latest_data(instrument_ref)
          history <- stream_ref$get_data_history()
          
          info_lines <- c(
            paste("Instrument:", instrument_ref),
            paste("Field:", field_ref),
            paste("Stream Status:", ifelse(stream_ref$is_open(), "Open", "Closed")),
            paste("Total Updates:", nrow(history))
          )
          
          if (!is.null(latest) && is.list(latest) && field_ref %in% names(latest)) {
            value <- latest[[field_ref]]
            info_lines <- c(
              info_lines,
              paste("Latest Value:", value),
              paste("Time:", format(Sys.time(), "%H:%M:%S"))
            )
          } else {
            info_lines <- c(info_lines, "Waiting for data...")
          }
          
          paste(info_lines, collapse = "\n")
        })
      }
      
      return(shiny::shinyApp(ui = ui, server = server))
    },
    
    #' @description Get summary statistics of streamed data
    #' @param instrument Optional instrument RIC (uses all if NULL)
    #' @return data.frame with summary statistics
    get_summary = function(instrument = NULL) {
      history <- private$.data_history
      
      if (nrow(history) == 0) {
        return(data.frame(message = "No data available"))
      }
      
      if (!is.null(instrument)) {
        history <- history[history$instrument == instrument, ]
      }
      
      if (nrow(history) == 0) {
        return(data.frame(message = paste("No data for instrument:", instrument)))
      }
      
      # Get numeric columns
      numeric_cols <- sapply(history, is.numeric)
      numeric_cols <- names(numeric_cols)[numeric_cols]
      numeric_cols <- setdiff(numeric_cols, c("timestamp"))  # Exclude timestamp
      
      if (length(numeric_cols) == 0) {
        return(data.frame(
          instrument = ifelse(is.null(instrument), "All", instrument),
          total_updates = nrow(history),
          message = "No numeric fields to summarize"
        ))
      }
      
      # Calculate summary statistics
      summary_list <- list()
      summary_list$instrument <- ifelse(is.null(instrument), "All", instrument)
      summary_list$total_updates <- nrow(history)
      summary_list$first_update <- min(history$timestamp, na.rm = TRUE)
      summary_list$last_update <- max(history$timestamp, na.rm = TRUE)
      
      for (col in numeric_cols) {
        values <- as.numeric(history[[col]])
        values <- values[!is.na(values)]
        if (length(values) > 0) {
          summary_list[[paste0(col, "_min")]] <- min(values)
          summary_list[[paste0(col, "_max")]] <- max(values)
          summary_list[[paste0(col, "_mean")]] <- mean(values)
          summary_list[[paste0(col, "_last")]] <- values[length(values)]
        }
      }
      
      return(as.data.frame(summary_list))
    },
    
    #' @description Clear data history buffer
    #' @return self (for chaining)
    clear_history = function() {
      private$.data_history <- data.table::data.table()
      return(self)
    }
  ),
  
  private = list(
    .definition = NULL,
    .manager = NULL,
    .stream_id = NULL,
    .is_open = FALSE,
    .callbacks = list(),
    .latest_data = list(),
    .data_history = NULL,
    .event_loop_active = FALSE,
    
    .handle_message = function(message_data, message_type = NULL) {
      # Don't process messages if stream is closed
      if (!private$.is_open) {
        return()
      }
      
      tryCatch({
        # Check if message_data is valid
        if (is.null(message_data) || length(message_data) == 0) {
          return()
        }
        
        # Determine message type
        if (is.null(message_type) || length(message_type) == 0) {
          if (!is.null(message_data$Type) && length(message_data$Type) > 0) {
            message_type <- as.character(message_data$Type)[1]
          } else if (!is.null(message_data$type) && length(message_data$type) > 0) {
            message_type <- as.character(message_data$type)[1]
          }
        } else {
          message_type <- as.character(message_type)[1]
        }
        
        debug <- getOption("refinitiv_streaming_debug", FALSE)
        if (debug) {
          message("[DEBUG] Stream._handle_message - message_type: ", message_type)
        }
        
        # Skip if no message type
        if (is.null(message_type) || length(message_type) == 0 || is.na(message_type)) {
          if (debug) message("[DEBUG] No message type, returning")
          return()
        }
        
        # Extract instrument name
        instrument <- private$.extract_instrument(message_data)
        if (is.null(instrument) || length(instrument) == 0 || is.na(instrument) || instrument == "UNKNOWN") {
          # Try to get from definition if we can't extract from message
          universe <- private$.definition$get_universe()
          if (length(universe) > 0 && !is.null(universe[1])) {
            instrument <- as.character(universe[1])
          } else {
            instrument <- "UNKNOWN"
          }
        }
        
        # Ensure instrument is a valid character
        instrument <- as.character(instrument)[1]
        if (is.na(instrument)) {
          instrument <- "UNKNOWN"
        }
        
        # Extract fields data
        fields_data <- private$.extract_fields(message_data)
        debug <- getOption("refinitiv_streaming_debug", FALSE)
        
        if (debug) {
          message("[DEBUG] Stream._handle_message - Instrument: ", instrument, " | Fields count: ", length(fields_data))
        }
        
        # Process based on message type (case-insensitive comparison)
        message_type_upper <- toupper(message_type)
        
        if (debug) {
          message("[DEBUG] Stream._handle_message - message_type: '", message_type, "' (upper: '", message_type_upper, "')")
        }
        
        # Check for empty fields - only skip for REFRESH and UPDATE messages
        # STATUS messages might not have fields but still need processing
        if ((message_type_upper == "REFRESH" || message_type_upper == "UPDATE") && 
            (is.null(fields_data) || length(fields_data) == 0)) {
          if (debug) message("[DEBUG] No fields extracted, skipping message")
          # No fields in this message, skip processing
          return()
        }
        
        if (message_type_upper == "REFRESH") {
          if (debug) message("[DEBUG] Calling .handle_refresh for ", instrument)
          private$.handle_refresh(instrument, fields_data, message_data)
        } else if (message_type_upper == "UPDATE") {
          if (debug) message("[DEBUG] Calling .handle_update for ", instrument)
          private$.handle_update(instrument, fields_data, message_data)
        } else if (message_type_upper == "STATUS") {
          # Check for errors
          if (!is.null(message_data$State) && is.list(message_data$State)) {
            state_stream <- message_data$State$Stream
            state_datastate <- message_data$State$DataState
            
            if (!is.null(state_stream) && length(state_stream) > 0) {
              state_stream <- as.character(state_stream)[1]
              if (state_stream == "Closed" || state_stream == "NonStreaming") {
                private$.handle_error(message_data)
              }
            }
            
            if (!is.null(state_datastate) && length(state_datastate) > 0) {
              state_datastate <- as.character(state_datastate)[1]
              if (state_datastate == "Suspect") {
                private$.handle_error(message_data)
              }
            }
          }
        }
      }, error = function(e) {
        # Suppress common parsing errors that don't indicate real problems
        error_msg <- as.character(e$message)[1]
        if (!grepl("argument is of length zero|subscript out of bounds", error_msg, ignore.case = TRUE)) {
          warning("Error handling message: ", error_msg)
        }
      })
    },
    
    .extract_instrument = function(message_data) {
      if (is.null(message_data) || length(message_data) == 0) {
        return(NULL)
      }
      
      # Helper function to safely extract character value
      safe_extract <- function(obj, default = NULL) {
        if (is.null(obj)) return(default)
        if (length(obj) == 0) return(default)
        val <- as.character(obj)[1]
        if (is.na(val) || nchar(val) == 0) return(default)
        return(val)
      }
      
      # Try different ways to extract instrument name
      if (!is.null(message_data$Key) && is.list(message_data$Key)) {
        name <- safe_extract(message_data$Key$Name)
        if (!is.null(name)) return(name)
      }
      
      if (!is.null(message_data$key) && is.list(message_data$key)) {
        name <- safe_extract(message_data$key$name)
        if (!is.null(name)) return(name)
      }
      
      # Try to get from Fields
      if (!is.null(message_data$Fields) && is.list(message_data$Fields)) {
        name <- safe_extract(message_data$Fields$DSPLY_NAME)
        if (!is.null(name)) return(name)
        
        name <- safe_extract(message_data$Fields$RIC)
        if (!is.null(name)) return(name)
      }
      
      # Try to get from Name field directly
      name <- safe_extract(message_data$Name)
      if (!is.null(name)) return(name)
      
      # Default to first instrument in universe
      universe <- private$.definition$get_universe()
      if (length(universe) > 0 && !is.null(universe[1])) {
        return(as.character(universe[1]))
      }
      
      return(NULL)
    },
    
    .extract_fields = function(message_data) {
      if (is.null(message_data) || length(message_data) == 0) {
        return(list())
      }
      
      # Extract Fields section
      fields <- message_data$Fields
      if (is.null(fields) || length(fields) == 0) {
        fields <- message_data$fields
      }
      
      if (is.null(fields) || length(fields) == 0) {
        return(list())
      }
      
      # Convert to list if it's a data.frame
      if (is.data.frame(fields)) {
        fields <- as.list(fields)
      }
      
      # Ensure it's a list
      if (!is.list(fields)) {
        return(list())
      }
      
      # Remove NULL values
      fields <- fields[!sapply(fields, is.null)]
      
      return(fields)
    },
    
    .handle_refresh = function(instrument, fields_data, message_data) {
      # Update latest data
      if (!is.null(instrument) && length(instrument) > 0 && !is.na(instrument)) {
        instrument <- as.character(instrument)[1]
        private$.latest_data[[instrument]] <- fields_data
        
        # Add to history
        # Create a list with all fields, ensuring they're properly expanded
        row_data <- c(
          list(
            timestamp = Sys.time(),
            instrument = instrument,
            message_type = "Refresh"
          ),
          fields_data  # fields_data is already a list, so this expands it
        )
        
        history_row <- data.table::as.data.table(row_data)
        
        # Initialize data_history if it's empty or NULL
        if (is.null(private$.data_history) || nrow(private$.data_history) == 0) {
          private$.data_history <- history_row
        } else {
          private$.data_history <- data.table::rbindlist(
            list(private$.data_history, history_row),
            fill = TRUE
          )
        }
        
        # Call refresh callbacks
        for (callback in private$.callbacks$refresh) {
          tryCatch({
            callback(self, instrument, fields_data)
          }, error = function(e) {
            warning("Error in refresh callback: ", e$message)
          })
        }
      }
    },
    
    .handle_update = function(instrument, fields_data, message_data) {
      debug <- getOption("refinitiv_streaming_debug", FALSE)
      
      if (is.null(instrument) || length(instrument) == 0 || is.na(instrument)) {
        if (debug) message("[DEBUG] .handle_update: Invalid instrument, returning")
        return()
      }
      
      instrument <- as.character(instrument)[1]
      
      if (debug) {
        message("[DEBUG] .handle_update: Processing for ", instrument, " with ", length(fields_data), " fields")
      }
      
      # Update latest data
      if (instrument %in% names(private$.latest_data)) {
        # Merge with existing data
        existing <- private$.latest_data[[instrument]]
        if (is.list(existing)) {
          for (field in names(fields_data)) {
            existing[[field]] <- fields_data[[field]]
          }
          private$.latest_data[[instrument]] <- existing
        } else {
          private$.latest_data[[instrument]] <- fields_data
        }
      } else {
        private$.latest_data[[instrument]] <- fields_data
      }
      
      if (debug) {
        message("[DEBUG] .handle_update: Latest data updated. History rows before: ", 
                if (is.null(private$.data_history)) 0 else nrow(private$.data_history))
      }
      
      # Initialize data_history if NULL
      if (is.null(private$.data_history)) {
        private$.data_history <- data.table::data.table()
      }
      
      # Add to history
      # Create a list with all fields, ensuring they're properly expanded
      row_data <- c(
        list(
          timestamp = Sys.time(),
          instrument = instrument,
          message_type = "Update"
        ),
        fields_data  # fields_data is already a list, so this expands it
      )
      
      history_row <- data.table::as.data.table(row_data)
      
      if (debug) {
        message("[DEBUG] .handle_update: Created history row with ", ncol(history_row), " columns: ", paste(names(history_row), collapse = ", "))
      }
      
      # Initialize data_history if it's empty or NULL
      if (is.null(private$.data_history) || nrow(private$.data_history) == 0) {
        private$.data_history <- history_row
      } else {
        private$.data_history <- data.table::rbindlist(
          list(private$.data_history, history_row),
          fill = TRUE
        )
      }
      
      if (debug) {
        message("[DEBUG] .handle_update: History rows after: ", nrow(private$.data_history))
      }
      
      # Call update callbacks
      if (debug) {
        message("[DEBUG] .handle_update: Calling ", length(private$.callbacks$update), " update callback(s)")
      }
      
      for (callback in private$.callbacks$update) {
        tryCatch({
          callback(self, instrument, fields_data)
        }, error = function(e) {
          warning("Error in update callback: ", e$message)
        })
      }
    },
    
    .handle_error = function(error_data) {
      error_message <- "Unknown error"
      if (!is.null(error_data) && is.list(error_data)) {
        if (!is.null(error_data$message)) {
          error_message <- error_data$message
        } else if (!is.null(error_data$Text)) {
          error_message <- error_data$Text
        }
      }
      
      # Call error callbacks
      for (callback in private$.callbacks$error) {
        tryCatch({
          callback(self, error_message)
        }, error = function(e) {
          warning("Error in error callback: ", e$message)
        })
      }
    },
    
    .start_event_loop = function() {
      # This function ensures the event loop runs periodically
      # so WebSocket messages are processed
      if (!requireNamespace("later", quietly = TRUE)) {
        return()
      }
      
      # Schedule periodic event processing
      private$.event_loop_active <- TRUE
      private$.schedule_next_event_loop()
    },
    
    .schedule_next_event_loop = function() {
      if (!private$.is_open || !private$.event_loop_active) {
        return()
      }
      
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          if (private$.is_open && private$.event_loop_active) {
            # Run event loop to process any pending messages
            later::run_now()
            # Schedule next run
            private$.schedule_next_event_loop()
          }
        }, delay = 0.1)
      }
    }
  )
)

