# Complete Streaming Example - Testing All Features
# 
# This example demonstrates all streaming functionality:
# - Connection and authentication
# - Event callbacks (refresh, update, error)
# - Data access (latest data, history)
# - Live plotting
# - Summary statistics
# - Multiple instruments
#
# Prerequisites:
# - Eikon/LSEG Workspace must be running
# - Valid API key configured
# - websocket and later packages installed

library(Refinitiv)

# ============================================================================
# SETUP
# ============================================================================

cat("\n")
cat(paste0("=", paste(rep("=", 70), collapse = ""), "=\n"))
cat("  REFINITIV STREAMING - COMPLETE EXAMPLE\n")
cat(paste0("=", paste(rep("=", 70), collapse = ""), "=\n"))
cat("\n")

# Enable debug logging
options(refinitiv_streaming_debug = TRUE)
cat("Debug logging enabled\n")

# ============================================================================
# STEP 1: Create Stream with Multiple Instruments
# ============================================================================

cat("\n[STEP 1] Creating stream for EUR=, GBP=, JPY=\n")
cat("  Fields: BID, ASK, OPEN_PRC\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

stream <- rd_get_streaming_data(
  universe = c("EUR=", "GBP=", "JPY="),
  fields = c("BID", "ASK", "OPEN_PRC", "DSPLY_NAME")
)

cat("✓ Stream created successfully\n")

# ============================================================================
# STEP 2: Register Event Callbacks
# ============================================================================

cat("\n[STEP 2] Registering event callbacks\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

# Track statistics using environment (avoids global assignment)
stats_env <- new.env(parent = emptyenv())
stats_env$update_count <- 0L
stats_env$refresh_count <- 0L
stats_env$error_count <- 0L
stats_env$last_values <- list()

# Refresh callback - called when initial data is received
stream$on_refresh(function(stream, instrument, fields) {
  stats_env$refresh_count <- stats_env$refresh_count + 1L
  cat(sprintf("[REFRESH #%d] %s\n", stats_env$refresh_count, instrument))
  if (!is.null(fields$DSPLY_NAME)) {
    cat(sprintf("  Display Name: %s\n", fields$DSPLY_NAME))
  }
  if (!is.null(fields$BID) && !is.null(fields$ASK)) {
    cat(sprintf("  BID: %s | ASK: %s | Spread: %s\n", 
                fields$BID, fields$ASK, 
                round(as.numeric(fields$ASK) - as.numeric(fields$BID), 5)))
  }
  stats_env$last_values[[instrument]] <- fields
})

# Update callback - called on every price update
stream$on_update(function(stream, instrument, fields) {
  stats_env$update_count <- stats_env$update_count + 1L
  
  # Only print every 10th update to avoid spam
  if (stats_env$update_count %% 10 == 0) {
    cat(sprintf("[UPDATE #%d] %s", stats_env$update_count, instrument))
    if (!is.null(fields$BID)) {
      cat(sprintf(" | BID: %s", fields$BID))
    }
    if (!is.null(fields$ASK)) {
      cat(sprintf(" | ASK: %s", fields$ASK))
    }
    cat("\n")
  }
  
  # Update last values
  if (instrument %in% names(stats_env$last_values)) {
    # Merge with existing
    for (field in names(fields)) {
      stats_env$last_values[[instrument]][[field]] <- fields[[field]]
    }
  } else {
    stats_env$last_values[[instrument]] <- fields
  }
})

# Error callback
stream$on_error(function(stream, error_message) {
  stats_env$error_count <- stats_env$error_count + 1L
  cat(sprintf("[ERROR #%d] %s\n", stats_env$error_count, error_message))
})

cat("✓ Callbacks registered:\n")
cat("  - on_refresh: Initial data handler\n")
cat("  - on_update: Price update handler (prints every 10th update)\n")
cat("  - on_error: Error handler\n")

# ============================================================================
# STEP 3: Open Stream and Collect Data
# ============================================================================

cat("\n[STEP 3] Opening stream and collecting data...\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

# Debug mode is already enabled at the top of the script
stream$open()
cat("✓ Stream opened\n")

# Give a moment for subscription to be sent and processed
Sys.sleep(2)

# Wait for data to accumulate
# Use later::run_now() instead of Sys.sleep() to allow event loop to process messages
cat("\nCollecting data for 10 seconds...\n")
if (requireNamespace("later", quietly = TRUE)) {
  for (i in 1:10) {
    # Process events for 1 second by calling run_now() repeatedly
    start_time <- Sys.time()
    while (as.numeric(Sys.time() - start_time, units = "secs") < 1) {
      later::run_now(0.1)  # Process events, wait up to 0.1 seconds
      Sys.sleep(0.05)  # Small sleep to avoid busy-waiting
    }
    history <- stream$get_data_history()
    cat(sprintf("  [%ds] Updates received: %d | History rows: %d\n", 
                i, stats_env$update_count, nrow(history)))
  }
} else {
  # Fallback if later is not available
  for (i in 1:10) {
    Sys.sleep(1)
    history <- stream$get_data_history()
    cat(sprintf("  [%ds] Updates received: %d | History rows: %d\n", 
                i, stats_env$update_count, nrow(history)))
  }
}

# ============================================================================
# STEP 4: Display Latest Data
# ============================================================================

cat("\n[STEP 4] Latest Data Snapshot\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

latest_data <- stream$get_latest_data()
for (instrument in names(latest_data)) {
  cat(sprintf("\n%s:\n", instrument))
  data <- latest_data[[instrument]]
  for (field in names(data)) {
    if (!field %in% c("timestamp", "instrument", "message_type")) {
      cat(sprintf("  %s: %s\n", field, data[[field]]))
    }
  }
}

# ============================================================================
# STEP 5: Display Summary Statistics
# ============================================================================

cat("\n[STEP 5] Summary Statistics\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

# Overall summary
overall_summary <- stream$get_summary()
cat("\nOverall Summary:\n")
print(overall_summary)

# Per-instrument summaries
for (instrument in c("EUR=", "GBP=", "JPY=")) {
  cat(sprintf("\n%s Summary:\n", instrument))
  inst_summary <- stream$get_summary(instrument = instrument)
  print(inst_summary)
}

# ============================================================================
# STEP 6: Display Data History Sample
# ============================================================================

cat("\n[STEP 6] Data History Sample (Last 5 Updates)\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

history <- stream$get_data_history()
if (nrow(history) > 0) {
  cat(sprintf("Total history rows: %d\n", nrow(history)))
  cat("\nLast 5 updates:\n")
  print(tail(history, 5))
} else {
  cat("No history data available\n")
}

# ============================================================================
# STEP 7: Test Live Plotting
# ============================================================================

cat("\n[STEP 7] Live Plotting\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))
cat("Creating live plot for EUR= BID...\n")
cat("(Plot will open in browser - close browser window when done)\n")

# Create plot for EUR= BID
plot_app <- stream$plot_live(
  field = "BID",
  instrument = "EUR=",
  xrange = 50,
  update_interval = 1000  # Update every second
)

cat("\n✓ Plot app created\n")
cat("Opening Shiny app...\n")
cat("(You can interact with the plot controls in the sidebar)\n")
cat("(Close the browser window when you're done viewing)\n\n")

# Run the app (this will block until browser is closed)
shiny::runApp(plot_app)

cat("\nPlot closed\n")

# ============================================================================
# STEP 8: Final Statistics
# ============================================================================

cat("\n[STEP 8] Final Statistics\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

cat(sprintf("Total Refresh events: %d\n", stats_env$refresh_count))
cat(sprintf("Total Update events: %d\n", stats_env$update_count))
cat(sprintf("Total Error events: %d\n", stats_env$error_count))
cat(sprintf("Stream is open: %s\n", stream$is_open()))
cat(sprintf("Total history rows: %d\n", nrow(stream$get_data_history())))

# ============================================================================
# STEP 9: Cleanup
# ============================================================================

cat("\n[STEP 9] Cleanup\n")
cat(paste0("-", paste(rep("-", 70), collapse = ""), "-\n"))

stream$close()
cat("✓ Stream closed\n")

cat("\n")
cat(paste0("=", paste(rep("=", 70), collapse = ""), "=\n"))
cat("  EXAMPLE COMPLETE - ALL FEATURES TESTED\n")
cat(paste0("=", paste(rep("=", 70), collapse = ""), "=\n"))
cat("\n")

