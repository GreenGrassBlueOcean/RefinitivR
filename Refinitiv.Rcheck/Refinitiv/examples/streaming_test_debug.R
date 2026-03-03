# Streaming Test with Debug Logging
# 
# Simple test script with debug logging enabled to diagnose issues

library(Refinitiv)

# Enable debug logging
options(refinitiv_streaming_debug = TRUE)

cat("\n=== Streaming Test with Debug Logging ===\n\n")

# Create stream
cat("Creating stream for EUR=...\n")
stream <- rd_get_streaming_data(
  universe = "EUR=",
  fields = c("BID", "ASK")
)

# Add callbacks
cat("Registering callbacks...\n")
update_env <- new.env(parent = emptyenv())
update_env$count <- 0L
stream$on_update(function(stream, instrument, fields) {
  update_env$count <- update_env$count + 1L
  if (update_env$count %% 5 == 0) {
    bid_val <- if (!is.null(fields$BID)) fields$BID else "N/A"
    ask_val <- if (!is.null(fields$ASK)) fields$ASK else "N/A"
    cat(sprintf("[UPDATE #%d] %s - BID: %s | ASK: %s\n", 
                update_env$count, instrument, bid_val, ask_val))
  }
})

stream$on_refresh(function(stream, instrument, fields) {
  cat(sprintf("[REFRESH] %s - Initial data received\n", instrument))
})

# Open stream
cat("\nOpening stream...\n")
cat("(Watch for debug messages showing login and subscription)\n\n")
stream$open()

# Wait for data
cat("\nWaiting 15 seconds for data...\n")
for (i in 1:15) {
  Sys.sleep(1)
  history <- stream$get_data_history()
  cat(sprintf("  [%2ds] Updates: %d | History rows: %d\n", 
              i, update_env$count, nrow(history)))
}

# Show results
cat("\n=== Results ===\n")
cat(sprintf("Total updates received: %d\n", update_env$count))
cat(sprintf("History rows: %d\n", nrow(stream$get_data_history())))
cat(sprintf("Latest data:\n"))
print(stream$get_latest_data())

# Close
stream$close()
cat("\nDone!\n")

