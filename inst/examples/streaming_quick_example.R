# Quick Streaming Example
# 
# A simple example to quickly test streaming functionality
# Run this to verify streaming is working correctly

library(Refinitiv)

cat("\n=== Quick Streaming Test ===\n\n")

# Create stream for EUR/USD
cat("Creating stream for EUR=...\n")
stream <- rd_get_streaming_data(
  universe = "EUR=",
  fields = c("BID", "ASK")
)

# Add update callback
stream$on_update(function(stream, instrument, fields) {
  bid_val <- if (!is.null(fields$BID)) fields$BID else "N/A"
  ask_val <- if (!is.null(fields$ASK)) fields$ASK else "N/A"
  cat(sprintf("[%s] BID: %s | ASK: %s\n", instrument, bid_val, ask_val))
})

# Open stream
cat("Opening stream...\n")
stream$open()

# Collect data for 10 seconds
cat("Collecting data for 10 seconds...\n")
Sys.sleep(10)

# Show results
cat("\n=== Results ===\n")
cat(sprintf("Total updates: %d\n", nrow(stream$get_data_history())))
cat(sprintf("Latest data:\n"))
print(stream$get_latest_data())

# Optional: Open live plot
cat("\nOpening live plot (close browser when done)...\n")
app <- stream$plot_live(field = "BID")
shiny::runApp(app)

# Close stream
stream$close()
cat("\nDone!\n")

