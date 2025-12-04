# Get streaming data from Refinitiv

High-level function to create and configure a streaming data connection.
This function provides a simple interface for real-time market data
streaming with event-driven callbacks.

## Usage

``` r
rd_get_streaming_data(
  universe,
  fields,
  on_refresh = NULL,
  on_update = NULL,
  on_error = NULL,
  RDObject = NULL,
  stream_type = "pricing",
  domain = "MarketPrice",
  parameters = NULL
)
```

## Arguments

- universe:

  Character vector of instrument RICs to stream

- fields:

  Character vector of field names to retrieve

- on_refresh:

  Optional function to call on refresh events. Function signature:
  \`function(stream, instrument, fields)\`

- on_update:

  Optional function to call on update events. Function signature:
  \`function(stream, instrument, fields)\`

- on_error:

  Optional function to call on error events. Function signature:
  \`function(stream, error_message)\`

- RDObject:

  Optional RD connection object (uses active session if not provided)

- stream_type:

  Type of stream ("pricing" or "analytics", default: "pricing")

- domain:

  Domain for OMM streams (default: "MarketPrice")

- parameters:

  Optional parameters list

## Value

Stream object with methods: - \`open()\` - Start streaming -
\`close()\` - Stop streaming - \`on_refresh(callback)\` - Register
refresh callback - \`on_update(callback)\` - Register update callback -
\`on_error(callback)\` - Register error callback -
\`get_latest_data(instrument)\` - Get current snapshot -
\`get_data_history()\` - Get buffered historical data -
\`plot_live(field, instrument, ...)\` - Create live Shiny plot -
\`get_summary(instrument)\` - Get summary statistics -
\`clear_history()\` - Clear data history buffer - \`is_open()\` - Check
if stream is open

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple usage with callbacks
stream <- rd_get_streaming_data(
  universe = c("EUR=", "GBP=", "JPY="),
  fields = c("BID", "ASK", "DSPLY_NAME"),
  on_update = function(stream, instrument, fields) {
    cat("Update for", instrument, ":\n")
    print(fields)
  }
)

# Start streaming
stream$open()

# Later: stop streaming
stream$close()
} # }

if (FALSE) { # \dontrun{
# With session integration
RD <- RDConnect(application_id = "your_key")
stream <- rd_get_streaming_data(
  universe = "AAPL.O",
  fields = c("BID", "ASK"),
  RDObject = RD
)
stream$open()
} # }

if (FALSE) { # \dontrun{
# Live plotting
stream <- rd_get_streaming_data(
  universe = "EUR=",
  fields = c("BID", "ASK")
)
stream$open()

# Create and run live plot
app <- stream$plot_live(field = "BID")
shiny::runApp(app)  # Opens in browser
} # }

if (FALSE) { # \dontrun{
# Get summary statistics
stream <- rd_get_streaming_data(
  universe = c("EUR=", "GBP="),
  fields = c("BID", "ASK")
)
stream$open()
Sys.sleep(10)  # Collect some data
summary <- stream$get_summary()
print(summary)
} # }
```
