# Important Messages

You can now use both Eikon and Workspace. The package only supports a
desktop session, not platform sessions. This is theoretically possible
but currently not implemented.

1.  **Python is now optional** - The package can send direct JSON
    messages to the terminal.
2.  **When using Python libraries** - If you update the package, also
    run `Refinitiv::install_eikon()` again.
3.  **Python Eikon and RDP libraries are deprecated** - Can no longer be
    used. The current commit is reverse compatible with previous
    commits.

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR/graph/badge.svg)](https://app.codecov.io/gh/GreenGrassBlueOcean/RefinitivR)
[![R build
status](https://github.com/GreenGrassBlueOcean/RefinitivR/workflows/R-CMD-check/badge.svg)](https://github.com/GreenGrassBlueOcean/RefinitivR/actions)
<!-- badges: end -->

## Overview

**RefinitivR** is an R interface to Refinitiv Eikon and Refinitiv
DataStream. It provides:

-   **Direct JSON API access** (no Python required)
-   **Python API support** via reticulate (optional)
-   **Real-time streaming data** with WebSocket support ⭐ NEW
-   **DataStream integration** via DatastreamDSWS2R
-   **Robust error handling** with automatic retries and chunking

This package is in no way affiliated with Thomson Reuters, Refinitiv,
Eikon, Datastream, or LSEG. A subscription to Eikon and Datastream is
required. Use at your own risk!

## Installation

``` r
install.packages("devtools")
devtools::install_github("GreenGrassBlueOcean/RefinitivR")
library(Refinitiv)
```

## Quick Start

### Basic Connection (JSON - No Python Required)

``` r
# Connect to Refinitiv Data Platform
RD <- RDConnect(application_id = "YOUR API KEY", PythonModule = "JSON")

# Get historical data
data <- rd_GetHistory(RDObject = RD, universe = c("AAPL.O", "MSFT.O"))
```

### Real-Time Streaming ⭐ NEW

``` r
# Create a streaming connection
stream <- rd_get_streaming_data(
  universe = "EUR=",
  fields = c("BID", "ASK", "OPEN_PRC")
)

# Add callbacks for real-time updates
stream$on_update(function(stream, instrument, fields) {
  cat(sprintf("%s: BID=%s, ASK=%s\n", instrument, fields$BID, fields$ASK))
})

# Open stream and collect data
stream$open()
Sys.sleep(10)  # Collect for 10 seconds

# View results
print(stream$get_latest_data())
print(stream$get_summary())

# Close stream
stream$close()
```

------------------------------------------------------------------------

## Table of Contents

1.  [Connecting to Refinitiv](#connecting-to-refinitiv)
2.  [Real-Time Streaming](#real-time-streaming) ⭐ NEW
3.  [Working with Refinitiv Data (RD)](#working-with-refinitiv-data-rd)
4.  [Legacy Eikon Functions](#working-with-the-legacy-eikon-functions)
5.  [DataStream](#datastream)
6.  [Custom Instruments](#custom-instruments)
7.  [Building Custom Visualizations](#building-custom-visualizations) ⭐
    NEW

------------------------------------------------------------------------

## Connecting to Refinitiv

### Direct JSON Method (Recommended - No Python Required)

``` r
# Refinitiv Data Platform
RD <- RDConnect(application_id = "YOUR API KEY", PythonModule = "JSON")

# Eikon (Legacy)
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "JSON")
```

### Python API Method (Optional)

First install the Python environment:

``` r
# Run RStudio with elevated permissions/Administrator
Refinitiv::install_eikon()
```

Then connect:

``` r
RD <- RDConnect(application_id = "YOUR API KEY", PythonModule = "RD")
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "RD")
```

**Note:** Make sure Eikon/LSEG Workspace is running and online before
connecting.

------------------------------------------------------------------------

## Real-Time Streaming ⭐ NEW

The package now includes a powerful real-time streaming API for live
market data using WebSocket connections.

### Prerequisites

``` r
# Required packages (installed automatically)
install.packages(c("websocket", "later", "shiny", "data.table"))
```

### Quick Example

``` r
library(Refinitiv)

# Create stream
stream <- rd_get_streaming_data(
  universe = "EUR=",
  fields = c("BID", "ASK")
)

# Add update callback
stream$on_update(function(stream, instrument, fields) {
  cat(sprintf("[%s] BID: %s | ASK: %s\n", 
              instrument, fields$BID, fields$ASK))
})

# Open and collect data
stream$open()
Sys.sleep(10)
stream$close()
```

### Multiple Instruments

``` r
stream <- rd_get_streaming_data(
  universe = c("EUR=", "GBP=", "JPY="),
  fields = c("BID", "ASK", "OPEN_PRC", "DSPLY_NAME")
)
```

### Event Callbacks

``` r
# Refresh callback - called when initial data is received
stream$on_refresh(function(stream, instrument, fields) {
  cat(sprintf("Initial data for %s received\n", instrument))
})

# Update callback - called on every price update
stream$on_update(function(stream, instrument, fields) {
  # Process update
})

# Error callback - called on errors
stream$on_error(function(stream, error_message) {
  warning("Stream error: ", error_message)
})
```

### Accessing Data

``` r
# Latest snapshot (all instruments)
latest <- stream$get_latest_data()

# Latest for specific instrument
eur_data <- stream$get_latest_data("EUR=")

# Full history (data.table)
history <- stream$get_data_history()

# Summary statistics
summary <- stream$get_summary()
summary_eur <- stream$get_summary(instrument = "EUR=")
```

### Built-in Live Plotting

``` r
# Create and open live plot
app <- stream$plot_live(field = "BID", instrument = "EUR=")
shiny::runApp(app)
```

### Advanced Usage

``` r
# Enable debug logging
options(refinitiv_streaming_debug = TRUE)

# Create stream with custom manager
def <- rd_streaming_pricing$Definition$new(
  universe = "EUR=",
  fields = c("BID", "ASK")
)
stream <- def$get_stream()
stream$open()
```

### Examples

Run the included examples:

``` r
# Quick example (~10 seconds)
source(system.file("examples", "streaming_quick_example.R", package = "Refinitiv"))

# Complete example (~30 seconds, all features)
source(system.file("examples", "streaming_complete_example.R", package = "Refinitiv"))

# Debug example (~15 seconds, with logging)
source(system.file("examples", "streaming_test_debug.R", package = "Refinitiv"))
```

Or use the helper script:

``` r
source("run_examples.R")  # Interactive menu
```

------------------------------------------------------------------------

## Working with Refinitiv Data (RD)

### Searching

``` r
# Basic search
results <- RDPsearch(RD, query = "AAPL.O")

# Advanced search with filters
Presidents <- RDPsearch(
  RD, 
  view = "People", 
  query = 'president',
  filter = "startswith(LastName,'H')",
  select = 'DocumentTitle',
  order_by = 'DocumentTitle asc',
  top = 20
)
```

### Get History

``` r
# Simple timeseries
timeseries <- rd_GetHistory(
  RDObject = RD, 
  universe = c("AAPL.O", "NVDA.O")
)

# With fields and parameters
data <- rd_GetHistory(
  RDObject = RD, 
  universe = c("GOOG.O", "AAPL.O"),
  fields = c("TR.Revenue", "TR.GrossProfit"),
  parameters = list("SDate" = "0CY", "Curn" = "CAD")
)
```

### Get Data

``` r
# Basic data request
ex1 <- rd_GetData(
  RDObject = RD, 
  rics = c("MMM", "III.L"),
  Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", 
                     "TR.CompanyName")
)
```

### Historical Pricing

``` r
# Daily data
daily <- rd_GetHistoricalPricing(
  universe = "VOD.L", 
  interval = "P1D",
  count = 20L, 
  RDObject = RD
)

# Intraday (1-minute)
intraday <- rd_GetHistoricalPricing(
  universe = c("VOD.L", "AAPL.O"),
  interval = "PT1M", 
  count = 500L,
  sessions = c("pre", "normal", "post"),
  RDObject = RD
)
```

------------------------------------------------------------------------

## Working with the Legacy Eikon Functions

### Connecting

``` r
Eikon <- EikonConnect(
  Eikonapplication_id = "YOUR EIKON API KEY", 
  PythonModule = "JSON"
)
```

### News

``` r
# Get headlines
headlines <- EikonGetNewsHeadlines(
  EikonObject = Eikon,
  query = "R:MSFT.O", 
  count = 2
)

# Get story
stories <- EikonGetNewsStory(
  story_id = headlines$storyId, 
  EikonObject = Eikon
)
```

### Timeseries

``` r
Timeseries <- EikonGetTimeseries(
  EikonObject = Eikon, 
  rics = c("MMM", "III.L"),
  start_date = "2020-01-01T01:00:00",
  end_date = paste0(Sys.Date(), "T01:00:00")
)
```

### Data Requests

``` r
Data <- EikonGetData(
  EikonObject = Eikon, 
  rics = c("MMM", "III.L"),
  Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", 
                     "TR.CompanyName")
)
```

**Note:** For large integers (e.g., market cap), see the section on
[retrieving large
integers](#retrieving-large-integers-with-eikongetdata-python-only-not-for-json-method)
below.

### Retrieving Large Integers with EikonGetData (Python only, not for JSON method)

There is currently an issue with the reticulate package handling large
integers (like e.g. market capatilization) from python to r. This issue
is described [here](https://github.com/rstudio/reticulate/issues/323)
and try for yourself
[here](https://community.rstudio.com/t/large-integer-conversion-from-python-to-r/82568)

This leads to the following behaviour:

``` r
ex2 <- EikonGetData( EikonObject = Eikon, rics = "AAPl.O"
                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
                    )
```

ex2 will return `-1` for the Market capitalization. Which can never be
the correct number.

A workaround is to scale back the expected output to a smaller number.
By expressing the market capatalization in millions the produced integer
becomes smaller and so prevents the integer of becoming too large. This
can be done by adding a named list in the parameters field:
`Parameters = list("scale" = 6)` in which the `6` stands for millions.

``` r
ex2a <- EikonGetData( EikonObject = Eikon, rics = "AAPl.O"
                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
                    , Parameters = list("scale" = 6)
                    )
```

Or for more complex formula’s scale back in the formula itself by adding
`scale = 6` to `TR.CompanyMarketCap`

``` r
ex2b <- EikonGetData( EikonObject = Eikon, rics = "AAPl.O"
                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D, scale=6)/*Market Cap*/"
                    )
```

------------------------------------------------------------------------

## Custom Instruments

Custom instruments can only be created using the JSON connection method.

### Setup

``` r
# Get UUID from Eikon Terminal: Help → About → User Details
RD <- RDConnect(
  application_id = NA, 
  PythonModule = "JSON", 
  UUID = "ABCDE-123456"
)
```

### Create Simple Instrument

``` r
# Create
rd_ManageCustomInstruments(
  operation = "CREATE", 
  symbol = "testAAPLandAMZN",
  formula = "AAPL.O + AMZN.O"
)

# Get details
rd_ManageCustomInstruments(
  operation = "GET", 
  symbol = "testAAPLandAMZN"
)

# Update
rd_ManageCustomInstruments(
  operation = "UPDATE", 
  symbol = "testAAPLandAMZN",
  formula = "AAPL.O + 2 * AMZN.O"
)

# Delete
rd_ManageCustomInstruments(
  operation = "DELETE", 
  symbol = "testAAPLandAMZN"
)
```

### Basket Instruments

``` r
# Build basket
basket <- CustomInstrumentBasketBuilder(
  RICs = c("AAPL.O", "AMZN.O"), 
  Weights = c(0.5, 0.5)
)

# Create basket instrument
rd_ManageCustomInstruments(
  operation = "CREATE", 
  symbol = "InterestingBasket",
  basket = basket, 
  currency = "USD"
)

# Add holidays
holidays <- CustomInstrumentHolidayBuilder(
  dates = c("2023-12-01", "2023-12-31"),
  reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2")
)

rd_ManageCustomInstruments(
  operation = "UPDATE", 
  symbol = "InterestingBasket",
  holidays = holidays
)
```

### Utilities

``` r
# List all custom instruments
AllActiveCustomInstruments <- rd_SearchCustomInstruments()

# Get official RIC name
RealInstrumentName <- CorrectCustomInstrument("InterestingBasket")
```

------------------------------------------------------------------------

## DataStream

``` r
DatastreamUserName <- "Your datastream username"
DatastreamPassword <- "Your datastream password"
DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)

DSResult <- DataStream$snapshotRequest(
  instrument = c("ABF", "RIO", "WPP"),
  datatype = "P",
  requestDate = "0D"
)
```

For further details, see
[DatastreamDSWS2R](https://github.com/CharlesCara/DatastreamDSWS2R).

------------------------------------------------------------------------

## Building Custom Visualizations ⭐ NEW

The streaming API provides several ways to build custom visualizations:

### Method 1: Using Built-in `plot_live()`

The simplest approach uses the built-in Shiny app:

``` r
stream <- rd_get_streaming_data(universe = "EUR=", fields = c("BID", "ASK"))
stream$open()

# Create and run plot
app <- stream$plot_live(field = "BID", instrument = "EUR=")
shiny::runApp(app)
```

**Requirements:** - `shiny` package: `install.packages("shiny")` -
Stream must be open and receiving data

### Method 2: Custom Shiny App

Build your own Shiny app using stream data:

``` r
library(shiny)
library(Refinitiv)

# Create stream
stream <- rd_get_streaming_data(universe = "EUR=", fields = c("BID", "ASK"))
stream$open()

# Custom UI
ui <- fluidPage(
  titlePanel("Custom Live Stream"),
  plotOutput("live_plot"),
  verbatimTextOutput("stats")
)

# Custom Server
server <- function(input, output, session) {
  # Update plot every second
  output$live_plot <- renderPlot({
    invalidateLater(1000, session)
    
    history <- stream$get_data_history()
    if (nrow(history) > 0) {
      plot(history$timestamp, history$BID, 
           type = "l", xlab = "Time", ylab = "BID",
           main = "EUR/USD Bid Price")
    }
  })
  
  output$stats <- renderText({
    latest <- stream$get_latest_data("EUR=")
    paste("Latest BID:", latest$BID)
  })
}

# Run app
shinyApp(ui, server)
```

### Method 3: Using `ggplot2` for Static Plots

Create static plots from history data:

``` r
library(ggplot2)

stream <- rd_get_streaming_data(universe = "EUR=", fields = c("BID", "ASK"))
stream$open()
Sys.sleep(30)  # Collect data
stream$close()

# Get history
history <- stream$get_data_history()

# Create plot
ggplot(history, aes(x = timestamp, y = BID)) +
  geom_line() +
  labs(title = "EUR/USD Bid Price Over Time",
       x = "Time", y = "BID Price") +
  theme_minimal()
```

### Method 4: Real-Time Updates with `plotly`

For interactive plots:

``` r
library(plotly)

stream <- rd_get_streaming_data(universe = "EUR=", fields = c("BID", "ASK"))
stream$open()

# Create initial plot
p <- plot_ly(x = numeric(0), y = numeric(0), type = "scatter", mode = "lines")

# Update callback
stream$on_update(function(stream, instrument, fields) {
  history <- stream$get_data_history()
  if (nrow(history) > 0) {
    # Update plotly plot
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("extendTraces", 
                        list(y = list(list(fields$BID))),
                        list(0))
  }
})
```

### Method 5: Accessing Data Programmatically

For maximum flexibility, access data directly:

``` r
# Get latest snapshot
latest <- stream$get_latest_data()

# Get full history (data.table)
history <- stream$get_data_history()

# Get summary statistics
summary <- stream$get_summary()

# Filter history
recent <- history[timestamp > Sys.time() - 3600]  # Last hour
```

### Visualization Dependencies

**For built-in plotting:** - `shiny` - Required for `plot_live()` -
`data.table` - For data history (already included)

**For custom visualizations:** - `shiny` - For interactive web apps -
`ggplot2` - For static plots - `plotly` - For interactive plots
(optional) - `dygraphs` - For time series plots (optional)

Install as needed:

``` r
install.packages(c("shiny", "ggplot2", "plotly", "dygraphs"))
```

### Best Practices

1.  **Always close streams** when done to free resources
2.  **Use callbacks** for real-time updates instead of polling
3.  **Filter history** for large datasets before plotting
4.  **Handle errors** in callbacks to prevent crashes
5.  **Use `later::run_now()`** in long-running scripts to process events

------------------------------------------------------------------------

## Examples

Example scripts are included in the package:

``` r
# List available examples
list.files(system.file("examples", package = "Refinitiv"))

# Run quick example
source(system.file("examples", "streaming_quick_example.R", package = "Refinitiv"))
```

Or use the interactive runner:

``` r
source("run_examples.R")
```

------------------------------------------------------------------------

## License

See LICENSE file.

------------------------------------------------------------------------

## Acknowledgments

-   Uses
    [DatastreamDSWS2R](https://github.com/CharlesCara/DatastreamDSWS2R)
    for DataStream connections
-   Built for stability and robustness with automatic retries and
    chunking
