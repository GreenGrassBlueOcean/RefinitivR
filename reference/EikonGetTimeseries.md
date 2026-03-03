# Function to obtain timeseries from Eikon via the JSON API

Automatically chunks the timeseries in seperate apicalls and binds them
together again in order to comply with api regulations.

## Usage

``` r
EikonGetTimeseries(
  EikonObject = rd_connection(),
  rics,
  interval = "daily",
  calender = "tradingdays",
  corax = "adjusted",
  fields = c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE"),
  start_date = "2020-01-01T01:00:00",
  end_date = paste0(Sys.Date(), "T01:00:00"),
  cast = TRUE,
  time_out = 60,
  verbose = FALSE,
  raw_output = FALSE,
  cache = NULL
)
```

## Arguments

- EikonObject:

  Eikon connection object. Defaults to
  [`rd_connection()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_connection.md),
  which auto-creates a connection on first use.

- rics:

  a vector containing Reuters rics

- interval:

  Data interval. Possible values: 'tick', 'minute', 'hour', 'daily',
  'weekly', 'monthly', 'quarterly', 'yearly' Default: 'daily'

- calender:

  Possible values: 'native', 'tradingdays', 'calendardays'., Default:
  'tradingdays'

- corax:

  possible values 'adjusted', 'unadjusted'. Default: 'adjusted'

- fields:

  a vector containing any combination ('TIMESTAMP', 'VOLUME', 'HIGH',
  'LOW', 'OPEN', 'CLOSE')

- start_date:

  Starting date and time of the historical range. string format is:
  '%Y-%m-%dT%H:%M:%S'.

- end_date:

  End date and time of the historical range. string format is:
  '%Y-%m-%dT%H:%M:%S'.

- cast:

  cast data from wide to long format using the data.table::dcast
  function, Default: TRUE

- time_out:

  set the maximum timeout to the Eikon server, default = 60

- verbose:

  boolean if TRUE prints out the API call details to the console

- raw_output:

  provide only the raw downloaded info from Eikon

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (300 s). `FALSE` disables caching. A positive numeric
  value sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

A data.frame containing time series from Eikon

## References

<https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item>

## Examples

``` r
if (FALSE) { # \dontrun{
Eikon <- Refinitiv::EikonConnect()
ex1 <- EikonGetTimeseries(
  EikonObject = Eikon, rics = c("MMM", "III.L"),
  start_date = "2020-01-01T01:00:00",
  end_date = paste0(Sys.Date(), "T01:00:00"), verbose = TRUE
)
} # }

if (FALSE) { # \dontrun{
EikonJson <- RefinitivJsonConnect()
ex1 <- EikonGetTimeseries(
  EikonObject = EikonJson, rics = c("MMM", "III.L"),
  start_date = "2020-01-01T01:00:00",
  end_date = paste0(Sys.Date(), "T01:00:00"), verbose = TRUE
)
} # }
```
