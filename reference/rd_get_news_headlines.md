# Retrieve News Headlines from a Refinitiv RDP (JSON) Connection

This function constructs an HTTP GET query to retrieve news headlines
from the Refinitiv RDP service. It builds a query string from the
provided parameters using
[`build_get_query_string()`](https://greengrassblueocean.github.io/RefinitivR/reference/build_get_query_string.md)
and then sends the GET request via `send_json_request()`.

## Usage

``` r
rd_get_news_headlines(
  RDObject = rd_connection(),
  query = NULL,
  limit = 10L,
  sort = "NewToOld",
  relevancy = "All",
  cursor = NULL,
  dateFrom = NULL,
  dateTo = NULL,
  raw_output = FALSE,
  debug = TRUE,
  cache = NULL
)
```

## Arguments

- RDObject:

  A connection object returned by
  [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md).
  Defaults to
  [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md)
  if not supplied.

- query:

  A character string (or vector) representing the search query.

- limit:

  An integer indicating the maximum number of headlines to retrieve.
  Maximum allowed value is 100.

- sort:

  An optional sort order (e.g. `"NewToOld"`). If not specified, the
  service default is used.

- relevancy:

  An optional relevancy filter (e.g. `"All"` or `"High"`).

- cursor:

  An optional pagination cursor.

- dateFrom:

  An optional start date/time (ISO 8601 format).

- dateTo:

  An optional end date/time (ISO 8601 format).

- raw_output:

  If `TRUE`, returns the raw JSON response; otherwise, the response is
  flattened into a `data.frame`.

- debug:

  If `TRUE`, prints debugging messages.

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (60 s). `FALSE` disables caching. A positive numeric value
  sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

A `data.frame` with flattened fields, or the raw JSON if
`raw_output = TRUE`.

## Details

The query parameters may include various tokens such as Reuters
Instrument Codes (RIC), language codes, explicit free-text tokens (using
quotes), and date ranges. In addition, the function supports pagination
via a cursor.

Examples of queries include:

- Explicit FreeText (use quotes):

  `rd_get_news_headlines(query = "\"electric car\" or \"electric vehicle\"")`

- SearchIn token (HeadlineOnly):

  `rd_get_news_headlines(query = "\"Reports\" or \"Announces\" and searchIn:HeadlineOnly")`

- SearchIn token (FullStory):

  `rd_get_news_headlines(query = "\"inflation\" and searchIn:FullStory")`

- Language Filter:

  French: `rd_get_news_headlines(query = "LFR")`; English:
  `rd_get_news_headlines(query = "L:EN")`

- Reuters Instrument Code (RIC):

  `rd_get_news_headlines(query = "MSFT.O")`

- RIC + Relevancy:

  `rd_get_news_headlines(query = "MSFT.O", relevancy = "High")`

- Company Name + RIC + Relevancy:

  `rd_get_news_headlines(query = "LEN and \"Microsoft\" and MSFT.O", relevancy = "High")`

- Most Read News (M:1RS):

  `rd_get_news_headlines(query = "M:1RS")`

- Newswire Specific RCS Codes:

  `rd_get_news_headlines(query = "MRG")`

- Explicit Token News Source (NS):

  `rd_get_news_headlines(query = "NS:RTRS or NS:PRN or NS:TWTR")`

- Increasing the Limit:

  `rd_get_news_headlines(query = "\"stock repurchase\"", limit = 50)`

- Pagination using Cursor:

  `rd_get_news_headlines(query = "MSFT.O", cursor = "H4sIAAAAAAAA...", limit = 10)`

- Daterange using LAST syntax:

  `rd_get_news_headlines(query = "MRG last 5 days")`

- Daterange with BETWEEN syntax:

  `rd_get_news_headlines(query = "M:1RS BETWEEN 2024-03 AND 2024-04")`

- Daterange with explicit from/to:

  `rd_get_news_headlines(query = "Major breaking news", dateFrom = "2024-04-13T00:00:00Z", dateTo = "2024-04-14T00:00:00Z")`

Note: The parameter `limit` must not exceed 100. If a value greater than
100 is provided, the function will throw an error. When
`raw_output = TRUE`, a list of raw JSON responses is returned.
