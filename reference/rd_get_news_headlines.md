# Retrieve News Headlines from a Refinitiv RDP (JSON) Connection

This function constructs an HTTP GET query to retrieve news headlines
from the Refinitiv RDP service. It builds a query string from the
provided parameters using
[`build_get_query_string()`](https://greengrassblueocean.github.io/RefinitivR/reference/build_get_query_string.md)
and then sends the GET request via `send_json_request()`.

## Usage

``` r
rd_get_news_headlines(
  RDObject = RefinitivJsonConnect(),
  query = NULL,
  limit = 10L,
  sort = "NewToOld",
  relevancy = "All",
  cursor = NULL,
  dateFrom = NULL,
  dateTo = NULL,
  raw_output = FALSE,
  debug = TRUE
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

## Value

A `data.frame` with flattened fields, or the raw JSON if
`raw_output = TRUE`.

## Details

The query parameters may include various tokens such as Reuters
Instrument Codes (RIC), language codes, explicit free-text tokens (using
quotes), and date ranges. In addition, the function supports pagination
via a cursor.

Examples of queries include:

\- \*\*Explicit FreeText (use quotes):\*\* Obtains headlines for stories
containing the text "electric car" or "electric vehicle"

        rd_get_news_headlines(query = "\"electric car\" or \"electric vehicle\"")
      

\- \*\*SearchIn token (HeadlineOnly):\*\* Obtains headlines for stories
with "Reports" or "Announces" in their title, limiting the search to
headlines only

        rd_get_news_headlines(query = "\"Reports\" or \"Announces\" and searchIn:HeadlineOnly")
      

\- \*\*SearchIn token (FullStory):\*\* Obtains headlines for stories
with "inflation" when searching in the full story text

        rd_get_news_headlines(query = "\"inflation\" and searchIn:FullStory")
      

\- \*\*Language Filter:\*\* For French headlines:

        rd_get_news_headlines(query = "LFR")
      

For English headlines (disambiguated with the language prefix "L:"):

        rd_get_news_headlines(query = "L:EN")
      

\- \*\*Reuters Instrument Code (RIC):\*\*

        rd_get_news_headlines(query = "MSFT.O")
      

\- \*\*Combination of RIC and Relevancy:\*\*

        rd_get_news_headlines(query = "MSFT.O", relevancy = "High")
      

\- \*\*Combination of Company Name, RIC and Relevancy:\*\*

        rd_get_news_headlines(query = "LEN and \"Microsoft\" and MSFT.O", relevancy = "High")
      

\- \*\*Most Read News (M:1RS):\*\*

        rd_get_news_headlines(query = "M:1RS")
      

\- \*\*Newswire Specific RCS Codes (MRG):\*\*

        rd_get_news_headlines(query = "MRG")
      

\- \*\*Explicit Token News Source (NS):\*\*

        rd_get_news_headlines(query = "NS:RTRS or NS:PRN or NS:TWTR")
      

\- \*\*Increasing the Limit:\*\*

        rd_get_news_headlines(query = "\"stock repurchase\"", limit = 50)
      

\- \*\*Pagination using Cursor:\*\*

        rd_get_news_headlines(query = "MSFT.O", cursor = "H4sIAAAAAAAA...", limit = 10)
      

\- \*\*Daterange using LAST syntax:\*\*

        rd_get_news_headlines(query = "MRG last 5 days")
      

\- \*\*Daterange with BETWEEN syntax:\*\*

        rd_get_news_headlines(query = "M:1RS BETWEEN 2024-03 AND 2024-04")
      

\- \*\*Daterange with explicit "from,to" syntax:\*\*

        rd_get_news_headlines(query = "Major breaking news", dateFrom = "2024-04-13T00:00:00Z", dateTo = "2024-04-14T00:00:00Z")
      

\*\*Note:\*\* The parameter `limit` must not exceed 100. If a value
greater than 100 is provided, the function will throw an error. When
`raw_output = TRUE`, a list of raw JSON responses is returned.
