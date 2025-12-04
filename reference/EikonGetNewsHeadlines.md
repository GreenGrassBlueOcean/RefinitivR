# Returns a list of news headlines

Returns a list of news headlines

## Usage

``` r
EikonGetNewsHeadlines(
  EikonObject = EikonConnect(),
  query = NULL,
  count = 10L,
  repository = c("NewsWire", "NewsRoom", "WebNews"),
  date_from = NULL,
  date_to = NULL,
  raw_output = FALSE,
  debug = FALSE
)
```

## Arguments

- EikonObject:

  Connection Object result from EikonConnect()

- query:

  character array optional News headlines search criteria. The text can
  contain RIC codes, company names, country names and operators (AND,
  OR, NOT, IN, parentheses and quotes for explicit search…).Tip: Append
  'R:' in front of RIC names to improve performance.

- count:

  integer, optional Max number of headlines retrieved.Value Range:
  \[1-100\].Default: 10

- repository:

  character, vector of characters, optionalPossible values:
  c("NewsWire","NewsRoom","WebNews") For "NewsRoom" and "WebNews"
  repositories a query must be defined.

- date_from:

  string or date, optional Beginning of date range. String format is:
  '%Y-%m-%dT%H:%M:%S'. e.g. 2016-01-20T15:04:05.

- date_to:

  string or datetime, optional End of date range. String format is:
  '%Y-%m-%dT%H:%M:%S'. e.g. 2016-01-20T15:04:05.

- raw_output:

  boolean if TRUE provide only the raw downloaded info from Eikon

- debug:

  boolean if TRUE prints out the python call to the console

## Value

Returns a data frame of news headlines with the following columns:

- "Index": Timestamp of the publication time

- "version_created": Date of the latest update on the news

- "text": Text of the Headline

- "story_id": Identifier to be used to retrieve the full story using the
  get_news_story legacy

- "source_code": Second news identifier

## Examples

``` r
if (FALSE) { # \dontrun{
 Eikon <- Refinitiv::EikonConnect()
 headlines <- EikonGetNewsHeadlines( EikonObject = Eikon
                                   , query = c("R:MSFT.O", "R:AAPL.O") , count = 2, debug = TRUE)
} # }

if (FALSE) { # \dontrun{
  EikonJson <- RefinitivJsonConnect()
  headlines <- EikonGetNewsHeadlines( EikonObject = EikonJson, debug = TRUE
                                    , query = "R:MSFT.O", count = 2)
} # }
```
