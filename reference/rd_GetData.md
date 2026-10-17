# Retrieve Snapshot Data from LSEG Workspace / Eikon via the JSON API

Automatically chunks the requested instruments to comply with API limits
and recombines the results into a single `data.frame`.

## Usage

``` r
rd_GetData(
  RDObject = rd_connection(),
  rics,
  Eikonformulas = NULL,
  Parameters = NULL,
  raw_output = FALSE,
  time_out = 60,
  verbose = getOption("refinitiv_progress", TRUE),
  SpaceConvertor = NULL,
  use_field_names_in_headers = FALSE,
  SyncFields = FALSE,
  cache = NULL,
  fields = NULL
)
```

## Arguments

- RDObject:

  Refinitiv Data connection object, defaults to
  [`rd_connection()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_connection.md)

- rics:

  a vector containing the instrument RICs

- Eikonformulas:

  a vector containing character string of Eikon Formulas (deprecated,
  use `fields` instead)

- Parameters:

  a named key value list for setting parameters, Default: NULL

- raw_output:

  to return the raw list by chunk for debugging purposes, default =
  FALSE

- time_out:

  set the maximum timeout to the LSEG/Eikon server, default = 60

- verbose:

  boolean, set to TRUE to print out the API call details with time stamp
  for debugging.

- SpaceConvertor:

  converts spaces in variables name into one of the following characters
  ".", "," , "-", "\_", default is NULL

- use_field_names_in_headers:

  boolean return request fieldnames in stead of titles

- SyncFields:

  boolean, synchronize fields over same time axis

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (300 s). `FALSE` disables caching. A positive numeric
  value sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

- fields:

  a vector containing character string of requested data fields
  (preferred over `Eikonformulas`)

## Value

a data.frame containing data from LSEG Workspace / Eikon

## References

<https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item>

## Examples

``` r
if (FALSE) { # \dontrun{
ex1 <- rd_GetData(
  rics = c("MMM", "III.L"),
  fields = c(
    "TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/",
    "TR.CompanyName"
  ),
  verbose = TRUE
)

ex2 <- rd_GetData(
  rics = "AAPL.O",
  fields = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
)

rics <- c("AAPL.O")
fields <- c(
  "TR.IssueMarketCap(Scale=6,ShType=FFL)", "TR.FreeFloatPct()/100/*FreefloatWeight*/",
  "TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/",
  "TR.CLOSEPRICE(Adjusted=0)/*close*/"
)

parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01", "Fill" = "None")
test_json <- rd_GetData(
  rics = rics,
  fields = fields,
  Parameters = parameters,
  use_field_names_in_headers = TRUE,
  SyncFields = FALSE
)
} # }
```
