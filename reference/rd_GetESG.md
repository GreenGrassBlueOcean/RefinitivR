# Retrieve ESG Data from LSEG

Fetches Environmental, Social, and Governance (ESG) data from the LSEG
Data Platform. Supports multiple views including scores, measures, basic
overview, and the ESG universe listing.

## Usage

``` r
rd_GetESG(
  RDObject = rd_connection(),
  universe,
  view = c("scores-full", "scores-standard", "measures-full", "measures-standard",
    "basic", "universe"),
  start = NULL,
  end = NULL,
  use_field_names_in_headers = TRUE,
  raw_output = FALSE,
  debug = FALSE,
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

- universe:

  Character vector of instrument codes (RICs, PermIDs, etc.).

- view:

  Character string selecting the ESG view. One of:

  `"scores-full"`

  :   Complete ESG scores with all pillars and categories (default).

  `"scores-standard"`

  :   Standard ESG scores (subset of full).

  `"measures-full"`

  :   Detailed ESG measures across all pillars.

  `"measures-standard"`

  :   Standard ESG measures (subset of full).

  `"basic"`

  :   Basic ESG overview data.

  `"universe"`

  :   List of instruments covered by the ESG universe.

- start:

  Optional integer. Initial value of the financial year range (e.g.
  `2020`).

- end:

  Optional integer. End value of the financial year range (e.g. `2023`).

- use_field_names_in_headers:

  Logical. If `TRUE` (default), column names come from the API's `title`
  field; if `FALSE`, from the internal `name` field.

- raw_output:

  Logical. If `TRUE`, returns the raw parsed JSON list instead of a
  `data.frame`.

- debug:

  Logical. If `TRUE`, prints debug messages.

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (300 s). `FALSE` disables caching. A positive numeric
  value sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

A `data.frame` with ESG data, or the raw JSON list if
`raw_output = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Full ESG scores for Apple and Microsoft
rd_GetESG(universe = c("AAPL.O", "MSFT.O"))

# Standard measures for a specific year range
rd_GetESG(
  universe = "AAPL.O", view = "measures-standard",
  start = 2020, end = 2023
)

# Basic overview
rd_GetESG(universe = "IBM.N", view = "basic")
} # }
```
