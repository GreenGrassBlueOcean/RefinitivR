# Retrieve Ownership Data from LSEG

Fetches ownership and shareholder data from the LSEG Data Platform.
Supports consolidated, fund, insider, and investor views with automatic
pagination for large result sets.

## Usage

``` r
rd_GetOwnership(
  RDObject = rd_connection(),
  universe,
  view = c("consolidated/breakdown", "consolidated/concentration",
    "consolidated/top-n-concentration", "consolidated/investors",
    "consolidated/shareholders-report", "consolidated/shareholders-history-report",
    "consolidated/recent-activity", "fund/holdings", "fund/breakdown",
    "fund/concentration", "fund/top-n-concentration", "fund/investors",
    "fund/shareholders-report", "fund/shareholders-history-report",
    "fund/recent-activity", "insider/shareholders-report", "insider/transaction-report",
    "investor/holdings", 
     "org-info"),
  stat_type = NULL,
  limit = NULL,
  sort_order = NULL,
  frequency = NULL,
  start = NULL,
  end = NULL,
  count = NULL,
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

  Character string selecting the ownership view. Default is
  `"consolidated/breakdown"`. See **Details** for all available views.

- stat_type:

  Optional integer (1–8) specifying the breakdown type. Required for
  `consolidated/breakdown` and `fund/breakdown` views:

  1

  :   Investor Type

  2

  :   Investment Style

  3

  :   Region

  4

  :   Rotation

  5

  :   Country

  6

  :   Metro Area

  7

  :   Investor Type (Parent)

  8

  :   Investor Style (Parent)

- limit:

  Optional integer. Maximum total number of rows to retrieve. If `NULL`
  (default), fetches all available data via automatic pagination. The
  LSEG API returns at most 100 rows per request; this function handles
  paging transparently.

- sort_order:

  Optional character. Sort direction: `"asc"` or `"desc"`.

- frequency:

  Optional character. Data frequency: `"Q"` (quarterly) or `"M"`
  (monthly).

- start:

  Optional character. Start date (ISO 8601 format).

- end:

  Optional character. End date (ISO 8601 format).

- count:

  Optional integer. Record count (alternative to limit for some
  endpoints).

- use_field_names_in_headers:

  Logical. If `TRUE` (default), column names come from the API's `title`
  field; if `FALSE`, from the internal `name` field.

- raw_output:

  Logical. If `TRUE`, returns the raw parsed JSON list (or list of
  pages) instead of a `data.frame`.

- debug:

  Logical. If `TRUE`, prints debug messages.

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (300 s). `FALSE` disables caching. A positive numeric
  value sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

A `data.frame` with ownership data, or the raw JSON list if
`raw_output = TRUE`.

## Details

**Available views:**

*Consolidated:*

- `"consolidated/breakdown"` — Ownership breakdown (requires
  `stat_type`)

- `"consolidated/concentration"`

- `"consolidated/top-n-concentration"`

- `"consolidated/investors"` — Institutional investor list

- `"consolidated/shareholders-report"`

- `"consolidated/shareholders-history-report"`

- `"consolidated/recent-activity"`

*Fund:*

- `"fund/holdings"` — Fund holdings

- `"fund/breakdown"` — Fund breakdown (requires `stat_type`)

- `"fund/concentration"`

- `"fund/top-n-concentration"`

- `"fund/investors"`

- `"fund/shareholders-report"`

- `"fund/shareholders-history-report"`

- `"fund/recent-activity"`

*Insider:*

- `"insider/shareholders-report"`

- `"insider/transaction-report"`

*Investor:*

- `"investor/holdings"` — Individual investor holdings

*Organization:*

- `"org-info"` — Organization information

## Examples

``` r
if (FALSE) { # \dontrun{
# Consolidated investor breakdown by type
rd_GetOwnership(
  universe = "AAPL.O",
  view = "consolidated/breakdown",
  stat_type = 1
)

# Top fund holders (first 50)
rd_GetOwnership(
  universe = "MSFT.O",
  view = "fund/investors",
  limit = 50
)

# Insider transactions
rd_GetOwnership(
  universe = "TSLA.O",
  view = "insider/transaction-report"
)

# Organization info
rd_GetOwnership(universe = "IBM.N", view = "org-info")
} # }
```
