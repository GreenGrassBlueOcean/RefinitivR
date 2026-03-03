# Retrieve Estimates Data from LSEG

Fetches consensus estimates (I/B/E/S) data from the LSEG Data Platform.
Supports actuals, summary forecasts, KPI, and historical-snapshot views
with configurable content packages.

## Usage

``` r
rd_GetEstimates(
  RDObject = rd_connection(),
  universe,
  view = c("view-summary/annual", "view-summary/interim", "view-summary/recommendations",
    "view-summary/non-periodic-measures",
    "view-summary/historical-snapshots-periodic-measures-annual",
    "view-summary/historical-snapshots-periodic-measures-interim",
    "view-summary/historical-snapshots-non-periodic-measures",
    "view-summary/historical-snapshots-recommendations", "view-actuals/annual",
    "view-actuals/interim", "view-actuals-kpi/annual", "view-actuals-kpi/interim",
    "view-summary-kpi/annual", "view-summary-kpi/interim", 
    
    "view-summary-kpi/historical-snapshots-kpi"),
  package = NULL,
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

  Character string selecting the estimates view. Default is
  `"view-summary/annual"`. See **Details** for all available views.

- package:

  Character string. Content tier controlling the breadth and depth of
  returned data. One of `"basic"`, `"standard"`, or `"professional"`.
  **Required** for all non-KPI views; ignored for KPI views.

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

A `data.frame` with estimates data, or the raw JSON list if
`raw_output = TRUE`.

## Details

**Available views:**

*Summary (require `package`):*

- `"view-summary/annual"` — Annual consensus estimates

- `"view-summary/interim"` — Interim period estimates

- `"view-summary/recommendations"` — Analyst recommendations

- `"view-summary/non-periodic-measures"` — Non-periodic measures

- `"view-summary/historical-snapshots-periodic-measures-annual"`

- `"view-summary/historical-snapshots-periodic-measures-interim"`

- `"view-summary/historical-snapshots-non-periodic-measures"`

- `"view-summary/historical-snapshots-recommendations"`

*Actuals (require `package`):*

- `"view-actuals/annual"` — Reported annual actuals

- `"view-actuals/interim"` — Reported interim actuals

*KPI (`package` not required):*

- `"view-actuals-kpi/annual"`

- `"view-actuals-kpi/interim"`

- `"view-summary-kpi/annual"`

- `"view-summary-kpi/interim"`

- `"view-summary-kpi/historical-snapshots-kpi"`

**Package tiers:**

- `"basic"`:

  Limited fields, single historical point (free tier).

- `"standard"`:

  Common fields, limited history.

- `"professional"`:

  All fields, complete history (requires entitlement).

## Examples

``` r
if (FALSE) { # \dontrun{
# Annual summary estimates for IBM (basic package)
rd_GetEstimates(universe = "IBM.N", package = "basic")

# Analyst recommendations for multiple companies
rd_GetEstimates(
  universe = c("AAPL.O", "MSFT.O"),
  view = "view-summary/recommendations",
  package = "standard"
)

# KPI actuals (no package needed)
rd_GetEstimates(
  universe = "TSLA.O",
  view = "view-actuals-kpi/annual"
)
} # }
```
