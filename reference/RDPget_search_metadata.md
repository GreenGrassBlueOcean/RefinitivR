# Get search metadata from RDP

Get search metadata from RDP

## Usage

``` r
RDPget_search_metadata(RDP = rd_connection(), searchView = NULL, cache = NULL)
```

## Arguments

- RDP:

  Refinitiv JSON connection object

- searchView:

  character choose from @seealso RDPShowAvailableSearchViews

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (3600 s / 1 hour). `FALSE` disables caching. A positive
  numeric value sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

data.table with metadata search results

## See also

RDPShowAvailableSearchViews

## Examples

``` r
if (FALSE) { # \dontrun{
test_json <- RDPget_search_metadata(
  RDP = RefinitivJsonConnect(),
  searchView = "EquityQuotes"
)
} # }
```
