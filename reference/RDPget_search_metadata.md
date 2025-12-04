# Get search metadata from RDP

Get search metadata from RDP

## Usage

``` r
RDPget_search_metadata(RDP = RDConnect(), searchView = NULL)
```

## Arguments

- RDP:

  Refinitiv DataPlatform Connection object

- searchView:

  character choose from @seealso RDPShowAvailableSearchViews

## Value

data.table with metadata search results

## See also

RDPShowAvailableSearchViews

## Examples

``` r
if (FALSE) { # \dontrun{
test_json <- RDPget_search_metadata(RDP =  RefinitivJsonConnect()
                              , searchView = "EquityQuotes")
test_rd <- RDPget_search_metadata(RDP = RDConnect()
                              , searchView = "EQUITY_QUOTES")
} # }
```
