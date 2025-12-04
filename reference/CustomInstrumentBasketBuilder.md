# Build a basket element for a custom instrument

Build a basket element for a custom instrument

## Usage

``` r
CustomInstrumentBasketBuilder(RICs = NULL, Weights = NULL, Normalize = TRUE)
```

## Arguments

- RICs:

  character vector of RICs in the basket

- Weights:

  numeric vector of weights in the basket

- Normalize:

  boolean Normalize all weight to a maximum of 1?

## Value

a basket object that can be used

## See also

\[rd_ManageCustomInstruments()\]

## Examples

``` r
if (FALSE) { # \dontrun{
CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))
} # }
```
