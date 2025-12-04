# Pricing Stream Definition

Definition class for pricing streams (market data). Use
`rd_streaming_pricing$Definition$new()` to create a pricing stream
definition.

## Usage

``` r
rd_streaming_pricing
```

## Format

A list containing the `Definition` R6 class constructor.

## Value

PricingStreamDefinition object

## Details

To create a pricing stream definition, call:
`rd_streaming_pricing$Definition$new(universe, fields, parameters = NULL, domain = "MarketPrice")`

Arguments:

- `universe`: Character vector of instrument RICs

- `fields`: Character vector of field names

- `parameters`: Optional parameters list

- `domain`: Domain for OMM streams (default "MarketPrice")

## Examples

``` r
if (FALSE) { # \dontrun{
stream_def <- rd_streaming_pricing$Definition$new(
  universe = c("EUR=", "GBP="),
  fields = c("BID", "ASK", "DSPLY_NAME")
)
stream <- stream_def$get_stream()
} # }
```
