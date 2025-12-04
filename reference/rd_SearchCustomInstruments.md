# Show all available custom instruments that have been created

Show all available custom instruments that have been created

## Usage

``` r
rd_SearchCustomInstruments(RDObject = RefinitivJsonConnect(), debug = TRUE)
```

## Arguments

- RDObject:

  Refinitiv Data connection object, defaults to RefinitivJsonConnect()

- debug:

  show api calls defaults to false

## Value

a list of custom Instruments created with all parameters

## Examples

``` r
if (FALSE) { # \dontrun{
test <- rd_SearchCustomInstruments()
} # }
```
