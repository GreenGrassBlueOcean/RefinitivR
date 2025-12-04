# Build a holiday object for a custom instrument

Build a holiday object for a custom instrument

## Usage

``` r
CustomInstrumentHolidayBuilder(dates = NULL, reasons = NULL)
```

## Arguments

- dates:

  vector of character dates in format YYYY-MM-DD

- reasons:

  character vector of reaons for this holida e.g. Christmas

## Value

object with holidays

## See also

\[rd_ManageCustomInstruments()\]

## Examples

``` r
CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
, reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))
#> [[1]]
#> [[1]]$date
#> [1] "2023-12-01"
#> 
#> [[1]]$reason
#> [1] "Special Bank Holiday 1"
#> 
#> 
#> [[2]]
#> [[2]]$date
#> [1] "2023-12-31"
#> 
#> [[2]]$reason
#> [1] "Special Bank Holiday 2"
#> 
#> 
#> attr(,"class")
#> [1] "Refinitiv_holidays"
```
