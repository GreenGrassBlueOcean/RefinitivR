# Translate Frequency to ISO 8601 Duration

This function translates a given frequency string such as 'minute',
'hour', 'daily', 'weekly', etc. into the corresponding ISO 8601 duration
format (e.g., 'PT1M', 'P1D', 'P1W'). It defaults to 'P1D' (daily) if the
input is not recognized.

## Usage

``` r
translate_to_iso8601_duration(frequency = "daily")
```

## Arguments

- frequency:

  A character string representing the time frequency. Accepted values
  include 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly',
  'yearly'. Default is 'daily'.

## Value

A character string representing the ISO 8601 duration equivalent of the
input frequency.

## Details

This function specially helps transitioning legacy EikonGetTimeseries
Code to get rd_GetHistoricalPricing() when using this function one do
not need to worry about the correct interval

## Examples

``` r
translate_to_iso8601_duration('minute')   # Returns "PT1M"
#> [1] "PT1M"
translate_to_iso8601_duration('hour')     # Returns "PT1H"
#> [1] "PT1H"
translate_to_iso8601_duration('weekly')   # Returns "P1W"
#> [1] "P1W"
translate_to_iso8601_duration('unknown')  # Returns "P1D" (default)
#> [1] "P1D"
```
