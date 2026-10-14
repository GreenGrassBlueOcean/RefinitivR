# Translate Frequency to ISO 8601 Duration

This function translates a given frequency string such as 'minute',
'hour', 'daily', 'weekly', etc. into the corresponding ISO 8601 duration
format (e.g., 'PT1M', 'P1D', 'P1W'). It defaults to 'P1D' (daily) if the
input is not recognized.

## Usage

``` r
translate_to_iso8601_duration(frequency = "daily", default = "P1D")
```

## Arguments

- frequency:

  A character string representing the time frequency. Accepted values
  include 'minute', '1min', '5min', '10min', '30min', '60min', 'hour',
  'hourly', '1h', 'daily', '1d', '1D', '7D', '7d', 'weekly', '1W',
  'monthly', '1M', 'quarterly', '3M', 'yearly', '12M', '1Y', or ISO 8601
  durations like 'PT1M', 'PT5M', 'PT10M', 'PT30M', 'PT60M', 'PT1H',
  'P1D', 'P7D', 'P1W', 'P1M', 'P3M', 'P12M', 'P1Y'. Default is 'daily'.

- default:

  The fallback duration to return if the frequency is not recognized.
  Default is 'P1D'.

## Value

A character string representing the ISO 8601 duration equivalent of the
input frequency.

## Details

This function specially helps transitioning legacy EikonGetTimeseries
Code to get rd_GetHistoricalPricing() when using this function one do
not need to worry about the correct interval

## Examples

``` r
Refinitiv:::translate_to_iso8601_duration("minute") # Returns "PT1M"
#> [1] "PT1M"
Refinitiv:::translate_to_iso8601_duration("1min") # Returns "PT1M"
#> [1] "PT1M"
Refinitiv:::translate_to_iso8601_duration("1D") # Returns "P1D"
#> [1] "P1D"
Refinitiv:::translate_to_iso8601_duration("daily") # Returns "P1D"
#> [1] "P1D"
Refinitiv:::translate_to_iso8601_duration("PT1M") # Returns "PT1M"
#> [1] "PT1M"
Refinitiv:::translate_to_iso8601_duration("hour") # Returns "PT1H"
#> [1] "PT1H"
Refinitiv:::translate_to_iso8601_duration("weekly") # Returns "P1W"
#> [1] "P1W"
Refinitiv:::translate_to_iso8601_duration("unknown") # Returns "P1D" (default)
#> [1] "P1D"
```
