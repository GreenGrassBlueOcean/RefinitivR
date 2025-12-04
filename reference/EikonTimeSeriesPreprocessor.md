# Preprocessor for Eikon Get timeseries to automatically chunk pieces in the required length

This is a subfunction of EikonGetTimeseries

## Usage

``` r
EikonTimeSeriesPreprocessor(interval, rics, start_date, end_date)
```

## Arguments

- interval:

  character choose from c('tick', 'minute', 'hour', 'daily', 'weekly',
  'monthly', 'quarterly', 'yearly')

- rics:

  character vector containing the Reuters unique stock indentifier

- start_date:

  character start date in format "YYYY-MM-DD"

- end_date:

  character end date in format "YYYY-MM-DD"

## Value

list with chunked rics

## See also

EikonGetTimeseries

## Examples

``` r
test <- Refinitiv:::EikonTimeSeriesPreprocessor(interval = "daily"
, rics = rep(letters, 1000), start_date = "2015-01-01", end_date = "2018-01-01")
#> The operation is too large for one api request and will be chunked in multiple requests
#> the operation you intend to perform will cost 20358000 data points
```
