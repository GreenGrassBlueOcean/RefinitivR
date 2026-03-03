# Postprocessor for raw Timeseries Requests

Postprocessor for raw Timeseries Requests

## Usage

``` r
PostProcessTimeSeriesRequest(RawTimeSeriesRequest)
```

## Arguments

- RawTimeSeriesRequest:

  raw return from eikon EikonGetTimeseries

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
RawTimeSeries <- try(EikonGetTimeseries(
  EikonObject = Eikon,
  rics = c("MMM"),
  start_date = "2020-01-01T01:00:00",
  end_date = "2020-01-10T01:00:00",
  fields = "CLOSE", raw = TRUE
))
PostProcessTimeSeriesRequest(RawTimeSeries)
} # }
```
