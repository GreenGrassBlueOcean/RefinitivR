# Process output from refintiv data to r data.frame output

Process output from refintiv data to r data.frame output

## Usage

``` r
rd_OutputProcesser(
  x,
  use_field_names_in_headers = TRUE,
  NA_cleaning = TRUE,
  SpaceConvertor = NULL
)
```

## Arguments

- x:

  refinitiv data platform output

- use_field_names_in_headers:

  boolean wheater or not to return titles of field (formulas) as headers

- NA_cleaning:

  clean NA in return data

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
 EndPoint = "data/datagrid/beta1/"
 payload <- list( 'universe'= as.list(c("GOOG.O", "NVDA.O"))
               , 'fields'= as.list(c('TR.CLOSE', 'TR.OPEN'))
               , 'parameters'=list('SDate'= '2022-10-05', 'EDate'= '2022-11-05')
               , 'output'= 'Col,T|Va,Row,In,date|'
               )

response <- send_json_request(json = payload, service = "rdp"
, EndPoint = EndPoint, request_type = "POST")
Output <- rd_OutputProcesser(response)
} # }
```
