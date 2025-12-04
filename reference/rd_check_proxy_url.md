# Check if refinitiv proxy url is alive

Check if refinitiv proxy url is alive

## Usage

``` r
rd_check_proxy_url(port = 9060, debug = TRUE)
```

## Arguments

- port:

  9060 or 9000

- debug:

  boolean TRUE or False

## Value

api status code and version

## Examples

``` r
if (FALSE) { # \dontrun{
test <- rd_check_proxy_url(port = 9000)
test <- rd_check_proxy_url(port = 9060)
} # }
```
