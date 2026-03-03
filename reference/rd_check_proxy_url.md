# Check if the LSEG Workspace proxy is alive

Check if the LSEG Workspace proxy is alive

## Usage

``` r
rd_check_proxy_url(port = 9000L, debug = TRUE)
```

## Arguments

- port:

  Proxy port (default 9000).

- debug:

  Logical; if `TRUE`, prints request details.

## Value

API status code and version.

## Examples

``` r
if (FALSE) { # \dontrun{
test <- rd_check_proxy_url(port = 9000)
} # }
```
