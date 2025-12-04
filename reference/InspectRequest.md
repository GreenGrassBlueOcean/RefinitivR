# function to check if a downloaded dataframe is empty

function to check if a downloaded dataframe is empty

## Usage

``` r
InspectRequest(df, functionname, verbose = TRUE)
```

## Arguments

- df:

  data.frame

- functionname:

  function name for error reporting

- verbose:

  Boolean print or not variable

## Value

boolean

## Examples

``` r
Refinitiv:::InspectRequest(data.frame(), functionname = "test")
#> data.frame
#> 
#> 
#> test request returned with length 0
Refinitiv:::InspectRequest(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test")
#> data.frame
#> testtest2
#> NULLNULL
#> test request returned with length 2
```
