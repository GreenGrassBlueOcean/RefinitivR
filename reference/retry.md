# Function to retry failed functions after a time out of 5 seconds. Especially useful for failed api calls.

Function to retry failed functions after a time out of 5 seconds.
Especially useful for failed api calls.

## Usage

``` r
retry(retryfun, max = 2, init = 0)
```

## Arguments

- retryfun:

  function to retry.

- max:

  maximum number of retries, default = 2

- init:

  initial state of retries should always be left zero, default = zero

## Value

None

## Examples

``` r
 retry(sum(1,"a"), max = 2)
#> api request failed, automatically retrying time 1/2 error received: Error in sum(1, "a"): invalid 'type' (character) of argument
#> api request failed, automatically retrying time 2/2 error received: Error in sum(1, "a"): invalid 'type' (character) of argument
#> [1] NA
```
