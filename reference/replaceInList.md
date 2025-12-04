# Replace items in nested list

Replace items in nested list

## Usage

``` r
replaceInList(x, FUN, ...)
```

## Arguments

- x:

  list

- FUN:

  function to operate on list

- ...:

  pass through parameters

## Value

list

## Examples

``` r
 x <- list(list(NA, NULL, NULL), list("a", "b", "c"))
# test <- Refinitiv:::replaceInList(x, function(x)if(is.null(x))NA else x)
```
