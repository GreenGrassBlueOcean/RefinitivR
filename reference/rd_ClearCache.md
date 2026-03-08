# Clear the RefinitivR Session Cache

Removes all cached API responses, fast-key aliases, and resets the
cached package version from the current R session.

## Usage

``` r
rd_ClearCache()
```

## Value

Invisibly `TRUE`.

## See also

[`rd_CacheInfo`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_CacheInfo.md)

## Examples

``` r
rd_ClearCache()
#> [RefinitivR] Cache cleared.
```
