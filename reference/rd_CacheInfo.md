# Show RefinitivR Cache Statistics

Reports the number of active and expired entries in the session cache,
together with an estimated total size in megabytes and the number of
fast-key aliases.

## Usage

``` r
rd_CacheInfo()
```

## Value

A list with elements `total_keys`, `active_keys`, `expired_keys`,
`aliases`, and `estimated_size_mb`, returned invisibly. A summary is
also printed via [`message()`](https://rdrr.io/r/base/message.html).

## See also

[`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md)

## Examples

``` r
rd_CacheInfo()
#> [RefinitivR] Cache: 0 active, 0 expired, 0 aliases, ~0.00 MB
```
