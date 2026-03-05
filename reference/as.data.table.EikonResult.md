# Convert EikonResult to data.table

Returns only the data portion as a `data.table`, avoiding the recycling
warning that occurs when the data and error frames have different row
counts.

## Usage

``` r
as.data.table.EikonResult(x, ...)
```

## Arguments

- x:

  An `EikonResult` object.

- ...:

  Passed to
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).

## Value

A `data.table`.
