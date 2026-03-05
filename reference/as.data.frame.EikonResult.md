# Convert EikonResult to data.frame

Returns only the data portion, discarding the error metadata. This
matches the documented `@return` of `EikonGetData` ("a data.frame
containing data from Eikon").

## Usage

``` r
# S3 method for class 'EikonResult'
as.data.frame(x, ...)
```

## Arguments

- x:

  An `EikonResult` object.

- ...:

  Ignored.

## Value

A `data.frame`.
