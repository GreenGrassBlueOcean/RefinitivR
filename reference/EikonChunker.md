# Returns a list of chunked Rics so that api limits can be satisfied

Returns a list of chunked Rics so that api limits can be satisfied

## Usage

``` r
EikonChunker(
  RICS,
  Eikonfields = NULL,
  MaxCallsPerChunk = 12000,
  Duration = NULL,
  MaxRicsperChunk = 300
)
```

## Arguments

- RICS:

  a vector containing RICS to be requested

- Eikonfields:

  a list of the eikonfields to be requested default NULL, if eikonfields
  are supplied duration may not be supplied

- MaxCallsPerChunk:

  the maximum amount of apicalls that can be made

- Duration:

  a natural number denoting the amoount of rows asked for in a
  TimeSeries default NULL, if Duration is supplied Eikonfields may not
  be supplied

- MaxRicsperChunk:

  a natural number denoting the maximum amount of Rics that should be
  available in one call, default is 300.

## Value

a list of splitted RICS that can be returned to guarantee compliance
with api limits.

## References

<https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item>

## Examples

``` r
if (FALSE) "internal function no examples" # \dontrun{}
```
