# Returns a list of chunked Rics so that api limits can be satisfied

Returns a list of chunked Rics so that api limits can be satisfied

## Usage

``` r
EikonChunker(
  RICS,
  Eikonfields = NULL,
  MaxCallsPerChunk = 12000,
  Duration = NULL,
  MaxRicsperChunk = 300,
  verbose = getOption("refinitiv_progress", TRUE),
  Parameters = NULL,
  MaxRowsPerChunk = 10000L
)
```

## Arguments

- RICS:

  a vector containing RICS to be requested

- Eikonfields:

  a list of the eikonfields to be requested default NULL

- MaxCallsPerChunk:

  the maximum amount of apicalls that can be made

- Duration:

  a natural number denoting the amount of rows asked for in a TimeSeries
  default NULL

- MaxRicsperChunk:

  a natural number denoting the maximum amount of Rics that should be
  available in one call, default is 300.

- verbose:

  boolean or character, prints progress information when chunking

- Parameters:

  optional named list of request parameters (e.g. `SDate`, `EDate`,
  `FRQ`) used to estimate multi-period row counts per instrument

- MaxRowsPerChunk:

  maximum rows per chunk before server-side truncation (default 10000L)

## Value

a list of splitted RICS that can be returned to guarantee compliance
with api limits.

## References

<https://developers.lseg.com/en/api-catalog/eikon/eikon-data-api/documentation>

## Examples

``` r
if (FALSE) { # \dontrun{
"internal function no examples"
} # }
```
