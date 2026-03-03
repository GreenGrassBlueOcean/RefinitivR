# Create a JSON connection object for the LSEG Workspace proxy

Create a JSON connection object for the LSEG Workspace proxy

## Usage

``` r
RefinitivJsonConnect(Eikonapplication_id = NA, Eikonapplication_port = 9000L)
```

## Arguments

- Eikonapplication_id:

  Optional application key. When `NA` (the default) the function checks
  the credential vault, then falls back to
  `"DEFAULT_WORKSPACE_APP_KEY"`.

- Eikonapplication_port:

  Deprecated and ignored (always 9000).

## Value

A connection environment with API method closures.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- RefinitivJsonConnect()
} # }
```
