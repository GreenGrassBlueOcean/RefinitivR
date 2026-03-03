# Connect to LSEG / Refinitiv Data via JSON

Establishes a connection to the LSEG Workspace Data API proxy. When
called with no arguments the terminal's built-in
`DEFAULT_WORKSPACE_APP_KEY` is used automatically.

## Usage

``` r
RDConnect(application_id = NA, PythonModule = "JSON", UUID = NA)

RDPConnect(application_id = NA, PythonModule = "JSON", UUID = NA)
```

## Arguments

- application_id:

  Optional application key. When `NA` (the default) the function checks
  the credential vault, then falls back to
  `"DEFAULT_WORKSPACE_APP_KEY"`.

- PythonModule:

  Deprecated. Ignored; JSON is always used.

- UUID:

  Optional character for custom instruments.

## Value

A `RefinitivConnection` object (also cached as the default singleton for
subsequent API calls).

## Details

This is functionally equivalent to
[`EikonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonConnect.md).
For most workflows you do not need to call either — API functions
default to
[`rd_connection()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Zero-argument connection (recommended):
rd <- RDConnect()

# Explicit key:
rd <- RDConnect(application_id = "your key")
} # }
if (FALSE) { # \dontrun{
rd <- RDPConnect()
} # }
```
