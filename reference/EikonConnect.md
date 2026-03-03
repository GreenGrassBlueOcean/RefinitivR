# Connect to LSEG Workspace via JSON

Establishes a connection to the LSEG Workspace Data API proxy running on
localhost. When called with no arguments the connection uses the
terminal's built-in `DEFAULT_WORKSPACE_APP_KEY` — no user-supplied API
key is required as long as LSEG Workspace is running.

## Usage

``` r
EikonConnect(
  Eikonapplication_id = NA,
  Eikonapplication_port = 9000L,
  UUID = NA,
  PythonModule = "JSON",
  TestConnection = FALSE
)
```

## Arguments

- Eikonapplication_id:

  Optional application key. When `NA` (the default) the function first
  checks the credential vault, then falls back to
  `"DEFAULT_WORKSPACE_APP_KEY"`.

- Eikonapplication_port:

  Deprecated and ignored.

- UUID:

  Optional character for custom instruments.

- PythonModule:

  Deprecated. Ignored; JSON is always used.

- TestConnection:

  Logical; if `TRUE`, performs a test query after connecting. Defaults
  to `FALSE`.

## Value

A `RefinitivConnection` object (also cached as the default singleton for
subsequent API calls).

## Details

For most workflows you do not need to call this function at all — API
functions default to
[`rd_connection()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_connection.md),
which auto-creates a connection on first use. Call `EikonConnect()`
explicitly when you need a custom API key or want to verify the
connection with `TestConnection = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Zero-argument connection (recommended):
Eikon <- EikonConnect()

# Explicit key:
Eikon <- EikonConnect(Eikonapplication_id = "your key")
} # }
```
