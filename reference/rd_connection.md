# Get or create the default Refinitiv connection

Returns a cached connection object, creating it on first use. The
connection auto-detects the LSEG terminal and uses the
`DEFAULT_WORKSPACE_APP_KEY`. Call with `reset = TRUE` to force
re-creation (e.g. after terminal restart).

## Usage

``` r
rd_connection(reset = FALSE)
```

## Arguments

- reset:

  Logical; if `TRUE`, discard cached connection and create a fresh one.
  Default `FALSE`.

## Value

A `RefinitivConnection` object.

## Details

Most users of LSEG Workspace do not need to supply an API key — the
default `DEFAULT_WORKSPACE_APP_KEY` is used automatically. If you need a
custom key, call `EikonConnect(Eikonapplication_id = "your_key")` once;
the singleton is updated automatically for all subsequent calls.

The connection object is lightweight (an environment of closures with no
network I/O on creation). Terminal detection happens on first use of an
API function.

To skip terminal auto-detection (e.g., in CI or with a remote terminal),
set `REFINITIV_PORT` and optionally `REFINITIV_BASE_URL` in your
`.Renviron` file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Most users never need to call this directly.
# All API functions default to rd_connection():
data <- EikonGetData(rics = "AAPL.O", Eikonformulas = "TR.CompanyName")

# Force re-creation after terminal restart:
rd_connection(reset = TRUE)
} # }
```
