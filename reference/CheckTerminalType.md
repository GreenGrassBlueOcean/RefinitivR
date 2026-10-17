# Check LSEG Workspace Connectivity

Verifies that the LSEG Workspace Data API proxy is reachable on port
9000 and sets the `eikon_port` option accordingly.

## Usage

``` r
CheckTerminalType(verbose = FALSE, force = FALSE)
```

## Arguments

- verbose:

  Logical; if `TRUE`, prints status messages. Defaults to `FALSE`.

- force:

  Logical; if `TRUE`, rechecks even when `eikon_port` is already set.
  Defaults to `FALSE`.

## Value

Invisibly `NULL`. Sets `options(eikon_port = 9000L)` on success; issues
a warning if the proxy is not reachable.

## Details

Auto-detection is skipped when `eikon_port` is already set.
Pre-configure via `options(eikon_port = 9000L)` or the `REFINITIV_PORT`
environment variable (read at package load).

## Examples

``` r
if (FALSE) { # \dontrun{
CheckTerminalType(verbose = TRUE, force = TRUE)
} # }
```
