# Check Terminal Type and Connectivity for Eikon or Workspace

This function checks the connectivity to Eikon or Workspace and sets the
corresponding port in the global options. It first verifies if there is
a terminal connection available and then checks if Eikon or Workspace
Desktop is running by testing specific ports.

## Usage

``` r
CheckTerminalType(verbose = FALSE, force = FALSE)
```

## Arguments

- verbose:

  Logical; if \`TRUE\`, the function will print messages about the
  detection process. Defaults to \`FALSE\`.

- force:

  Logical; if \`TRUE\`, the function will recheck and reset the terminal
  type even if the `eikon_port` option is already set. Defaults to
  \`FALSE\`.

## Value

This function sets a global option `eikon_port` to either 9000
(Workspace) or 9060 (Eikon), depending on the detected connection. If
there is no connection, the function will stop with an error message.

## Details

The function checks for the existence of a terminal connection by
attempting to connect to the proxy on port 9000. If successful, it then
checks for Eikon connectivity by attempting to connect to port 9060. If
Eikon is running, the port is set to 9060, and if Workspace is detected,
the port is set to 9000.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check terminal connection and detect terminal type
CheckTerminalType(verbose = TRUE, force = TRUE)
} # }
```
