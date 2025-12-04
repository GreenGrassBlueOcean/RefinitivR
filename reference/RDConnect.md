# RD connection function to refinitiv Data libraries

RD connection function to refinitiv Data libraries

RDPConnect alias to provide backwards compatability with code written
for RDP

## Usage

``` r
RDConnect(application_id = NA, PythonModule = "JSON", UUID = NA)

RDPConnect(application_id = NA, PythonModule = "JSON", UUID = NA)
```

## Arguments

- application_id:

  refinitiv data api key

- PythonModule:

  character "JSON" or "RD"

- UUID:

  optional character parameter for custom instruments, not necessary for
  regular requests

## Value

RD opbject

## Examples

``` r
if (FALSE) { # \dontrun{
rd <- RDConnect(application_id = "your key")
} # }
if (FALSE) { # \dontrun{
rd <- RDPConnect(application_id = "your key")
} # }
```
