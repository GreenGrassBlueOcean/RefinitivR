# Initialize Eikon Python api

Initialize Eikon Python api

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

  Eikon api key

- Eikonapplication_port:

  proxy port id

- UUID:

  optional character parameter for custom instruments, not necessary for
  regular requests

- PythonModule:

  character choose between Eikon (python),RD (python),JSON (direct JSON
  message without python)

- TestConnection:

  Boolean, TRUE or FALSE test connection after initiating contact with
  Eikon terminal

## Value

a Python module that is an EikonObject

## Examples

``` r
if (FALSE) { # \dontrun{
Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L
, PythonModule = "Eikon")
Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L
, PythonModule = "RD")
} # }
```
