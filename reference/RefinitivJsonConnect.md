# Connect to Eikon directly with JSON requests

Connect to Eikon directly with JSON requests

## Usage

``` r
RefinitivJsonConnect(Eikonapplication_id = NA, Eikonapplication_port = 9000L)
```

## Arguments

- Eikonapplication_id:

  character eikon api key

- Eikonapplication_port:

  numeric proxy port 9000 or 9060 for RDP/RD

## Value

RefinitivConnection Object

## Examples

``` r
if (FALSE) { # \dontrun{
 EikonJson <- RefinitivJsonConnect()
} # }
```
