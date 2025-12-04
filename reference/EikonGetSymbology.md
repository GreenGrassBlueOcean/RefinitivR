# Returns a list of instrument names converted into another instrument code. For example: convert SEDOL instrument names to RIC names

original python parameters raw_output and debug cannot be used due to
int64 python to R conversion problem.
<https://github.com/rstudio/reticulate/issues/729>

## Usage

``` r
EikonGetSymbology(
  EikonObject,
  symbol,
  from_symbol_type = "RIC",
  to_symbol_type = c("CUSIP", "ISIN", "SEDOL", "RIC", "ticker", "lipperID", "IMO",
    "OAPermID"),
  bestMatch = TRUE,
  time_out = 60,
  verbose = FALSE,
  raw_output = FALSE
)
```

## Arguments

- EikonObject:

  Eikon object created using EikonConnect function

- symbol:

  character or list of characters Single instrument or list of
  instruments to convert.

- from_symbol_type:

  character Instrument code to convert from. Possible values: 'CUSIP',
  'ISIN', 'SEDOL', 'RIC', 'ticker', 'lipperID', 'IMO' Default: 'RIC'

- to_symbol_type:

  character string or list Instrument code to convert to. Possible
  values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker', 'lipperID', 'IMO',
  'OAPermID' Default: None (means all symbol types are requested)

- bestMatch:

  boolean When set to TRUE, only primary symbol is requested. When set
  to FALSE, all symbols are requested

- time_out:

  numeric set the maximum timeout to the Eikon server, default = 60

- verbose:

  boolean, set to true to print out the actual python call with time
  stamp for debugging.

- raw_output:

  boolean Set this parameter to True to get the data in json format if
  set to FALSE, the function will return a data frame Default: FALSE

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
Eikon <- Refinitiv::EikonConnect()
ex1 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "AAPL.O"
 , to_symbol_type = "ISIN" )
ex2 <- EikonGetSymbology(EikonObject = Eikon
, symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
,  to_symbol_type = "RIC" , verbose = TRUE)
ex3 <- EikonGetSymbology(EikonObject = Eikon
, symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
ex4 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.AS"
, to_symbol_type = "ISIN"  , verbose = TRUE)
ex5 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.L"
, to_symbol_type = "ISIN"  , verbose = TRUE)
ex6 <- EikonGetSymbology(EikonObject = Eikon
, symbol =  c("GB00B03MLX29", "NL0015476987"), from_symbol_type = "ISIN"
,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
ex7 <- EikonGetSymbology(EikonObject = Eikon
, symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
} # }

if (FALSE) { # \dontrun{
 EikonJson <- RefinitivJsonConnect()
 ex1 <- EikonGetSymbology(EikonObject = EikonJson, symbol =  "AAPL.O"
 , to_symbol_type = "ISIN" )
} # }


```
