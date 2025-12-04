# Function to process raw output of python get_symbology to better in r readable format

Function to process raw output of python get_symbology to better in r
readable format

## Usage

``` r
ProcessSymbology(EikonSymbologyResult, from_symbol_type, to_symbol_type)
```

## Arguments

- EikonSymbologyResult:

  nested list: output from EikonGetSymbology with option raw_output set
  to TRUE

- from_symbol_type:

  character use her same input as in EikonGetSymbology

- to_symbol_type:

  character use her same input as in EikonGetSymbology

## Value

data.frame containing 4 columns to_symbol_type, from_symbol_type,
BestMatch (as defined by Eikon), error

## See also

\[EikonGetSymbology()\]

## Examples

``` r
if (FALSE) { # \dontrun{
Raw_output_No_BestMatch <- EikonGetSymbology(EikonObject = Eikon
, symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
, to_symbol_type = "RIC" , raw_output = TRUE, bestMatch = FALSE  )
ProcessSymbology(EikonSymbologyResult = Raw_output_No_BestMatch
, from_symbol_type = "ISIN", to_symbol_type = "RIC")

Raw_output_BestMatch <- EikonGetSymbology(EikonObject = Eikon
, symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
, to_symbol_type = "RIC" , raw_output = TRUE, bestMatch = TRUE  )
ProcessSymbology(EikonSymbologyResult = Raw_output_BestMatch
, from_symbol_type = "ISIN", to_symbol_type = "RIC")
} # }
```
