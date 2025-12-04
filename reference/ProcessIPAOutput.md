# Process output of RDP IPA calls

Process output of RDP IPA calls

## Usage

``` r
ProcessIPAOutput(IPAoutput)
```

## Arguments

- IPAoutput:

  output to a python call RDP\$ipa\$FinancialContracts

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
ipa_output <- RDPGetOptionAnalytics(
 OptionRics = c("AAPLL032112500.U", "AAPLL032113700.U")
, raw = TRUE)
ProcessIPAOutput(ipa_output)
} # }
```
