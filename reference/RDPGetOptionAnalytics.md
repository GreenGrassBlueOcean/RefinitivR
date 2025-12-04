# Get RDP option analytics

Get RDP option analytics

## Usage

``` r
RDPGetOptionAnalytics(
  RDP = RDConnect(),
  OptionRics = NULL,
  raw = FALSE,
  verbose = TRUE
)
```

## Arguments

- RDP:

  Refinitiv DataPlatform Connection object, defaults to RDConnect()

- OptionRics:

  character vector with option rics

- raw:

  return raw data from RDP call

- verbose:

  boolean, print download progress or not

## Value

data.frame with option data

## Examples

``` r
if (FALSE) { # \dontrun{
OPtionInstruments <- Refinitiv::RDPsearch(query = "aapl.o", view = "RelatedOption")
OPtionInstruments <- OPtionInstruments[grep(pattern = "*.U"
, x = OPtionInstruments$RIC),]$RIC

Analytics <- RDPGetOptionAnalytics(OptionRics = OPtionInstruments)
Analytics <- RDPGetOptionAnalytics(OptionRics = c("AAPLL032112500.U", "AAPLL032113700.U"))

} # }
```
