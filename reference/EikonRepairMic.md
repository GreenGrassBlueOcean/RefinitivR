# Repair Market Identifier Codes (MIC) utilizing RDN Exchange Identifiers

\`EikonRepairMic\` addresses the unreliability of the
\`TR.OperatingMIC\` field in Refinitiv Eikon data. It repairs missing
(\`NA\`), empty (\`""\`), or invalid (\`"XXXX"\`) MIC codes by
cross-referencing against an internal lookup table
(\`Refinitiv::OperatingMicLookup\`).

The function is designed to be "invisible" in a pipeline, automatically
detecting available exchange identifiers and handling both space and
dot-separated column names.

## Usage

``` r
EikonRepairMic(Fundamentals_Data, verbose = FALSE)
```

## Arguments

- Fundamentals_Data:

  data.frame or data.table. The dataset containing fundamentals, which
  must include at least one exchange identifier (e.g.,
  \`RDN_ExchangeCode\`).

- verbose:

  logical. If \`TRUE\`, prints a summary of repairs made to the console.

## Value

A `data.frame` (matches input class) with corrected Market Identifier
Codes.

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard usage in a data fetching pipeline
rics <- c("WM", "AAPL.O", "VOD.L")
fields <- c("TR.RDNExchangeCode", "TR.OperatingMIC", "TR.CompanyName")

raw_data <- EikonGetData(Eikon, rics, fields)$PostProcessedEikonGetData

# WM often returns "XXXX" for OperatingMIC; this fixes it using RDNExchangeCode
clean_data <- EikonRepairMic(raw_data, verbose = TRUE)
} # }
```
