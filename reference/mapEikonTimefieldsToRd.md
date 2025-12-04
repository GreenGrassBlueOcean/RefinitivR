# Map Eikon Time Fields to Refinitiv Data (RD) Fields

This function translates a vector of column names from the Eikon format
to corresponding Refinitiv Data (RD) names based on a predefined
mapping. If a column name does not have a mapping, it is kept unchanged.

## Usage

``` r
mapEikonTimefieldsToRd(
  cols = c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE")
)
```

## Arguments

- cols:

  A character vector of original column names in Eikon format. Defaults
  to c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE").

## Value

A character vector of translated column names for Refinitiv Data (RD).

## See also

\[rd_GetHistoricalPricing()\]

## Examples

``` r
mapEikonTimefieldsToRd()
#>   TIMESTAMP      VOLUME        HIGH         LOW        OPEN       CLOSE 
#> "TIMESTAMP" "ACVOL_UNS"  "MKT_HIGH"   "MKT_LOW"  "MKT_OPEN"   "CLS_AUC" 
mapEikonTimefieldsToRd(c("TIMESTAMP", "UNKNOWN_COLUMN", "CLOSE"))
#>        TIMESTAMP   UNKNOWN_COLUMN            CLOSE 
#>      "TIMESTAMP" "UNKNOWN_COLUMN"        "CLS_AUC" 
```
