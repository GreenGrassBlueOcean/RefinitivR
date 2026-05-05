# Robust Symbol Conversion for Refinitiv Data

\`rd_ConvertSymbol\` is the modern, robust replacement for
[`EikonGetSymbology`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetSymbology.md).
It resolves critical gaps in the LSEG Data Library's native symbology by
introducing a multi-tiered fallback strategy. It automatically maps
"Bare RICs" (e.g., \`HES\`) to their primary exchange-listed
instruments, and uses a historical timeline lookup to fix fragile
letter-casing issues on delisted instruments.

## Usage

``` r
rd_ConvertSymbol(
  symbols,
  from_symbol_type = "RIC",
  to_symbol_type = "RIC",
  bestMatch = TRUE,
  handle_bare_rics = TRUE,
  canonical_history_fallback = TRUE,
  time_out = 300,
  verbose = FALSE,
  RDObject = NULL
)
```

## Arguments

- symbols:

  Character vector of symbols to convert.

- from_symbol_type:

  Character. Source symbology type. One of: `"RIC"`, `"ISIN"`,
  `"SEDOL"`, `"CUSIP"`, `"ticker"`, `"lipperID"`, `"IMO"`. Default:
  `"RIC"`.

- to_symbol_type:

  Character. Target symbology type. Same options as `from_symbol_type`,
  plus `"OAPermID"`. Default: `"RIC"`.

- bestMatch:

  Logical. If `TRUE` (default), returns only the single best match per
  input symbol. If `FALSE`, returns all alternatives.

- handle_bare_rics:

  Logical. If `TRUE` (default), enables Tier 2: bare RIC resolution via
  `TR.PrimaryInstrument`. Only applies when `from_symbol_type = "RIC"`.

- canonical_history_fallback:

  Logical. If `TRUE` (default), enables Tier 3: historical canonical
  fallback via `rd_GetHistory`. Only applies when
  `from_symbol_type = "RIC"`.

- time_out:

  Numeric. Maximum timeout in seconds. Default: 300.

- verbose:

  Logical. If `TRUE`, prints debug information.

- RDObject:

  Connection object (ignored, kept for back-compat).

## Value

A `data.table` with the following columns:

- `OriginalSymbol`:

  The input symbol as provided.

- `MappedSymbol`:

  The resolved symbol, or `NA` if unresolved.

- `ResolutionTier`:

  Which tier resolved the symbol: `"symbology"`, `"primary_instrument"`,
  `"history_canonical"`, `"identity"`, or `"none"`.

- `IsActive`:

  Logical. Whether the resolved instrument is active.

- `DelistingDate`:

  Character. The retire date (YYYY-MM-DD) if inactive, or `NA`.

## Details

The function operates in three tiers, executed in order until a match is
found:

1.  **Symbology API**: Standard conversion using the Refinitiv symbology
    endpoint. Handles active instruments, ISIN/SEDOL/CUSIP conversions,
    and most common lookups.

2.  **Bare RIC Resolution**: For RIC-to-RIC conversions where the input
    symbol lacks a dot suffix (e.g., `HES`, `A`, `LLY`), queries
    `TR.PrimaryInstrument` via `EikonGetData` to find the active
    exchange-suffixed RIC. This tier is critical because LSEG's
    symbology service cannot map bare tickers to exchange-suffixed RICs.

3.  **Canonical History Fallback**: For symbols that remain unresolved —
    typically delisted instruments with letter-casing mismatches —
    queries `rd_GetHistory(fields = "TR.RIC", start = "1950-01-01")`.
    This exploits the case-insensitive nature of the history endpoint to
    extract the true canonical RIC string (e.g., `1cOv.De` resolves to
    `1COv.DE^L25`).

## See also

[`EikonGetSymbology`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetSymbology.md)
(deprecated predecessor)

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Standard conversion (Tier 1) ---
# ISIN to RIC: standard symbology handles this natively
rd_ConvertSymbol(symbols = "US0378331005",
                 from_symbol_type = "ISIN",
                 to_symbol_type = "RIC")
#>   OriginalSymbol MappedSymbol ResolutionTier IsActive DelistingDate
#> 1:   US0378331005       AAPL.O      symbology     TRUE          <NA>

# --- Active Bare RIC Resolution (Tier 2) ---
# "A" is a bare RIC (no dot suffix). LSEG symbology cannot resolve it,
# but Tier 2 queries TR.PrimaryInstrument and maps to A.N (NYSE).
rd_ConvertSymbol("A")
#>   OriginalSymbol MappedSymbol     ResolutionTier IsActive DelistingDate
#> 1:              A          A.N primary_instrument     TRUE          <NA>

# --- Delisted Bare RIC (Tier 2) ---
# "HES" was Hess Corp, recently acquired and delisted. The bare ticker
# has no exchange suffix, so symbology fails. Tier 2 maps it to the
# delisted form HES.N^G25.
rd_ConvertSymbol("HES")
#>   OriginalSymbol MappedSymbol     ResolutionTier IsActive DelistingDate
#> 1:            HES    HES.N^G25 primary_instrument    FALSE    2025-07-18

# --- Case-Sensitive Delisted RIC (Tier 3) ---
# Refinitiv is highly case-sensitive for European delisted RICs.
# "1cOv.De" fails both symbology and bare-RIC checks. Tier 3 uses
# rd_GetHistory(TR.RIC) which is case-insensitive, extracting the
# canonical string "1COv.DE^L25" from the historical timeline.
rd_ConvertSymbol("1cOv.De")
#>   OriginalSymbol  MappedSymbol    ResolutionTier IsActive DelistingDate
#> 1:        1cOv.De 1COv.DE^L25 history_canonical    FALSE    2025-12-08

# --- Batch conversion (mixed scenarios) ---
rd_ConvertSymbol(c("AAPL.O", "HES", "1cOv.De", "INVALID_RIC"))
#>   OriginalSymbol  MappedSymbol     ResolutionTier IsActive DelistingDate
#> 1:         AAPL.O        AAPL.O          identity     TRUE          <NA>
#> 2:            HES     HES.N^G25 primary_instrument    FALSE    2025-07-18
#> 3:        1cOv.De  1COv.DE^L25  history_canonical    FALSE    2025-12-08
#> 4:    INVALID_RIC          <NA>               none       NA          <NA>

# --- Disable fallbacks for speed ---
# If you know all inputs are well-formed, skip Tier 2 & 3:
rd_ConvertSymbol(c("AAPL.O", "VOD.L"),
                 handle_bare_rics = FALSE,
                 canonical_history_fallback = FALSE)
} # }
```
