# Function to obtain data from Eikon via the JSON API

The function automatically chunks the list of rics into chunks that
comply with the api limitations and in the end rebuilds the chunks again
into a single data.frame.

## Usage

``` r
EikonGetData(
  EikonObject = rd_connection(),
  rics,
  Eikonformulas,
  Parameters = NULL,
  raw_output = FALSE,
  time_out = 60,
  verbose = getOption("refinitiv_progress", TRUE),
  SpaceConvertor = ".",
  cache = NULL
)
```

## Arguments

- EikonObject:

  Eikon connection object. Defaults to
  [`rd_connection()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_connection.md),
  which auto-creates a connection on first use.

- rics:

  a vector containing the instrument RICS

- Eikonformulas:

  a vector containing character string of Eikon Formulas

- Parameters:

  a named key value list for setting parameters, Default: NULL

- raw_output:

  to return the raw list by chunk for debugging purposes, default =
  FALSE

- time_out:

  set the maximum timeout to the Eikon server, default = 60

- verbose:

  boolean, set to TRUE to print out the API call details with time stamp
  for debugging.

- SpaceConvertor:

  converts spaces in variables name into one of the following characters
  ".", "," , "-", "\_", default is "."

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (300 s). `FALSE` disables caching. A positive numeric
  value sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

a data.frame containing data from Eikon

## Details

**LSEG Backend AQL Error Handling and Adaptive Sub-Chunking:** When
LSEG's backend Asynchronous Query Library (AQL) is under heavy load and
receives a large multi-RIC \\\times\\ multi-formula request (e.g. \\\>
28\\ RICs \\\times\\ 100+ `TR.*` formulas), LSEG can fail in three ways:

1.  *Async ticket poll failure*: `DataGrid_StandardAsync` returns
    `estimatedDuration` + `ticket`, and the subsequent poll returns
    top-level `ErrorCode = 2504` / `500` / `400`. `send_json_request()`
    restores `original_json` before retrying so it resubmits the query
    rather than re-polling a dead ticket.

2.  *Collapsed 1-column HTTP 200 payload*: The proxy returns HTTP 200
    with `totalColumnsCount = 1`,
    `headers[[1]][[1]]$displayName = NULL`, and `code = 100`
    (`"Asynchronous Query library internal error."`) in `$error` for
    every row. Without validation, post-processing turns
    `displayName = NULL` into a column literally named `"NA"` and
    silently drops all requested formula columns for that chunk
    (observed in production under both R 4.5.3 on 2026-10-10 /
    2026-10-14 and R 4.6.1 on 2026-10-17).

3.  *Persistent oversized chunk timeouts*: Retrying the same 100+ RIC
    chunk against a saturated AQL worker pool fails \\\sim 85\\\\ of the
    time. When an oversized chunk (\\\> 28\\ RICs) hits an AQL collapse,
    `EikonGetData()` automatically splits that chunk into \\\le 25\\-RIC
    sub-chunks (`getOption("refinitiv_aql_fallback_rics", 25L)`), merges
    the raw sub-chunk payloads, and pre-emptively steps down all
    remaining oversized chunks in the same call.

## References

<https://developers.lseg.com/en/api-catalog/eikon/eikon-data-api/documentation>

## Examples

``` r
if (FALSE) { # \dontrun{
Eikon <- Refinitiv::EikonConnect()
ex1 <- EikonGetData(
  EikonObject = Eikon, rics = c("MMM", "III.L"),
  Eikonformulas = c(
    "TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/",
    "TR.CompanyName"
  ), verbose = TRUE
)

ex2 <- EikonGetData(
  EikonObject = Eikon, rics = "AAPL.O",
  Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
)

# ex2 will return -1 which is most likely not the current market cap of apple")
# a workaround is to scale back the output to millions

ex2a <- EikonGetData(
  EikonObject = Eikon, rics = "AAPL.O",
  Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/",
  Parameters = list("scale" = 6)
)
# or for more complex formula's
# scale back in the formula itself
ex2b <- EikonGetData(
  EikonObject = Eikon, rics = "AAPL.O",
  Eikonformulas = "TR.CompanyMarketCap(Sdate=0D, scale=6)/*Market Cap*/"
)
} # }

if (FALSE) { # \dontrun{
EikonJson <- RefinitivJsonConnect()
ex1 <- EikonGetData(
  EikonObject = EikonJson, rics = c("MMM", "III.L"),
  Eikonformulas = c(
    "TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/",
    "TR.CompanyName"
  ), verbose = TRUE
)
} # }
```
