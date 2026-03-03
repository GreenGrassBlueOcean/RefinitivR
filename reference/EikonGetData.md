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
  verbose = FALSE,
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

## References

<https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item>

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
