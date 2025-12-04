# The get_history function allows you to retrieve pricing history, as well as Fundamental and Reference data history through a single function call.

The get_history function allows you to retrieve pricing history, as well
as Fundamental and Reference data history through a single function
call.

## Usage

``` r
rd_GetHistory(
  RD = RDConnect(),
  universe = NULL,
  fields = NULL,
  parameters = NULL,
  interval = NULL,
  start = NULL,
  end = NULL,
  adjustments = NULL,
  count = NULL,
  use_field_names_in_headers = TRUE,
  SpaceConvertor = NULL,
  debug = FALSE
)
```

## Arguments

- RD:

  Refinitiv data object (currently only RDconnect())

- universe:

  Instruments to request str or vector

- fields:

  Fields to request str or vector

- parameters:

  a named key value list for setting parameters, Default: NULL

- interval:

  Date interval. Supported intervals are: \["minute", "1min", "5min",
  "10min", "30min", "60min", "hourly", "1h", "daily", "1d", "1D", "7D",
  "7d", "weekly", "1W", "monthly", "1M", "quarterly", "3M", "6M",
  "yearly", "12M", "1Y"\]

- start:

  The start date and timestamp of the requested history str, date

- end:

  The end date and timestamp of the requested history str, date

- adjustments:

  Tells the system whether to apply or not apply CORAX (Corporate
  Actions) events or exchange/manual corrections or price and volume
  adjustment according to trade/quote qualifier summarization actions to
  historical time series data. Possible values are
  \["exchangeCorrection", "manualCorrection", "CCH", "CRE", "RTS",
  "RPO", "unadjusted", "qualifiers"\]

- count:

  The maximum number of data points returned. Values range: 1 - 10000

- use_field_names_in_headers:

  boolean If True - returns field name as column headers for data
  instead of title, it is advisable to leave this setting to TRUE to
  prevent the issue with two date columns when using specific fields
  like e.g.

- SpaceConvertor:

  converts spaces in variables name into one of the following characters
  ".", "," , "-", "\_", default is NULL, use this for compatability with
  eikon

- debug:

  boolean, default = FALSE, if TRUE, prints out the url used to retrieve
  the data

## Value

data.frame

## Details

This function is currently work in progress and only works with
RDConnect (python), directJson is not available for this function.

\#section regarding adjustments parameters:

The vector of adjustment types (comma delimiter) that tells the system
whether to apply or not apply CORAX (Corporate Actions) events or
exchange/manual corrections to historical time series data.

The supported values of adjustments :

- **"unadjusted"**: Not apply both exchange/manual corrections and CORAX

- **"exchangeCorrection"**: Apply exchange correction adjustment to
  historical pricing

- **"manualCorrection"**: Apply manual correction adjustment to
  historical pricing i.e. annotations made by content analysts

- **"CCH"**: Apply Capital Change adjustment to historical Pricing due
  to Corporate Actions e.g. stock split

- **"CRE"**:Apply Currency Redenomination adjustment when there is
  redenomination of currency

- **"RPO"**:Apply Reuters Price Only adjustment to adjust historical
  price only not volume

- **"RTS"**:Apply Reuters TimeSeries adjustment to adjust both
  historical price and volume

- **"qualifiers"**:Apply price or volume adjustment to historical
  pricing according to trade/quote qualifier summarization actions

## Examples

``` r
if (FALSE) { # \dontrun{
RDObject <-  RDConnect("your api key here", PythonModule = "JSON")
timeseries1 <-  rd_GetHistory(universe=c("AAPL.O", "NVDA.O"))
timeseries2 <- rd_GetHistory(universe="GOOG.O"
                            ,fields = c("BID", "ASK"),interval="tick",count=5)

test <- rd_GetHistory(universe= "AAPL.O"
                     , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)"
                       ,"TR.FreeFloatPct()/100/*FreefloatWeight*/"
                       ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
                       ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")
                     , parameters = list("Curn" = "USD"
                     , "SDate" = "2020-10-27", "EDate" = "2020-12-01"))

test <- rd_GetHistory(universe = c("GOOG.O","AAPL.O")
                       , fields = c("TR.Revenue","TR.GrossProfit")
                       , parameters = list("SDate" = "0CY", "Curn" = "CAD"))
test <-  rd_GetHistory(universe = c("GOOG.O","AAPL.O")
                      , fields = c("TR.PriceTargetMean(SDate:0CY)","TR.LOWPRICE(SDate:0d)"))


test <- rd_GetHistory( universe = c("GOOG.O","MSFT.O","FB.O","AMZN.O")
                     ,fields = c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
                     ,parameters = list("Scale" = 6,"SDate" = 0
                     ,"EDate" = -3,"FRQ" = "FY", "Curn" = "EUR"))
} # }
```
