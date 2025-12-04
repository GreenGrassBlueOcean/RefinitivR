# GetHistoricalPricing

get historical timeseries from the Refinitiv API

## Usage

``` r
rd_GetHistoricalPricing(
  RDObject = RefinitivJsonConnect(),
  universe = NULL,
  interval = "P1D",
  start = NULL,
  end = NULL,
  adjustments = NULL,
  count = 20L,
  fields = NULL,
  sessions = NULL,
  debug = FALSE,
  SpaceConvertor = "."
)
```

## Arguments

- RDObject:

  connection object, defaults to RefinitivJsonConnect()

- universe:

  The entity universe e.g. RIC name

- interval:

  The consolidation interval in ISO8601. defaults to P1D, see also
  details

- start:

  The start date and timestamp of the query. see also details

- end:

  The end date and timestamp of the query. see also details

- adjustments:

  character vector: The list of adjustment types (comma delimiter) that
  tells the system whether to apply or not apply CORAX (Corporate
  Actions) events or exchange/manual corrections to historical time
  series data. see also details

- count:

  integer The maximum number of data returned

- fields:

  The comma separated list of fields that are to be returned in the
  response (only interday)

- sessions:

  The list of market session classification (comma delimiter) that tells
  the system to return historical time series data based on the market
  session definition (market open/market close)

- debug:

  boolean, if TRUE prints url of get requests

- SpaceConvertor:

  converts spaces in variables name into one of the following characters
  ".", "," , "-", "\_", default is NULL

## Value

data.frame with result

## Details

Additional details on parameters:

\## **RDObject**: The support connection objects are:

- JSON::

  RefinitivJsonConnect

- refinitiv.data::

  RDConnect()

\## **Interval**: The support intervals are:

- Intraday Summaries Intervals:

  PT1M, PT5M, PT10M, PT30M, PT60M, and PT1H

- Interday Summaries Tntervals:

  P1D, P7D, P1W, P1M, P3M, P12M, and P1Y.

When interval is not specified, back-end will return the lowest
supported interday interval.

\## **start** & **end**:

- **Intraday Summaries Interval**:

- **Interday Summaries Tntervals**:

\## **adjustments**: The list of adjustment types (comma delimiter) that
tells the system whether to apply or not apply CORAX (Corporate Actions)
events or exchange/manual corrections to historical time series data.

- **If unspecified:** :

  the response will be controlled by each back-end service with the
  proper adjustments in the response so that the clients know which
  adjustment types are applied by default. In this case, the returned
  data will be applied with exchange and manual corrections as well as
  being applied with CORAX adjustments.

- **If specified:** :

  it means that the clients want to get some specific adjustment types
  applied or even unadjusted.

Normally, the back-end should strictly serve what clients need. However,
if the back-end cannot support them, back-end can still return the form
that the back-end supports with the proper adjustments in the response
together with status block (if applicable) instead of an error message.
The supported values of adjustments :

- unadjusted :

  Not apply both exchange/manual corrections and CORAX

- exchangeCorrection :

  Apply exchange correction adjustment to historical pricing

- manualCorrection :

  Apply manual correction adjustment to historical pricing i.e.
  annotations made by content analysts

- CCH :

  Apply Capital Change adjustment to historical Pricing due to Corporate
  Actions e.g. stock split

- CRE :

  Apply Currency Redenomination adjustment when there is redenomination
  of currency

- RPO :

  Apply Reuters Price Only adjustment to adjust historical price only
  not volume

- RTS :

  Apply Reuters TimeSeries adjustment to adjust both historical price
  and volume

\### Notes:

- 1 :

  Summaries data will always have exchangeCorrection and
  manualCorrection applied. If the request is explicitly asked for
  uncorrected data, a status block will be returned along with the
  corrected data saying "Uncorrected summaries are currently not
  supported".

- 2 :

  unadjusted will be ignored when other values are specified.

\### Limitations: Adjustment behaviors listed in the limitation section
may be changed or improved in the future.

- 1 :

  In case of any combination of correction types is specified (i.e.
  exchangeCorrection or manualCorrection), all correction types will be
  applied to data in applicable event types.

- 2 :

  In case of any combination of CORAX is specified (i.e. CCH, CRE, RPO,
  and RTS), all CORAX will be applied to data in applicable event types.

\## **count**: The maximum number of data returned. If count is smaller
than the total amount of data of the time range specified, some data
(the oldest) will not be delivered. To retrieve all available data with
in the time range specified, this parameter should not be specified. The
returned data could be less than the number requested if there are not
enough data with in the time range specified. If not specified, count
will default to 20 unless both the start and end parameters are also
specified. This parameter has no maximum limit for Interday summaries
interval. The minimum value for this parameter is 1. Negative value is
not supported. See more details on "Start / End / Count Behavior" in
Readme.

## See also

\[translate_to_iso8601_duration()\] for translation from eikon legacy
interval like e.g. 'daily' to 'P1D'

\[mapEikonTimefieldsToRd()\] for translation of eikon timeseries field
names to RD field names.

## Examples

``` r
if (FALSE) { # \dontrun{
# run with python refinitiv data
Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D"
, count = 20L, RDObject = RDConnect(PythonModule = "RD"))

# run with r json
Vodafone2 <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D"
, count = 20L, RDObject = RDConnect(PythonModule = "JSON"))

identical(Vodafone, Vodafone2)

# run wit a subset of fields
Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D", count = 20L
, fields =c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS") )


# test for interday

Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "PT1M", count = 20L
, RDObject = RefinitivJsonConnect())

 # 1 minute - Count - All Sessions
 Vodafone <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                    , interval = "PT1M", count = 500L
                                    , sessions= c("pre","normal","post")
                                    , RDObject = RefinitivJsonConnect())


 # test with custom instrument you need to construct a custom instrument first
 # intraday
 Vodafone <- rd_GetHistoricalPricing( universe = "S)lseg_epam4.ABCDE-123456"
 , interval = "P1D", count = 20)

 # interday
 Vodafone <- rd_GetHistoricalPricing( universe = "S)lseg_epam4.ABCDE-123456"
 , interval = "PT1M", count = 500L)


} # }
```
