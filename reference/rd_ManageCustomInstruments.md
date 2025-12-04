# CREATE, GET, UPDATE or DELETE a custom instrument

CREATE, GET, UPDATE or DELETE a custom instrument

## Usage

``` r
rd_ManageCustomInstruments(
  RDObject = RefinitivJsonConnect(),
  symbol = NULL,
  formula = NULL,
  basket = NULL,
  udc = NULL,
  currency = NULL,
  instrumentName = NULL,
  exchangeName = NULL,
  holidays = NULL,
  timeZone = NULL,
  description = NULL,
  operation = c("CREATE", "GET", "UPDATE", "DELETE"),
  UUID = getOption(".RefinitivUUID"),
  debug = FALSE
)
```

## Arguments

- RDObject:

  Refinitiv Data connection object, defaults to RefinitivJsonConnect()

- symbol:

  character instrument symbol in the format "S)someSymbol.YOURUUID"

- formula:

  formula consisting of rics (fields can be specified by comma)

- basket:

  of rics and weights use CustomInstrumentBasketBuilder()

- udc:

  optional a user defined continuation object as returned by
  CustomInstrumentUDCBuilder()

- currency:

  3-letter code of the currency of the instrument, e.g. GBP

- instrumentName:

  Human-readable name of the instrument. Maximum of 16 characters.

- exchangeName:

  4-letter code of the listing exchange.

- holidays:

  List of custom calendar definitions. use
  CustomInstrumentHolidayBuilder() if required

- timeZone:

  character Time Series uses an odd custom 3-letter value for time zone
  IDs, e.g. "LON" for London.

- description:

  character Free text field from the user to put any notes or text. Up
  to 1000 characters.

- operation:

  character one of "CREATE", "GET", "UPDATE", "DELETE"

- UUID:

  character Eikon UUID can be found here: Eikon Terminal –\> help –\>
  about –\> user details: UUID e.g. ABCDE-123456

- debug:

  boolean prints intermediate api calls to console

## Value

list with data of the result of the request.

## See also

CustomInstrumentBasketBuilder()

CustomInstrumentHolidayBuilder()

CustomInstrumentUDCBuilder()

## Examples

``` r
if (FALSE) { # \dontrun{
 # Create Simple Instrument
 rd_ManageCustomInstruments(operation = "CREATE", symbol = "testAAPLandAMZN",
                              formula = "AAPL.O + AMZN.O")
 #'  #get instrument details
 rd_ManageCustomInstruments(operation = "GET", symbol = "testAAPLandAMZN")

 # Update Instrument formula
 rd_ManageCustomInstruments(operation = "UPDATE", symbol = "testAAPLandAMZN",
                              formula = "AAPL.O + 2 * AMZN.O")
 #Delete
 rd_ManageCustomInstruments(operation = "DELETE", symbol = "testAAPLandAMZN")

#build a custom instrument with a basket
basket <- CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))

rd_ManageCustomInstruments(operation = "CREATE", symbol = "InterestingBasket",
                              basket = basket, currency = "USD")

#update the basket with some holidays
holidays <-  CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
       , reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))


rd_ManageCustomInstruments(operation = "UPDATE", symbol = "InterestingBasket",
                             holidays = holidays )
 #Delete instrument
 rd_ManageCustomInstruments(operation = "DELETE", symbol = "InterestingBasket")

RealInstrumentName <- CorrectCustomInstrument("InterestingBasket")
} # }
```
