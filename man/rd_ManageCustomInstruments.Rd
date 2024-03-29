% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd_custominstruments.r
\name{rd_ManageCustomInstruments}
\alias{rd_ManageCustomInstruments}
\title{CREATE, GET, UPDATE or DELETE a custom instrument}
\usage{
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
}
\arguments{
\item{RDObject}{Refinitiv Data connection object, defaults to RefinitivJsonConnect()}

\item{symbol}{character instrument symbol in the format "S)someSymbol.YOURUUID"}

\item{formula}{formula consisting of rics (fields can be specified by comma)}

\item{basket}{of rics and weights use CustomInstrumentBasketBuilder()}

\item{udc}{optional a user defined continuation object as returned by CustomInstrumentUDCBuilder()}

\item{currency}{3-letter code of the currency of the instrument, e.g. GBP}

\item{instrumentName}{Human-readable name of the instrument. Maximum of 16 characters.}

\item{exchangeName}{4-letter code of the listing exchange.}

\item{holidays}{List of custom calendar definitions. use CustomInstrumentHolidayBuilder() if required}

\item{timeZone}{character Time Series uses an odd custom 3-letter value for time zone IDs, e.g. "LON" for London.}

\item{description}{character Free text field from the user to put any notes or text. Up to 1000 characters.}

\item{operation}{character one of "CREATE", "GET", "UPDATE", "DELETE"}

\item{UUID}{character  Eikon UUID can be found here: Eikon Terminal --> help --> about --> user details: UUID e.g. ABCDE-123456}

\item{debug}{boolean prints intermediate api calls to console}
}
\value{
list with data of the result of the request.
}
\description{
CREATE, GET, UPDATE or DELETE a custom instrument
}
\examples{
\dontrun{
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
}
}
\seealso{
CustomInstrumentBasketBuilder()

CustomInstrumentHolidayBuilder()

CustomInstrumentUDCBuilder()
}
