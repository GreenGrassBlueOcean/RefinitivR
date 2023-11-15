# Important messages:
1. Python is now optional, the package can now also send direct JSON messages to the terminal.
2. When using the pyton libraries and if you update the package: also run `Refinitiv::install_eikon()` again.
3. Python Eikon and RDP libraries are depricated and can no longer be used. The current commit of the package is reverse compatible with previous commits.

[![Codecov test coverage](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR/branch/master/graph/badge.svg)](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR?branch=master)
[![R build status](https://github.com/GreenGrassBlueOcean/RefinitivR/workflows/R-CMD-check/badge.svg)](https://github.com/GreenGrassBlueOcean/RefinitivR/actions)

# RefinitivR
An R interface to Refinitv Eikon and Refinitiv DataStream.

RefinitivR is an R interface to Refinitiv Eikon by using direct JSON messages or the reticulate package (Python Eikon/RDP api). It also provides access to DataStream using the DatastreamDSWS2R package. This package is in no way affiliated with Thomson Reuters,Refinitv, Eikon, Datastream or LSEG. A subscription to EIkon and Datastream is required to use this package. Use of the package is at own risk!
This package uses the DatastreamDSWS2R package from CharlesCara (https://github.com/CharlesCara/DatastreamDSWS2R) for the DataStream Connections.

The reason that this package was developed is that the existing eikonapir package has stability issues leading to dropped api calls. This package gives multiple options for connecting with the Refinitiv API and retries also dropped api calls which will result in a much more stable api connection. Furthermore the package tries to be as robust as possible by automatically chunking long requests in api limit compliant pieces and retries failed requests in order to overcome http 400 errors.

An interface to the R datastream package DatastreamDSWS2R is also provided to allow for easy retrieval of information so that both Eikon and Datastream commands can be used from a single r package.

```r
install.packages("devtools")
devtools::install_github("GreenGrassBlueOcean/RefinitivR")
```
load the package
```r
library(Refinitiv)
```

# Connecting to Refinitiv directly 
When using the direct JSON method no python installation is necessary:
```r
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "JSON")
RD <- RDConnect(application_id =  "YOUR API KEY",  PythonModule = "JSON")
```

# Connecting to Refinitiv Eikon through the official Refinitiv python API

If one wants to use the Python RD Packages:
Install the reticulate MiniConda environment r-eikon so that the python module RD can be used by the r-package.

Installation of the python packages
(Run Rstudio with elevated permissions or as Administrator for the installation of the python libraries)

```r
Refinitiv::install_eikon()
```

```r
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "RD")
RD <- RDConnect(application_id =  "YOUR API KEY",  PythonModule = "RD")
```
# Working with Refintiv Data (RD) libraries

## Connecting:
(make sure that Eikon/LSEG WorkSpace is running and online)
```r
 RD <- RDConnect('your api key', PythonModule = "JSON")
```
or when you have installed the python environment using `Refinitiv::install_eikon()` you can also use:  
```r
 RD <- RDConnect('your api key', PythonModule = "RD")
```

## Searching:
```r
 test <- RDPsearch(RD, query =  "AAPL.O")
 test <- RDPsearch(RD, query =  "AAPL.O", select = "ContractType,RIC")

 Presidents <- RDPsearch( RD, view = "People", query = 'president'
                        , filter = "startswith(LastName,'H')"
                        , select = 'DocumentTitle'
                        , boost = ''
                        , order_by = 'DocumentTitle asc'
                        , group_by = 'FirstName'
                        , group_count = 2
                        , top = 20
                        )
```

## Get History
```r
timeseries1 <-  rd_GetHistory(RDObject = RD, universe=c("AAPL.O", "NVDA.O"))

Datarequest <- rd_GetHistory( RDObject = RD, universe = c("GOOG.O","AAPL.O")
                            , fields = c("TR.Revenue","TR.GrossProfit")
                            , parameters = list("SDate" = "0CY", "Curn" = "CAD")
                            )
```

## Get Data
```r
ex1 <- rd_GetData( RDObject = RD, rics = c("MMM", "III.L")
                 , Eikonformulas = c( "TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
                                    , "TR.CompanyName")
                 , verbose = TRUE
                 )

 ex2 <- rd_GetData( RDObject = RD, rics = "AAPL.O"
                  , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
                  )
```

## Historical timeseries
### Intraday Timeseries
```r
# Daily - Count
Intraday_vod <- rd_GetHistoricalPricing( universe = "VOD.L", interval = "P1D"
                                       , count = 20L, RDObject = RD)

```
### Interday Timeseries
```r
# 1 minute - Count - All Sessions
Vodafone <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
                                   , interval = "PT1M", count = 500L
                                   , sessions= c("pre","normal","post")
                                   , RDObject = RD
                                   )
```

## Custom Instruments

### Connecting for Custom Instruments

1. Custom Instruments can only be created using JSON connection method.
1. UUID Parameter: Eikon UUID can be found here: Eikon Terminal --> help --> about --> user details: UUID e.g. ABCDE-123456

```r
RDConnect(application_id = NA, PythonModule = "JSON", UUID = "ABCDE-123456")
```


### Create a Simple Instrument:
```r
rd_ManageCustomInstruments( operation = "CREATE", symbol = "testAAPLandAMZN"
                          , formula = "AAPL.O + AMZN.O")
```
Get instrument details
```r
rd_ManageCustomInstruments(operation = "GET", symbol = "testAAPLandAMZN")
```
Update Instrument formula
```r
rd_ManageCustomInstruments( operation = "UPDATE", symbol = "testAAPLandAMZN"
                          , formula = "AAPL.O + 2 * AMZN.O")
```
Delete
```r
rd_ManageCustomInstruments(operation = "DELETE", symbol = "testAAPLandAMZN")
```

### Build a custom instrument out of a basket of instruments
Build the basket:
```r
basket <- CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))
```
Create a custom instrument with the basket":
```r
rd_ManageCustomInstruments(operation = "CREATE", symbol = "InterestingBasket",
                               basket = basket, currency = "USD")
```
Create holidays for the instrument:
```r
holidays <-  CustomInstrumentHolidayBuilder( dates = c("2023-12-01", "2023-12-31")
                                           , reasons = c("Special Bank Holiday 1"
                                                       , "Special Bank Holiday 2")
                                           )
```
Update the basket with some holidays:
```r
rd_ManageCustomInstruments(operation = "UPDATE", symbol = "InterestingBasket",
                           holidays = holidays )
```
Delete instrument:
```r
rd_ManageCustomInstruments(operation = "DELETE", symbol = "InterestingBasket")
```

### Useful add on functions

List all created and active custom instruments:
```r
AllActiveCustomInstruments <- rd_SearchCustomInstruments()
```

Custom instruments come in very specific RIC format. 
To ease the use of the package RefinitivR create this specific format for you.
The following formula allows to retrieve the official custom instrument name (RIC).

```r
RealInstrumentName <- CorrectCustomInstrument("InterestingBasket")
```
## OMM streaming (beta version will still change in future)

Create streaming object
```r
OMM_ws <- create_OMM_Stream()
EUR_stream <- OMM_ws$new(name = "EUR=", fields = c("BID","ASK","OPEN_PRC"))
# start stream
EUR_stream$connect()
# stop stream
EUR_stream$close()
```

# Working with the Legacy Eikon functions

## Connecting
(make sure that Eikon/LSEG WorkSpace is running and online)
```r
  Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "JSON")
```
or when you have installed the python environment using `Refinitiv::install_eikon()` you can also use:  
```r
 Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "RD")
```

## Getting News Stories
Scan the headlines
```r
headlines <- EikonGetNewsHeadlines( EikonObject = Eikon
                                   , query = "R:MSFT.O", count = 2)
```
Get the news stories

```r
stories <- EikonGetNewsStory(story_id = headlines$storyId, EikonObject = Eikon)
```

## Performing a timeseries request

```r
Timeseries <- EikonGetTimeseries( EikonObject = Eikon, rics = c("MMM", "III.L"),
                                , start_date = "2020-01-01T01:00:00",
                                , end_date = paste0(Sys.Date(), "T01:00:00")
                                )
```

## Performing a request for a monthly economic timeseries
```r
  EconTimeSeries <- EikonGetTimeseries( EikonObject = Eikon
                                      , rics = "USCPI=ECI"
                                      , interval = "monthly"
                                      , fields = c()
                                      , start_date = "2020-01-01T01:00:00",
                                      , end_date = paste0(Sys.Date(), "T01:00:00")
                                      )
```


## Performing a data request
```r
Data <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                     Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", "TR.CompanyName"))
```

### Retrieving large integers with EikonGetData (Python only, not for JSON method)

There is currently an issue with the reticulate package handling large integers (like e.g. market capatilization) from python to r.
This issue is described [here](https://github.com/rstudio/reticulate/issues/323) 
and try for yourself [here](https://community.rstudio.com/t/large-integer-conversion-from-python-to-r/82568)

This leads to the following behaviour:

```r
ex2 <- EikonGetData( EikonObject = Eikon, rics = "AAPl.O"
                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
                    )
```

ex2 will return `-1` for the Market capitalization. Which can never be the correct number.
A workaround is to scale back the expected output to a smaller number.
By expressing the market capatalization in millions the produced integer becomes smaller and so prevents the integer of becoming too large.
This can be done by adding a named list in the parameters field: `Parameters = list("scale" = 6)` in which the `6` stands for millions.

```r
ex2a <- EikonGetData( EikonObject = Eikon, rics = "AAPl.O"
                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
                    , Parameters = list("scale" = 6)
                    )
```

Or for more complex formula's scale back in the formula itself by adding `scale = 6` to `TR.CompanyMarketCap`
```r
ex2b <- EikonGetData( EikonObject = Eikon, rics = "AAPl.O"
                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D, scale=6)/*Market Cap*/"
                    )
 ```



# DataStream

```r
DatastreamUserName <- "Your datastream username"
DatastreamPassword <- "Your datastream password"
DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)

DSResult <- DataStream$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                       datatype = "P",
                                       requestDate = "0D")

```
For further details on accessing datastream see https://github.com/CharlesCara/DatastreamDSWS2R.

licensing see LICENSE File!
