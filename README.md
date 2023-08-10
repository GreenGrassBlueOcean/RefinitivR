# Important messages:
1. Python is now optional, the package can now also send direct JSON messages to the terminal. (with exception of streaming data.)
2. When using the pyton libraries and if you update the package: also run `Refinitiv::install_eikon()` again.


[![Codecov test coverage](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR/branch/master/graph/badge.svg)](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR?branch=master)
[![R build status](https://github.com/GreenGrassBlueOcean/RefinitivR/workflows/R-CMD-check/badge.svg)](https://github.com/GreenGrassBlueOcean/RefinitivR/actions)

# RefinitivR
An R interface to Refinitv Eikon and Refinitiv DataStream.

RefinitivR is an R interface to Refinitiv Eikon by using direct JSON messages or the reticulate package (Python Eikon/RDP api). It also provides access to DataStream using the DatastreamDSWS2R package. This package is in no way affiliated with Thomson Reuters,Refinitv, Eikon or Datastream. A subscription to EIkon and Datastream is required to use this package. Use of the package is at own risk!
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

# Connecting to Refinitiv Eikon directly 
When using the direct JSON method no python installation is necessary:
```r
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "JSON")
```
Streaming Prices is not possible with JSON.

# Connecting to Refinitiv Eikon through the official Refinitiv python API

If one wants to use the Python Eikon/RDP Packages:
Install the reticulate MiniConda environment r-eikon so that the python module eikon and RDP can be used by the r-package.
The python refinitiv-data package is not supported.

Installation of the python packages
(Run Rstudio with elevated permissions or as Administrator for the installation of the python libraries)

```r
Refinitiv::install_eikon()
```

```r
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "Eikon")
```
or If one wants to use  the Python Refinitiv Dataplatform Package (RDP):
```r
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", PythonModule = "RDP")
```

# Performing a timeseries request
(make sure that Eikon is running and online)
```r
Timeseries <- EikonGetTimeseries( EikonObject = Eikon, rics = c("MMM", "III.L"),
                                , start_date = "2020-01-01T01:00:00",
                                , end_date = paste0(Sys.Date(), "T01:00:00")
                                )
```

# Performing a request for a monthly economic timeseries
```r
  EconTimeSeries <- EikonGetTimeseries( EikonObject = Eikon
                                      , rics = "USCPI=ECI"
                                      , interval = "monthly"
                                      , fields = c()
                                      , start_date = "2020-01-01T01:00:00",
                                      , end_date = paste0(Sys.Date(), "T01:00:00")
                                      )
```


# Performing a data request
```r
Data <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                     Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", "TR.CompanyName"))
```

## Retrieving large integers with EikonGetData (Python only, not for JSON method)

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

# Working with RDP libraries

Making a search request:
```r
 RDPConnect('your api key', PythonModule = "JSON")
```
or
```r
 RDPConnect('your api key', PythonModule = "RDP")
```

```r
 test <- RDPsearch(query =  "AAPL.O")
 test <- RDPsearch(query =  "AAPL.O", select = "ContractType,RIC")

 Presidents <- RDPsearch( view = "People", query = 'president'
                        , filter = "startswith(LastName,'H')"
                        , select = 'DocumentTitle'
                        , boost = ''
                        , order_by = 'DocumentTitle asc'
                        , group_by = 'FirstName'
                        , group_count = 2
                        , top = 20
                        )
```

Get a streaming price snapshot

```r
rdp <- RDPConnect('your api key', PythonModule = "RDP") # direct JSON not possible
streamObject <- rdp_streaming_prices(rdp, universe = c("EUR=","JPY="), fields = c('DSPLY_NAME', 'BID', 'ASK'))
rdp_get_snapshot(streamObject)
```

# Perfoming a RD request

Currently rd.get_history requests from the python package refinitiv.data are now also possible.
As the get_history command makes 3 seperate json requests this is currently not supported in directjson.
This will change soon.

```r
RDConnect("your api key here")
timeseries1 <-  rd_GetHistory(universe=c("AAPL.O", "NVDA.O"))

Datarequest <- rd_GetHistory(universe = c("GOOG.O","AAPL.O")
                        , fields = c("TR.Revenue","TR.GrossProfit")
                        , parameters = list("SDate" = "0CY", "Curn" = "CAD"))
```
# Performing a DataStream request

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


# Adding a conda binary manually to reticulate

![image](https://github.com/GreenGrassBlueOcean/RefinitivR/assets/43173895/218d0fcc-4c31-48b6-9e49-dfe0926f58d2)



```r
options(reticulate.conda_binary = "path to conda")
```

so in this case:
```r
options(reticulate.conda_binary = "C:\\Users\\XXXX\\AppData\\Local\\miniconda3\\Scripts\\conda.exe")
```
