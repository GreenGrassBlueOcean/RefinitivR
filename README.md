[![Codecov test coverage](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR/branch/master/graph/badge.svg)](https://codecov.io/gh/GreenGrassBlueOcean/RefinitivR?branch=master)
[![R build status](https://github.com/GreenGrassBlueOcean/RefinitivR/workflows/R-CMD-check/badge.svg)](https://github.com/GreenGrassBlueOcean/RefinitivR/actions)

# RefinitivR
An R interface to Refinitv Eikon and Refinitiv DataStream.

RefinitivR is an R interface to the Eikon Python api using the reticulate package and to DataStream using the DatastreamDSWS2R package. This package is in no way affiliated with Thomson Reuters,Refinitv, Eikon or Datastream. A subscription to EIkon and Datastream is required to use this package. Use of the package is at own risk!
This package uses the DatastreamDSWS2R package from CharlesCara (https://github.com/CharlesCara/DatastreamDSWS2R) for the DataStream Connections.

The reason that this package was developed is that the existing eikonapir package has stability issues leading to dropped api calls. The python api is much more stable and therefore interfacing with the python api will result in a much more stable api connection. Furthermore the package tries to be as robust as possible by automatically chunking long requests in api limit compliant pieces and retries failed requests in order to overcome http 400 errors.

An interface to the R datastream package DatastreamDSWS2R is also provided to allow for easy retrieval of information so that both Eikon and Datastream commands can be used from a single r package.

# Installation of the package
(Run Rstudio with elevated permissions or as Administrator for the installation of the python libraries)
```r
install.packages("devtools")
devtools::install_github("GreenGrassBlueOcean/RefinitivR")
```
load the package
```r
library(Refinitiv)
```
Install the reticulate MiniConda environment r-eikon so that the python module eikon can be used by the r package.
```r
Refinitiv::install_eikon()
```

# Connecting to the Eikonapi

If one wants to use the Python Eikon Package:
```r
Eikon <- EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", Eikonapplication_port = 9000L,
                      PythonModule = "Eikon")
```
If one wants to use  the Python Refinitiv Dataplatform Package (RDP):
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

# Performing a data request
```r
Data <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                     Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", "TR.CompanyName"))
```

## Retrieving large integers with EikonGetData

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
