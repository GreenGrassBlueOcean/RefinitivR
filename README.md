[![Build Status](https://travis-ci.com/GreenGrassBlueOcean/RefinitivR.svg?branch=master)](https://travis-ci.com/GreenGrassBlueOcean/RefinitivR)

# RefinitivR
An R interface to Refinitv Eikon and Refinitiv DataStream.

RefinitivR is an R interface to the Eikon Python api using the reticulate package and to DataStream using the DatastreamDSWS2R package. This package is in no way affiliated with Thomson Reuters,Refinitv, Eikon or Datastream. A subscription to EIkon and Datastream is required to use this package. Use of the package is at own risk!
This package uses of the DatastreamDSWS2R package from CharlesCara (https://github.com/CharlesCara/DatastreamDSWS2R) for the DataStream Connections.

The reason that this package was developed is that the existing eikonapir package has stability issues leading to dropped api calls. The python api is much more stable and therefore interfacing with the python api will result in a much more stable api connection. Furthermore the package tries to be as robust as possible by automatically chunking long requests in api limit compliant pieces and retries failed requests in order to overcome http 400 errors.

An interface to the R datastream package DatastreamDSWS2R is also provided to allow for easy retrieval of information so that both Eikon and Datastream commands can be used from a single r package.

# Installation of the package
(Run Rstudio with elevated permissions or as Administrator for the installation of the python libraries)
```
install.packages("devtools")
devtools::install_github("GreenGrassBlueOcean/RefinitivR")
```
load the package
```
library(Refinitiv)
```
Install the reticulate MiniConda environment r-eikon so that the python module eikon can be used by the r package.
```
Refinitiv::install_eikon()
```

# Connecting to the Eikonapi
```
Eikon <- Refinitiv::EikonConnect(Eikonapplication_id = "YOUR EIKON API KEY", Eikonapplication_port = 9000L)
```

# Performing a timeseries request
(make sure that Eikon is running and online)
```
Timeseries <- EikonGetTimeseries( EikonObject = Eikon, rics = c("MMM", "III.L"),
                                , start_date = "2020-01-01T01:00:00",
                                , end_date = paste0(Sys.Date(), "T01:00:00")
                                )
```

# Performing a data request
```
Data <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
                     Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", "TR.CompanyName"))
```

# Performing a DataStream request

```
DatastreamUserName <- "Your datastream username"
DatastreamPassword <- "Your datastream password"
DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)

DSResult <- DataStream$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                       datatype = "P",
                                       requestDate = "0D")

```
for further details on accesing datastream see https://github.com/CharlesCara/DatastreamDSWS2R

licensing see LICENSE File!
