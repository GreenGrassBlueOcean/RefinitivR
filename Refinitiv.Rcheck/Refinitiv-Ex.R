pkgname <- "Refinitiv"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('Refinitiv')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CheckTerminalType")
### * CheckTerminalType

flush(stderr()); flush(stdout())

### Name: CheckTerminalType
### Title: Check LSEG Workspace Connectivity
### Aliases: CheckTerminalType
### Keywords: internal

### ** Examples

## Not run: 
##D CheckTerminalType(verbose = TRUE, force = TRUE)
## End(Not run)




cleanEx()
nameEx("CheckifCustomInstrument")
### * CheckifCustomInstrument

flush(stderr()); flush(stdout())

### Name: CheckifCustomInstrument
### Title: Check if a symbol is really a custom symbol
### Aliases: CheckifCustomInstrument
### Keywords: internal

### ** Examples

## Not run: 
##D CheckifCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
##D CheckifCustomInstrument(symbol = c("test", 'test2') , UUID = "ABCDE-123456")
##D CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")
##D CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = NULL)
## End(Not run)



cleanEx()
nameEx("ConstructTicketJsonBody")
### * ConstructTicketJsonBody

flush(stderr()); flush(stdout())

### Name: ConstructTicketJsonBody
### Title: rewrite JSON body in case a waiting ticket is assigned so that
###   the correct json is requested
### Aliases: ConstructTicketJsonBody
### Keywords: internal

### ** Examples

## Not run: 
##D  see tests for examples
## End(Not run)



cleanEx()
nameEx("CorrectCustomInstrument")
### * CorrectCustomInstrument

flush(stderr()); flush(stdout())

### Name: CorrectCustomInstrument
### Title: Function to check if custom symbol is in format
###   "S)INSTRUMENTSYMBOL.UUID"
### Aliases: CorrectCustomInstrument

### ** Examples

CorrectCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
CorrectCustomInstrument(symbol = "S)test" , UUID = "ABCDE-123456")
CorrectCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")



cleanEx()
nameEx("CustomInstrumentBasketBuilder")
### * CustomInstrumentBasketBuilder

flush(stderr()); flush(stdout())

### Name: CustomInstrumentBasketBuilder
### Title: Build a basket element for a custom instrument
### Aliases: CustomInstrumentBasketBuilder

### ** Examples

## Not run: 
##D CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))
## End(Not run)



cleanEx()
nameEx("CustomInstrumentHolidayBuilder")
### * CustomInstrumentHolidayBuilder

flush(stderr()); flush(stdout())

### Name: CustomInstrumentHolidayBuilder
### Title: Build a holiday object for a custom instrument
### Aliases: CustomInstrumentHolidayBuilder

### ** Examples

CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
, reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))



cleanEx()
nameEx("CustomInstrumentUDCBuilder")
### * CustomInstrumentUDCBuilder

flush(stderr()); flush(stdout())

### Name: CustomInstrumentUDCBuilder
### Title: Build a user defined continuation object for a custom instrument
### Aliases: CustomInstrumentUDCBuilder

### ** Examples

## Not run: 
##D  # no example cause function not implemented yet
## End(Not run)



cleanEx()
nameEx("DataStreamConnect")
### * DataStreamConnect

flush(stderr()); flush(stdout())

### Name: DataStreamConnect
### Title: Initialize DataStream Connection
### Aliases: DataStreamConnect

### ** Examples

## Not run: 
##D DatastreamUserName <- "Your datastream username"
##D DatastreamPassword <- "Your datastream password"
##D DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)
## End(Not run)



cleanEx()
nameEx("EikonChunker")
### * EikonChunker

flush(stderr()); flush(stdout())

### Name: EikonChunker
### Title: Returns a list of chunked Rics so that api limits can be
###   satisfied
### Aliases: EikonChunker
### Keywords: internal

### ** Examples

## Not run: "internal function no examples"



cleanEx()
nameEx("EikonConnect")
### * EikonConnect

flush(stderr()); flush(stdout())

### Name: EikonConnect
### Title: Connect to LSEG Workspace via JSON
### Aliases: EikonConnect

### ** Examples

## Not run: 
##D # Zero-argument connection (recommended):
##D Eikon <- EikonConnect()
##D 
##D # Explicit key:
##D Eikon <- EikonConnect(Eikonapplication_id = "your key")
## End(Not run)



cleanEx()
nameEx("EikonGetData")
### * EikonGetData

flush(stderr()); flush(stdout())

### Name: EikonGetData
### Title: Function to obtain data from Eikon via the JSON API
### Aliases: EikonGetData

### ** Examples

## Not run: 
##D Eikon <- Refinitiv::EikonConnect()
##D ex1 <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
##D              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
##D              , "TR.CompanyName"), verbose = TRUE)
##D 
##D ex2 <- EikonGetData( EikonObject = Eikon, rics = "AAPL.O"
##D                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
##D                    )
##D 
##D # ex2 will return -1 which is most likely not the current market cap of apple")
##D # a workaround is to scale back the output to millions
##D 
##D ex2a <- EikonGetData( EikonObject = Eikon, rics = "AAPL.O"
##D                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
##D                    , Parameters = list("scale" = 6)
##D                    )
##D # or for more complex formula's
##D # scale back in the formula itself
##D ex2b <- EikonGetData( EikonObject = Eikon, rics = "AAPL.O"
##D                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D, scale=6)/*Market Cap*/"
##D                    )
## End(Not run)


## Not run: 
##D EikonJson <- RefinitivJsonConnect()
##D ex1 <- EikonGetData(EikonObject = EikonJson, rics = c("MMM", "III.L"),
##D              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
##D              , "TR.CompanyName"), verbose = TRUE)
##D 
## End(Not run)





cleanEx()
nameEx("EikonGetNewsHeadlines")
### * EikonGetNewsHeadlines

flush(stderr()); flush(stdout())

### Name: EikonGetNewsHeadlines
### Title: Returns a list of news headlines
### Aliases: EikonGetNewsHeadlines

### ** Examples

## Not run: 
##D  Eikon <- Refinitiv::EikonConnect()
##D  headlines <- EikonGetNewsHeadlines( EikonObject = Eikon
##D                                    , query = c("R:MSFT.O", "R:AAPL.O") , count = 2, debug = TRUE)
## End(Not run)

## Not run: 
##D   EikonJson <- RefinitivJsonConnect()
##D   headlines <- EikonGetNewsHeadlines( EikonObject = EikonJson, debug = TRUE
##D                                     , query = "R:MSFT.O", count = 2)
## End(Not run)



cleanEx()
nameEx("EikonGetNewsStory")
### * EikonGetNewsStory

flush(stderr()); flush(stdout())

### Name: EikonGetNewsStory
### Title: Return a single news story corresponding to the identifier
###   provided in story_id
### Aliases: EikonGetNewsStory

### ** Examples

## Not run: 
##D  EikonJson <- RefinitivJsonConnect()
##D  headlines <- EikonGetNewsHeadlines(EikonObject = EikonJson
##D                                    , query = "R:MSFT.O", count = 2)
##D  stories <- EikonGetNewsStory(story_id = headlines$storyId
##D  , EikonObject = EikonJson)
##D 
## End(Not run)

## Not run: 
##D   Eikon <- Refinitiv::EikonConnect()
##D   story_id <- "urn:newsml:newswire.refinitiv.com:20230829:nRTVm1b2r:5"
##D   stories_RD <- EikonGetNewsStory(story_id = story_id
##D   , EikonObject = Eikon, debug = TRUE, raw_output  = FALSE)
##D 
##D   EikonJson <- RefinitivJsonConnect()
##D   stories_JSON <- EikonGetNewsStory(story_id = story_id
##D   , EikonObject = EikonJson, debug = TRUE, raw_output  = FALSE)
##D 
##D  identical(stories_RD, stories_JSON)
## End(Not run)



cleanEx()
nameEx("EikonGetSymbology")
### * EikonGetSymbology

flush(stderr()); flush(stdout())

### Name: EikonGetSymbology
### Title: Returns a list of instrument names converted into another
###   instrument code. For example: convert SEDOL instrument names to RIC
###   names
### Aliases: EikonGetSymbology

### ** Examples

## Not run: 
##D Eikon <- Refinitiv::EikonConnect()
##D ex1 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "AAPL.O"
##D  , to_symbol_type = "ISIN" )
##D ex2 <- EikonGetSymbology(EikonObject = Eikon
##D , symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
##D ,  to_symbol_type = "RIC" , verbose = TRUE)
##D ex3 <- EikonGetSymbology(EikonObject = Eikon
##D , symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
##D ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
##D ex4 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.AS"
##D , to_symbol_type = "ISIN"  , verbose = TRUE)
##D ex5 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.L"
##D , to_symbol_type = "ISIN"  , verbose = TRUE)
##D ex6 <- EikonGetSymbology(EikonObject = Eikon
##D , symbol =  c("GB00B03MLX29", "NL0015476987"), from_symbol_type = "ISIN"
##D ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
##D ex7 <- EikonGetSymbology(EikonObject = Eikon
##D , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
##D ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
## End(Not run)

## Not run: 
##D  EikonJson <- RefinitivJsonConnect()
##D  ex1 <- EikonGetSymbology(EikonObject = EikonJson, symbol =  "AAPL.O"
##D  , to_symbol_type = "ISIN" )
## End(Not run)






cleanEx()
nameEx("EikonGetTimeseries")
### * EikonGetTimeseries

flush(stderr()); flush(stdout())

### Name: EikonGetTimeseries
### Title: Function to obtain timeseries from Eikon via the JSON API
### Aliases: EikonGetTimeseries

### ** Examples

## Not run: 
##D Eikon <- Refinitiv::EikonConnect()
##D ex1 <- EikonGetTimeseries(EikonObject = Eikon, rics = c("MMM", "III.L"),
##D                    start_date = "2020-01-01T01:00:00",
##D                    end_date = paste0(Sys.Date(), "T01:00:00"), verbose = TRUE)
## End(Not run)

## Not run: 
##D   EikonJson <- RefinitivJsonConnect()
##D   ex1 <- EikonGetTimeseries(EikonObject = EikonJson, rics = c("MMM", "III.L"),
##D                    start_date = "2020-01-01T01:00:00",
##D                    end_date = paste0(Sys.Date(), "T01:00:00"), verbose = TRUE)
## End(Not run)








cleanEx()
nameEx("EikonNameCleaner")
### * EikonNameCleaner

flush(stderr()); flush(stdout())

### Name: EikonNameCleaner
### Title: Convert Eikon formula's in human readable names
### Aliases: EikonNameCleaner
### Keywords: internal

### ** Examples

Refinitiv:::EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC"))



cleanEx()
nameEx("EikonPostProcessor")
### * EikonPostProcessor

flush(stderr()); flush(stdout())

### Name: EikonPostProcessor
### Title: A postprocessor to process Eikon get_data output into R
###   data.frames
### Aliases: EikonPostProcessor
### Keywords: internal

### ** Examples

## Not run: "internal function no examples"



cleanEx()
nameEx("EikonRepairMic")
### * EikonRepairMic

flush(stderr()); flush(stdout())

### Name: EikonRepairMic
### Title: Repair Market Identifier Codes (MIC) utilizing RDN Exchange
###   Identifiers
### Aliases: EikonRepairMic

### ** Examples

## Not run: 
##D # Standard usage in a data fetching pipeline
##D rics <- c("WM", "AAPL.O", "VOD.L")
##D fields <- c("TR.RDNExchangeCode", "TR.OperatingMIC", "TR.CompanyName")
##D 
##D raw_data <- EikonGetData(Eikon, rics, fields)$PostProcessedEikonGetData
##D 
##D # WM often returns "XXXX" for OperatingMIC; this fixes it using RDNExchangeCode
##D clean_data <- EikonRepairMic(raw_data, verbose = TRUE)
## End(Not run)



cleanEx()
nameEx("EikonShowAttributes")
### * EikonShowAttributes

flush(stderr()); flush(stdout())

### Name: EikonShowAttributes
### Title: Show the available methods of a Refinitiv connection object
### Aliases: EikonShowAttributes
### Keywords: internal

### ** Examples

## Not run: 
##D Eikon <- EikonConnect(Eikonapplication_id = "your key")
##D EikonShowAttributes(EikonObject = Eikon)
## End(Not run)



cleanEx()
nameEx("EikonTimeSeriesPreprocessor")
### * EikonTimeSeriesPreprocessor

flush(stderr()); flush(stdout())

### Name: EikonTimeSeriesPreprocessor
### Title: Preprocessor for Eikon Get timeseries to automatically chunk
###   pieces in the required length
### Aliases: EikonTimeSeriesPreprocessor
### Keywords: internal

### ** Examples

test <- Refinitiv:::EikonTimeSeriesPreprocessor(interval = "daily"
, rics = rep(letters, 1000), start_date = "2015-01-01", end_date = "2018-01-01")



cleanEx()
nameEx("InspectRequest")
### * InspectRequest

flush(stderr()); flush(stdout())

### Name: InspectRequest
### Title: function to check if a downloaded dataframe is empty
### Aliases: InspectRequest
### Keywords: internal

### ** Examples

Refinitiv:::InspectRequest(data.frame(), functionname = "test")
Refinitiv:::InspectRequest(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test")



cleanEx()
nameEx("PostProcessTimeSeriesRequest")
### * PostProcessTimeSeriesRequest

flush(stderr()); flush(stdout())

### Name: PostProcessTimeSeriesRequest
### Title: Postprocessor for raw Timeseries Requests
### Aliases: PostProcessTimeSeriesRequest
### Keywords: internal

### ** Examples

## Not run: 
##D RawTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
##D rics = c("MMM"),
##D start_date = "2020-01-01T01:00:00",
##D end_date = "2020-01-10T01:00:00",
##D fields = "CLOSE", raw = TRUE))1
##D PostProcessTimeSeriesRequest(RawTimeSeries)
## End(Not run)



cleanEx()
nameEx("ProcessIPAOutput")
### * ProcessIPAOutput

flush(stderr()); flush(stdout())

### Name: ProcessIPAOutput
### Title: Process output of RDP IPA calls
### Aliases: ProcessIPAOutput
### Keywords: internal

### ** Examples

## Not run: 
##D ipa_output <- RDPGetOptionAnalytics(
##D  OptionRics = c("AAPLL032112500.U", "AAPLL032113700.U")
##D , raw = TRUE)
##D ProcessIPAOutput(ipa_output)
## End(Not run)



cleanEx()
nameEx("ProcessSymbology")
### * ProcessSymbology

flush(stderr()); flush(stdout())

### Name: ProcessSymbology
### Title: Function to process raw output of get_symbology to a more
###   R-readable format
### Aliases: ProcessSymbology
### Keywords: internal

### ** Examples

## Not run: 
##D Raw_output_No_BestMatch <- EikonGetSymbology(EikonObject = Eikon
##D , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
##D , to_symbol_type = "RIC" , raw_output = TRUE, bestMatch = FALSE  )
##D ProcessSymbology(EikonSymbologyResult = Raw_output_No_BestMatch
##D , from_symbol_type = "ISIN", to_symbol_type = "RIC")
##D 
##D Raw_output_BestMatch <- EikonGetSymbology(EikonObject = Eikon
##D , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
##D , to_symbol_type = "RIC" , raw_output = TRUE, bestMatch = TRUE  )
##D ProcessSymbology(EikonSymbologyResult = Raw_output_BestMatch
##D , from_symbol_type = "ISIN", to_symbol_type = "RIC")
## End(Not run)



cleanEx()
nameEx("PropertiesActiveRefinitivObject")
### * PropertiesActiveRefinitivObject

flush(stderr()); flush(stdout())

### Name: PropertiesActiveRefinitivObject
### Title: Function to check which Refinitiv connection method is active
### Aliases: PropertiesActiveRefinitivObject
### Keywords: internal

### ** Examples

## Not run: 
##D test <- PropertiesActiveRefinitivObject()
## End(Not run)



cleanEx()
nameEx("RDConnect")
### * RDConnect

flush(stderr()); flush(stdout())

### Name: RDConnect
### Title: Connect to LSEG / Refinitiv Data via JSON
### Aliases: RDConnect RDPConnect

### ** Examples

## Not run: 
##D # Zero-argument connection (recommended):
##D rd <- RDConnect()
##D 
##D # Explicit key:
##D rd <- RDConnect(application_id = "your key")
## End(Not run)
## Not run: 
##D rd <- RDPConnect()
## End(Not run)



cleanEx()
nameEx("RDPShowAvailableSearchViews")
### * RDPShowAvailableSearchViews

flush(stderr()); flush(stdout())

### Name: RDPShowAvailableSearchViews
### Title: Show Available Search Views for searchView parameter in
###   RDPget_search_metadata
### Aliases: RDPShowAvailableSearchViews

### ** Examples

RDPShowAvailableSearchViews()



cleanEx()
nameEx("RDPget_search_metadata")
### * RDPget_search_metadata

flush(stderr()); flush(stdout())

### Name: RDPget_search_metadata
### Title: Get search metadata from RDP
### Aliases: RDPget_search_metadata

### ** Examples

## Not run: 
##D test_json <- RDPget_search_metadata(RDP = RefinitivJsonConnect()
##D                                    , searchView = "EquityQuotes")
## End(Not run)



cleanEx()
nameEx("RDPsearch")
### * RDPsearch

flush(stderr()); flush(stdout())

### Name: RDPsearch
### Title: RDP search function is a wrapper for the pyton rdp.search
###   function
### Aliases: RDPsearch

### ** Examples

## Not run: 
##D RDConnect('your api key')
##D test <- RDPsearch(query =  "AAPL.O", select = "ContractType,RIC")
##D 
##D Presidents <- RDPsearch( view = "People", query = 'president'
##D                        , filter = "startswith(LastName,'H')"
##D                        , select = 'DocumentTitle'
##D                        , boost = ''
##D                        , order_by = 'DocumentTitle asc'
##D                        , group_by = 'FirstName'
##D                        , group_count = 2
##D                        , top = 20
##D                        , navigators = 'HullType'
##D                        , features = 'spell' )
##D 
##D reporates <- RDPsearch( view = "IndicatorQuotes"
##D                       , query = "repo rate", group_by = "CentralBankName"
##D                       , group_count = 3
##D                       , select = paste0("CentralBankName,DocumentTitle,"
##D                                        ,"RIC,ObservationValue")
##D                       , top = 1000)
##D 
##D EquitiesSearch <-  RDPsearch( view = "EquityQuotes"
##D                             , filter = paste0("Eps gt 6.0 and "
##D                                       , "RCSTRBC2012Name eq 'Personal & "
##D                                       , "Household Products & Services' "
##D                                       , "and MktCapTotalUsd gt 100000000 "
##D                                       , "and IsPrimaryRIC eq true")
##D                             , top =  10000
##D                             , select = paste0("DocumentTitle , RIC, Eps,"
##D                                              ," MktCapTotalUsd"))
##D 
##D 
##D Vessels <- RDPsearch( view = "VesselPhysicalAssets"
##D                     , filter = paste0( "RCSAssetTypeLeaf eq 'tanker'"
##D                                , " and RCSRegionLeaf eq 'Gulf of Mexico'")
##D                     , top =  10000
##D                     , navigators = "OriginPort"
##D                     , select = paste0( "DocumentTitle,RIC,OriginPort"
##D                                      , " ,DestinationPort,RCSFlagLeaf"
##D                                      , ",AssetName,AISStatus,"
##D                                      , "VesselCurrentPortRIC,IMO")
##D                     )
##D 
##D 
##D ListedSearch <- RDPsearch(Arglist = list(query = "president", view = "People"))
##D 
##D SearchQuery = "aapl.o"
##D ListedSearch <- RDPsearch(query = SearchQuery)
## End(Not run)

## Not run: 
##D   SearchQuery = "aapl.o"
##D   ListedSearch <- RDPsearch(RDP = RefinitivJsonConnect(), query = SearchQuery)
##D 
## End(Not run)



cleanEx()
nameEx("RefinitivJsonConnect")
### * RefinitivJsonConnect

flush(stderr()); flush(stdout())

### Name: RefinitivJsonConnect
### Title: Create a JSON connection object for the LSEG Workspace proxy
### Aliases: RefinitivJsonConnect

### ** Examples

## Not run: 
##D conn <- RefinitivJsonConnect()
## End(Not run)



cleanEx()
nameEx("TR_Field")
### * TR_Field

flush(stderr()); flush(stdout())

### Name: TR_Field
### Title: Helper function to build the Eikonformulas parameter for the
###   EikonGetData function.
### Aliases: TR_Field

### ** Examples

TR_Field(Field_name = 'tr.revenue')
TR_Field(Field_name ='tr.open', sort_dir ='asc', sort_priority = 1)
TR_Field(Field_name ='TR.GrossProfit', Parameters = list('Scale' = 6, 'Curn'= 'EUR')
        , sort_dir = 'asc', sort_priority = 0)



cleanEx()
nameEx("TestDataStreamCredentials")
### * TestDataStreamCredentials

flush(stderr()); flush(stdout())

### Name: TestDataStreamCredentials
### Title: Test if the DataStream Credentials are valid
### Aliases: TestDataStreamCredentials

### ** Examples

TestDataStreamCredentials(DatastreamUsername = "wrongusername"
, DatastreamPassword = "wrongPassword")



cleanEx()
nameEx("build_get_query_string")
### * build_get_query_string

flush(stderr()); flush(stdout())

### Name: build_get_query_string
### Title: Build GET Query String
### Aliases: build_get_query_string
### Keywords: internal

### ** Examples

## Not run: 
##D   query_list <- list(
##D     query    = "R:TSLA.O AND Language:EN",
##D     limit    = 5,
##D     dateFrom = "2023-01-01T00:00:00Z",
##D     extra    = NULL
##D   )
##D   qs <- build_get_query_string(query_list)
##D   # qs will be:
##D   # "?query=R%3ATSLA.O%20AND%20Language%3AEN&limit=5&dateFrom=2023-01-01T00%3A00%3A00Z"
##D 
##D   # Passing an empty list returns an empty string:
##D   build_get_query_string(list())
## End(Not run)




cleanEx()
nameEx("flattenNestedlist")
### * flattenNestedlist

flush(stderr()); flush(stdout())

### Name: flattenNestedlist
### Title: Flatten a nested list put list format in data.table format way
###   (don't mix up lists and vectors in one nested list)
### Aliases: flattenNestedlist
### Keywords: internal

### ** Examples

# Refinitiv:::flattenNestedlist(list(list("a", "b"), c(1,2)))



cleanEx()
nameEx("get_rdp_streaming_url")
### * get_rdp_streaming_url

flush(stderr()); flush(stdout())

### Name: get_rdp_streaming_url
### Title: Show all available custom instruments that have been created
### Aliases: get_rdp_streaming_url
### Keywords: internal

### ** Examples

## Not run: 
##D test <- get_rdp_streaming_url()
## End(Not run)



cleanEx()
nameEx("mapEikonTimefieldsToRd")
### * mapEikonTimefieldsToRd

flush(stderr()); flush(stdout())

### Name: mapEikonTimefieldsToRd
### Title: Map Eikon Time Fields to Refinitiv Data (RD) Fields
### Aliases: mapEikonTimefieldsToRd
### Keywords: internal

### ** Examples

Refinitiv:::mapEikonTimefieldsToRd()
Refinitiv:::mapEikonTimefieldsToRd(c("TIMESTAMP", "UNKNOWN_COLUMN", "CLOSE"))




cleanEx()
nameEx("rd_CacheInfo")
### * rd_CacheInfo

flush(stderr()); flush(stdout())

### Name: rd_CacheInfo
### Title: Show RefinitivR Cache Statistics
### Aliases: rd_CacheInfo

### ** Examples

rd_CacheInfo()



cleanEx()
nameEx("rd_ClearCache")
### * rd_ClearCache

flush(stderr()); flush(stdout())

### Name: rd_ClearCache
### Title: Clear the RefinitivR Session Cache
### Aliases: rd_ClearCache

### ** Examples

rd_ClearCache()



cleanEx()
nameEx("rd_GetData")
### * rd_GetData

flush(stderr()); flush(stdout())

### Name: rd_GetData
### Title: Function to obtain data from Eikon/LSEG via the JSON API
### Aliases: rd_GetData

### ** Examples

## Not run: 
##D Refinitiv <- RDConnect()
##D ex1 <- rd_GetData(RDObject = Refinitiv, rics = c("MMM", "III.L"),
##D              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
##D              , "TR.CompanyName"), verbose = TRUE)
##D 
##D ex2 <- rd_GetData( RDObject = Refinitiv, rics = "AAPL.O"
##D                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
##D                    )
##D 
##D  rics <- c("AAPL.O")
##D  fields <- c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
##D             ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
##D             ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")
##D 
##D  parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01", "Fill" ="None")
##D  test_json <- rd_GetData( RDObject = Refinitiv
##D                         , rics =  rics
##D                         , Eikonformulas =  fields
##D                         , Parameters = parameters
##D                         , use_field_names_in_headers = TRUE
##D                         , SyncFields = FALSE
##D                         )
## End(Not run)



cleanEx()
nameEx("rd_GetESG")
### * rd_GetESG

flush(stderr()); flush(stdout())

### Name: rd_GetESG
### Title: Retrieve ESG Data from LSEG
### Aliases: rd_GetESG

### ** Examples

## Not run: 
##D   # Full ESG scores for Apple and Microsoft
##D   rd_GetESG(universe = c("AAPL.O", "MSFT.O"))
##D 
##D   # Standard measures for a specific year range
##D   rd_GetESG(universe = "AAPL.O", view = "measures-standard",
##D             start = 2020, end = 2023)
##D 
##D   # Basic overview
##D   rd_GetESG(universe = "IBM.N", view = "basic")
## End(Not run)




cleanEx()
nameEx("rd_GetEstimates")
### * rd_GetEstimates

flush(stderr()); flush(stdout())

### Name: rd_GetEstimates
### Title: Retrieve Estimates Data from LSEG
### Aliases: rd_GetEstimates

### ** Examples

## Not run: 
##D   # Annual summary estimates for IBM (basic package)
##D   rd_GetEstimates(universe = "IBM.N", package = "basic")
##D 
##D   # Analyst recommendations for multiple companies
##D   rd_GetEstimates(universe = c("AAPL.O", "MSFT.O"),
##D                   view = "view-summary/recommendations",
##D                   package = "standard")
##D 
##D   # KPI actuals (no package needed)
##D   rd_GetEstimates(universe = "TSLA.O",
##D                   view = "view-actuals-kpi/annual")
## End(Not run)




cleanEx()
nameEx("rd_GetHistoricalPricing")
### * rd_GetHistoricalPricing

flush(stderr()); flush(stdout())

### Name: rd_GetHistoricalPricing
### Title: GetHistoricalPricing
### Aliases: rd_GetHistoricalPricing

### ** Examples

## Not run: 
##D # run with a subset of fields
##D Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D", count = 20L
##D , fields =c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS") )
##D 
##D 
##D # test for interday
##D 
##D Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "PT1M", count = 20L
##D , RDObject = RefinitivJsonConnect())
##D 
##D  # 1 minute - Count - All Sessions
##D  Vodafone <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
##D                                     , interval = "PT1M", count = 500L
##D                                     , sessions= c("pre","normal","post")
##D                                     , RDObject = RefinitivJsonConnect())
##D 
##D 
##D  # test with custom instrument you need to construct a custom instrument first
##D  # intraday
##D  Vodafone <- rd_GetHistoricalPricing( universe = "S)lseg_epam4.ABCDE-123456"
##D  , interval = "P1D", count = 20)
##D 
##D  # interday
##D  Vodafone <- rd_GetHistoricalPricing( universe = "S)lseg_epam4.ABCDE-123456"
##D  , interval = "PT1M", count = 500L)
##D 
##D 
## End(Not run)



cleanEx()
nameEx("rd_GetHistory")
### * rd_GetHistory

flush(stderr()); flush(stdout())

### Name: rd_GetHistory
### Title: The get_history function allows you to retrieve pricing history,
###   as well as Fundamental and Reference data history through a single
###   function call.
### Aliases: rd_GetHistory

### ** Examples

## Not run: 
##D RDObject <- RDConnect("your api key here")
##D timeseries1 <- rd_GetHistory(universe=c("AAPL.O", "NVDA.O"))
##D timeseries2 <- rd_GetHistory(universe="GOOG.O"
##D                             ,fields = c("BID", "ASK"),interval="tick",count=5)
##D 
##D test <- rd_GetHistory(universe= "AAPL.O"
##D                      , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)"
##D                        ,"TR.FreeFloatPct()/100/*FreefloatWeight*/"
##D                        ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
##D                        ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")
##D                      , parameters = list("Curn" = "USD"
##D                      , "SDate" = "2020-10-27", "EDate" = "2020-12-01"))
##D 
##D test <- rd_GetHistory(universe = c("GOOG.O","AAPL.O")
##D                        , fields = c("TR.Revenue","TR.GrossProfit")
##D                        , parameters = list("SDate" = "0CY", "Curn" = "CAD"))
##D test <-  rd_GetHistory(universe = c("GOOG.O","AAPL.O")
##D                       , fields = c("TR.PriceTargetMean(SDate:0CY)","TR.LOWPRICE(SDate:0d)"))
##D 
##D 
##D test <- rd_GetHistory( universe = c("GOOG.O","MSFT.O","FB.O","AMZN.O")
##D                      ,fields = c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
##D                      ,parameters = list("Scale" = 6,"SDate" = 0
##D                      ,"EDate" = -3,"FRQ" = "FY", "Curn" = "EUR"))
## End(Not run)



cleanEx()
nameEx("rd_GetOwnership")
### * rd_GetOwnership

flush(stderr()); flush(stdout())

### Name: rd_GetOwnership
### Title: Retrieve Ownership Data from LSEG
### Aliases: rd_GetOwnership

### ** Examples

## Not run: 
##D   # Consolidated investor breakdown by type
##D   rd_GetOwnership(universe = "AAPL.O",
##D                   view = "consolidated/breakdown",
##D                   stat_type = 1)
##D 
##D   # Top fund holders (first 50)
##D   rd_GetOwnership(universe = "MSFT.O",
##D                   view = "fund/investors",
##D                   limit = 50)
##D 
##D   # Insider transactions
##D   rd_GetOwnership(universe = "TSLA.O",
##D                   view = "insider/transaction-report")
##D 
##D   # Organization info
##D   rd_GetOwnership(universe = "IBM.N", view = "org-info")
## End(Not run)




cleanEx()
nameEx("rd_ManageCustomInstruments")
### * rd_ManageCustomInstruments

flush(stderr()); flush(stdout())

### Name: rd_ManageCustomInstruments
### Title: CREATE, GET, UPDATE or DELETE a custom instrument
### Aliases: rd_ManageCustomInstruments

### ** Examples

## Not run: 
##D  # Create Simple Instrument
##D  rd_ManageCustomInstruments(operation = "CREATE", symbol = "testAAPLandAMZN",
##D                               formula = "AAPL.O + AMZN.O")
##D  #'  #get instrument details
##D  rd_ManageCustomInstruments(operation = "GET", symbol = "testAAPLandAMZN")
##D 
##D  # Update Instrument formula
##D  rd_ManageCustomInstruments(operation = "UPDATE", symbol = "testAAPLandAMZN",
##D                               formula = "AAPL.O + 2 * AMZN.O")
##D  #Delete
##D  rd_ManageCustomInstruments(operation = "DELETE", symbol = "testAAPLandAMZN")
##D 
##D #build a custom instrument with a basket
##D basket <- CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))
##D 
##D rd_ManageCustomInstruments(operation = "CREATE", symbol = "InterestingBasket",
##D                               basket = basket, currency = "USD")
##D 
##D #update the basket with some holidays
##D holidays <-  CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
##D        , reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))
##D 
##D 
##D rd_ManageCustomInstruments(operation = "UPDATE", symbol = "InterestingBasket",
##D                              holidays = holidays )
##D  #Delete instrument
##D  rd_ManageCustomInstruments(operation = "DELETE", symbol = "InterestingBasket")
##D 
##D RealInstrumentName <- CorrectCustomInstrument("InterestingBasket")
## End(Not run)



cleanEx()
nameEx("rd_OutputProcesser")
### * rd_OutputProcesser

flush(stderr()); flush(stdout())

### Name: rd_OutputProcesser
### Title: Process output from refintiv data to r data.frame output
### Aliases: rd_OutputProcesser
### Keywords: internal

### ** Examples

## Not run: 
##D  EndPoint = "data/datagrid/beta1/"
##D  payload <- list( 'universe'= as.list(c("GOOG.O", "NVDA.O"))
##D                , 'fields'= as.list(c('TR.CLOSE', 'TR.OPEN'))
##D                , 'parameters'=list('SDate'= '2022-10-05', 'EDate'= '2022-11-05')
##D                , 'output'= 'Col,T|Va,Row,In,date|'
##D                )
##D 
##D response <- send_json_request(json = payload, service = "rdp"
##D , EndPoint = EndPoint, request_type = "POST")
##D Output <- rd_OutputProcesser(response)
## End(Not run)



cleanEx()
nameEx("rd_SearchCustomInstruments")
### * rd_SearchCustomInstruments

flush(stderr()); flush(stdout())

### Name: rd_SearchCustomInstruments
### Title: Show all available custom instruments that have been created
### Aliases: rd_SearchCustomInstruments

### ** Examples

## Not run: 
##D test <- rd_SearchCustomInstruments()
## End(Not run)



cleanEx()
nameEx("rd_VerifyToken")
### * rd_VerifyToken

flush(stderr()); flush(stdout())

### Name: rd_VerifyToken
### Title: Verify the Validity of a JWT Access Token
### Aliases: rd_VerifyToken
### Keywords: internal

### ** Examples

## Not run: 
##D # Example token (replace with a real token)
##D access_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
##D is_valid <- rd_VerifyToken(access_token)
##D print(is_valid)
## End(Not run)




cleanEx()
nameEx("rd_check_proxy_url")
### * rd_check_proxy_url

flush(stderr()); flush(stdout())

### Name: rd_check_proxy_url
### Title: Check if the LSEG Workspace proxy is alive
### Aliases: rd_check_proxy_url
### Keywords: internal

### ** Examples

## Not run: 
##D test <- rd_check_proxy_url(port = 9000)
## End(Not run)



cleanEx()
nameEx("rd_connection")
### * rd_connection

flush(stderr()); flush(stdout())

### Name: rd_connection
### Title: Get or create the default Refinitiv connection
### Aliases: rd_connection

### ** Examples

## Not run: 
##D # Most users never need to call this directly.
##D # All API functions default to rd_connection():
##D data <- EikonGetData(rics = "AAPL.O", Eikonformulas = "TR.CompanyName")
##D 
##D # Force re-creation after terminal restart:
##D rd_connection(reset = TRUE)
## End(Not run)



cleanEx()
nameEx("rd_get_streaming_data")
### * rd_get_streaming_data

flush(stderr()); flush(stdout())

### Name: rd_get_streaming_data
### Title: Get streaming data from Refinitiv
### Aliases: rd_get_streaming_data

### ** Examples

## Not run: 
##D # Simple usage with callbacks
##D stream <- rd_get_streaming_data(
##D   universe = c("EUR=", "GBP=", "JPY="),
##D   fields = c("BID", "ASK", "DSPLY_NAME"),
##D   on_update = function(stream, instrument, fields) {
##D     cat("Update for", instrument, ":\n")
##D     print(fields)
##D   }
##D )
##D 
##D # Start streaming
##D stream$open()
##D 
##D # Later: stop streaming
##D stream$close()
## End(Not run)

## Not run: 
##D # With session integration
##D RD <- RDConnect(application_id = "your_key")
##D stream <- rd_get_streaming_data(
##D   universe = "AAPL.O",
##D   fields = c("BID", "ASK"),
##D   RDObject = RD
##D )
##D stream$open()
## End(Not run)

## Not run: 
##D # Live plotting
##D stream <- rd_get_streaming_data(
##D   universe = "EUR=",
##D   fields = c("BID", "ASK")
##D )
##D stream$open()
##D 
##D # Create and run live plot
##D app <- stream$plot_live(field = "BID")
##D shiny::runApp(app)  # Opens in browser
## End(Not run)

## Not run: 
##D # Get summary statistics
##D stream <- rd_get_streaming_data(
##D   universe = c("EUR=", "GBP="),
##D   fields = c("BID", "ASK")
##D )
##D stream$open()
##D Sys.sleep(10)  # Collect some data
##D summary <- stream$get_summary()
##D print(summary)
## End(Not run)



cleanEx()
nameEx("rd_get_top_news")
### * rd_get_top_news

flush(stderr()); flush(stdout())

### Name: rd_get_top_news
### Title: Retrieve Top News Packages from a Refinitiv RDP (JSON)
###   Connection, Then Fetch Stories
### Aliases: rd_get_top_news

### ** Examples

## Not run: 
##D 
##D **Examples for Filtering:**
##D # Example 1: Retrieve all top news from the "Main" group
##D main_news <- rd_get_top_news(group = "^Main$")
##D 
##D # Example 2: Retrieve only the "Front Page" of top news by filtering on page name
##D front_page_news <- rd_get_top_news(page = "^Front Page$")
##D 
##D # Example 3: Retrieve stories from the "Sports & Lifestyle" group where the page is "Sport"
##D sports_news <- rd_get_top_news(group = "Sports & Lifestyle", page = "Sport")
##D 
##D # Example 4: Filtering yields no results (empty data frame)
##D no_news <- rd_get_top_news(group = "NonExistent")
## End(Not run)







cleanEx()
nameEx("rd_handshake")
### * rd_handshake

flush(stderr()); flush(stdout())

### Name: rd_handshake
### Title: Get Bearer Key from Terminal
### Aliases: rd_handshake

### ** Examples

## Not run: 
##D # Fetch a new token regardless of existing tokens
##D response <- rd_handshake(force = TRUE, debug = TRUE)
##D print(response)
## End(Not run)




cleanEx()
nameEx("rd_streaming_pricing")
### * rd_streaming_pricing

flush(stderr()); flush(stdout())

### Name: rd_streaming_pricing
### Title: Pricing Stream Definition
### Aliases: rd_streaming_pricing
### Keywords: datasets

### ** Examples

## Not run: 
##D stream_def <- rd_streaming_pricing$Definition$new(
##D   universe = c("EUR=", "GBP="),
##D   fields = c("BID", "ASK", "DSPLY_NAME")
##D )
##D stream <- stream_def$get_stream()
## End(Not run)



cleanEx()
nameEx("replaceInList")
### * replaceInList

flush(stderr()); flush(stdout())

### Name: replaceInList
### Title: Replace items in nested list
### Aliases: replaceInList
### Keywords: internal

### ** Examples

 x <- list(list(NA, NULL, NULL), list("a", "b", "c"))
# test <- Refinitiv:::replaceInList(x, function(x)if(is.null(x))NA else x)



cleanEx()
nameEx("retry")
### * retry

flush(stderr()); flush(stdout())

### Name: retry
### Title: Retry a function call with exponential backoff
### Aliases: retry
### Keywords: internal

### ** Examples

# Succeeds immediately:
Refinitiv:::retry(function() sum(1, 1))

# Fails and retries, then gives up:
## Not run: 
##D retry(function() sum(1, "a"), max_attempts = 2)
## End(Not run)



cleanEx()
nameEx("translate_to_iso8601_duration")
### * translate_to_iso8601_duration

flush(stderr()); flush(stdout())

### Name: translate_to_iso8601_duration
### Title: Translate Frequency to ISO 8601 Duration
### Aliases: translate_to_iso8601_duration
### Keywords: internal

### ** Examples

Refinitiv:::translate_to_iso8601_duration('minute')   # Returns "PT1M"
Refinitiv:::translate_to_iso8601_duration('hour')     # Returns "PT1H"
Refinitiv:::translate_to_iso8601_duration('weekly')   # Returns "P1W"
Refinitiv:::translate_to_iso8601_duration('unknown')  # Returns "P1D" (default)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
