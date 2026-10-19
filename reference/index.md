# Package index

## Connection & Authentication

Zero-config connection management, terminal auto-detection for LSEG
Workspace and Refinitiv Eikon, and credential storage.

- [`rd_connection()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_connection.md)
  : Get or create the default Refinitiv connection
- [`RDConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDConnect.md)
  [`RDPConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDConnect.md)
  : Connect to LSEG / Refinitiv Data via JSON
- [`EikonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonConnect.md)
  : Connect to LSEG Workspace via JSON
- [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md)
  : Create a JSON connection object for the LSEG Workspace proxy
- [`rd_handshake()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_handshake.md)
  : Get Bearer Key from Terminal
- [`refinitiv_vault()`](https://greengrassblueocean.github.io/RefinitivR/reference/refinitiv_vault.md)
  : Inspect the credential vault safely
- [`print(`*`<RefinitivConnection>`*`)`](https://greengrassblueocean.github.io/RefinitivR/reference/print.RefinitivConnection.md)
  : Print a RefinitivConnection object
- [`names(`*`<RefinitivConnection>`*`)`](https://greengrassblueocean.github.io/RefinitivR/reference/names.RefinitivConnection.md)
  : List methods on a RefinitivConnection object

## Snapshot & Historical Data

Retrieve current snapshots, fundamental history, and historical pricing
time series with corporate action adjustments.

- [`rd_GetData()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetData.md)
  : Retrieve Snapshot Data from LSEG Workspace / Eikon via the JSON API
- [`rd_GetHistory()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetHistory.md)
  : The get_history function allows you to retrieve pricing history, as
  well as Fundamental and Reference data history through a single
  function call.
- [`rd_GetHistoricalPricing()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetHistoricalPricing.md)
  : GetHistoricalPricing
- [`TR_Field()`](https://greengrassblueocean.github.io/RefinitivR/reference/TR_Field.md)
  : Helper function to build the Eikonformulas parameter for the
  EikonGetData function.

## ESG, Estimates & Ownership

Retrieve Environmental, Social, and Governance (ESG) scores and
measures, I/B/E/S consensus estimates, and institutional/insider
ownership data.

- [`rd_GetESG()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetESG.md)
  : Retrieve ESG Data from LSEG
- [`rd_GetEstimates()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetEstimates.md)
  : Retrieve Estimates Data from LSEG
- [`rd_GetOwnership()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetOwnership.md)
  : Retrieve Ownership Data from LSEG

## Symbology & Search

Multi-tiered symbol conversion (RIC, ISIN, CUSIP, SEDOL, bare tickers,
delisted instruments) and discovery search across LSEG views.

- [`rd_ConvertSymbol()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ConvertSymbol.md)
  : Robust Symbol Conversion for Refinitiv Data
- [`RDPsearch()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPsearch.md)
  : RDP search function is a wrapper for the pyton rdp.search function
- [`RDPget_search_metadata()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPget_search_metadata.md)
  : Get search metadata from RDP
- [`RDPShowAvailableSearchViews()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPShowAvailableSearchViews.md)
  : Show Available Search Views for searchView parameter in
  RDPget_search_metadata
- [`EikonRepairMic()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonRepairMic.md)
  : Repair Market Identifier Codes (MIC) utilizing RDN Exchange
  Identifiers

## News

Search news headlines, retrieve full news stories, and browse top news
packages.

- [`rd_get_news_headlines()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_news_headlines.md)
  : Retrieve News Headlines from a Refinitiv RDP (JSON) Connection
- [`rd_get_news_story()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_news_story.md)
  : Retrieve Full News Story from a Refinitiv RDP (JSON) Connection
- [`rd_get_top_news()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_top_news.md)
  : Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection,
  Then Fetch Stories

## Real-Time Streaming

WebSocket-based live market data streaming with event callbacks, history
buffering, and interactive visualization.

- [`rd_get_streaming_data()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_streaming_data.md)
  : Get streaming data from Refinitiv
- [`rd_streaming_pricing`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_streaming_pricing.md)
  : Pricing Stream Definition
- [`rd_streaming_debug()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_streaming_debug.md)
  : Debug streaming connection

## Custom Instruments

Create, search, update, and delete custom formula and basket
instruments.

- [`rd_ManageCustomInstruments()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ManageCustomInstruments.md)
  : CREATE, GET, UPDATE or DELETE a custom instrument
- [`rd_SearchCustomInstruments()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_SearchCustomInstruments.md)
  : Show all available custom instruments that have been created
- [`CorrectCustomInstrument()`](https://greengrassblueocean.github.io/RefinitivR/reference/CorrectCustomInstrument.md)
  : Function to check if custom symbol is in format
  "S)INSTRUMENTSYMBOL.UUID"
- [`CustomInstrumentBasketBuilder()`](https://greengrassblueocean.github.io/RefinitivR/reference/CustomInstrumentBasketBuilder.md)
  : Build a basket element for a custom instrument
- [`CustomInstrumentHolidayBuilder()`](https://greengrassblueocean.github.io/RefinitivR/reference/CustomInstrumentHolidayBuilder.md)
  : Build a holiday object for a custom instrument
- [`CustomInstrumentUDCBuilder()`](https://greengrassblueocean.github.io/RefinitivR/reference/CustomInstrumentUDCBuilder.md)
  : Build a user defined continuation object for a custom instrument

## Session Cache

Inspect and manage the opt-in session-scoped request cache.

- [`rd_CacheInfo()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_CacheInfo.md)
  : Show RefinitivR Cache Statistics
- [`rd_ClearCache()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md)
  : Clear the RefinitivR Session Cache

## DataStream

Connect to LSEG/Refinitiv DataStream via DatastreamDSWS2R.

- [`DataStreamConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/DataStreamConnect.md)
  : Initialize DataStream Connection
- [`TestDataStreamCredentials()`](https://greengrassblueocean.github.io/RefinitivR/reference/TestDataStreamCredentials.md)
  : Test if the DataStream Credentials are valid

## Legacy Eikon API

Backward-compatible Eikon UDF functions and result wrappers.

- [`EikonGetData()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetData.md)
  : Function to obtain data from Eikon via the JSON API
- [`EikonGetTimeseries()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetTimeseries.md)
  : Function to obtain timeseries from Eikon via the JSON API
- [`EikonGetSymbology()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetSymbology.md)
  : Returns a list of instrument names converted into another instrument
  code. For example: convert SEDOL instrument names to RIC names
- [`EikonGetNewsHeadlines()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetNewsHeadlines.md)
  : Returns a list of news headlines
- [`EikonGetNewsStory()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetNewsStory.md)
  : Return a single news story corresponding to the identifier provided
  in story_id
- [`print(`*`<EikonResult>`*`)`](https://greengrassblueocean.github.io/RefinitivR/reference/print.EikonResult.md)
  : Print an EikonResult
- [`as.data.table.EikonResult()`](https://greengrassblueocean.github.io/RefinitivR/reference/as.data.table.EikonResult.md)
  : Convert EikonResult to data.table
- [`as.data.frame(`*`<EikonResult>`*`)`](https://greengrassblueocean.github.io/RefinitivR/reference/as.data.frame.EikonResult.md)
  : Convert EikonResult to data.frame

## Bundled Datasets

Reference lookup tables included with the package.

- [`NagerHolidayData`](https://greengrassblueocean.github.io/RefinitivR/reference/NagerHolidayData.md)
  : Public Holidays between the years 1990 and 2030
- [`OperatingMicLookup`](https://greengrassblueocean.github.io/RefinitivR/reference/OperatingMicLookup.md)
  : Operating Mic lookup from Reuters Exchange Code
- [`SearchViewsLookup`](https://greengrassblueocean.github.io/RefinitivR/reference/SearchViewsLookup.md)
  : Lookup code for SearchViews to alternate between RD and JSON format
  SearchViews

## Defunct Functions

Functions removed in v0.2.0 following removal of the Python/reticulate
bridge.

- [`install_eikon()`](https://greengrassblueocean.github.io/RefinitivR/reference/install_eikon.md)
  : Install Eikon Python environment (Defunct)
- [`RDPGetOptionAnalytics()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPGetOptionAnalytics.md)
  : Get RDP option analytics
