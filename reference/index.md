# Package index

## All functions

- [`CheckTerminalType()`](https://greengrassblueocean.github.io/RefinitivR/reference/CheckTerminalType.md)
  : Check Terminal Type and Connectivity for Eikon or Workspace
- [`CorrectCustomInstrument()`](https://greengrassblueocean.github.io/RefinitivR/reference/CorrectCustomInstrument.md)
  : Function to check if custom symbol is in format
  "S)INSTRUMENTSYMBOL.UUID"
- [`CustomInstrumentBasketBuilder()`](https://greengrassblueocean.github.io/RefinitivR/reference/CustomInstrumentBasketBuilder.md)
  : Build a basket element for a custom instrument
- [`CustomInstrumentHolidayBuilder()`](https://greengrassblueocean.github.io/RefinitivR/reference/CustomInstrumentHolidayBuilder.md)
  : Build a holiday object for a custom instrument
- [`CustomInstrumentUDCBuilder()`](https://greengrassblueocean.github.io/RefinitivR/reference/CustomInstrumentUDCBuilder.md)
  : Build a user defined continuation object for a custom instrument
- [`DataStreamConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/DataStreamConnect.md)
  : Initialize DataStream Connection
- [`EikonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonConnect.md)
  : Initialize Eikon Python api
- [`EikonGetData()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetData.md)
  : Function to obtain data from Eikon. Based on the Eikon python
  function get_data
- [`EikonGetNewsHeadlines()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetNewsHeadlines.md)
  : Returns a list of news headlines
- [`EikonGetNewsStory()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetNewsStory.md)
  : Return a single news story corresponding to the identifier provided
  in story_id
- [`EikonGetSymbology()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetSymbology.md)
  : Returns a list of instrument names converted into another instrument
  code. For example: convert SEDOL instrument names to RIC names
- [`EikonGetTimeseries()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonGetTimeseries.md)
  : Function to obtain timeseries from Eikon. Based on the Eikon python
  function get_timeseries
- [`EikonRepairMic()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonRepairMic.md)
  : Often operating Mics are missing from the Eikon api, this function
  does repair these missing operating Mics based upon an internal list
  of codes.
- [`EikonShowAttributes()`](https://greengrassblueocean.github.io/RefinitivR/reference/EikonShowAttributes.md)
  : Show the attributes of the Python Eikon
- [`NagerHolidayData`](https://greengrassblueocean.github.io/RefinitivR/reference/NagerHolidayData.md)
  : Public Holidays between the years 1990 and 2030
- [`OperatingMicLookup`](https://greengrassblueocean.github.io/RefinitivR/reference/OperatingMicLookup.md)
  : Operating Mic lookup from Reuters Exchange Code
- [`PropertiesActiveRefinitivObject()`](https://greengrassblueocean.github.io/RefinitivR/reference/PropertiesActiveRefinitivObject.md)
  : Function to check through which Refinitiv Connection object requests
  are made
- [`RDConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDConnect.md)
  [`RDPConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDConnect.md)
  : RD connection function to refinitiv Data libraries
- [`RDPGetOptionAnalytics()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPGetOptionAnalytics.md)
  : Get RDP option analytics
- [`RDPShowAvailableSearchViews()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPShowAvailableSearchViews.md)
  : Show Available Search Views for seachview parameter in
  RDPget_search_metadata
- [`RDPget_search_metadata()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPget_search_metadata.md)
  : Get search metadata from RDP
- [`RDPsearch()`](https://greengrassblueocean.github.io/RefinitivR/reference/RDPsearch.md)
  : RDP search function is a wrapper for the pyton rdp.search function
- [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md)
  : Connect to Eikon directly with JSON requests
- [`SearchViewsLookup`](https://greengrassblueocean.github.io/RefinitivR/reference/SearchViewsLookup.md)
  : Lookup code for SearchViews to alternate between RD and JSON format
  SearchViews
- [`TR_Field()`](https://greengrassblueocean.github.io/RefinitivR/reference/TR_Field.md)
  : Helper function to build the Eikonformulas parameter for the
  EikonGetData function.
- [`TestDataStreamCredentials()`](https://greengrassblueocean.github.io/RefinitivR/reference/TestDataStreamCredentials.md)
  : Test if the DataStream Credentials are valid
- [`build_get_query_string()`](https://greengrassblueocean.github.io/RefinitivR/reference/build_get_query_string.md)
  : Build GET Query String
- [`get_rdp_streaming_url()`](https://greengrassblueocean.github.io/RefinitivR/reference/get_rdp_streaming_url.md)
  : Show all available custom instruments that have been created
- [`install_eikon()`](https://greengrassblueocean.github.io/RefinitivR/reference/install_eikon.md)
  : Check if Conda exists, if not instals miniconda, add the python
  eikon module to the python environment r-eikon
- [`mapEikonTimefieldsToRd()`](https://greengrassblueocean.github.io/RefinitivR/reference/mapEikonTimefieldsToRd.md)
  : Map Eikon Time Fields to Refinitiv Data (RD) Fields
- [`rd_GetData()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetData.md)
  : Function to obtain data from Eikon. Based on the Eikon python
  function get_data
- [`rd_GetHistoricalPricing()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetHistoricalPricing.md)
  : GetHistoricalPricing
- [`rd_GetHistory()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_GetHistory.md)
  : The get_history function allows you to retrieve pricing history, as
  well as Fundamental and Reference data history through a single
  function call.
- [`rd_ManageCustomInstruments()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ManageCustomInstruments.md)
  : CREATE, GET, UPDATE or DELETE a custom instrument
- [`rd_SearchCustomInstruments()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_SearchCustomInstruments.md)
  : Show all available custom instruments that have been created
- [`rd_VerifyToken()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_VerifyToken.md)
  : Verify the Validity of a JWT Access Token
- [`rd_check_proxy_url()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_check_proxy_url.md)
  : Check if refinitiv proxy url is alive
- [`rd_get_news_headlines()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_news_headlines.md)
  : Retrieve News Headlines from a Refinitiv RDP (JSON) Connection
- [`rd_get_news_story()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_news_story.md)
  : Retrieve Full News Story from a Refinitiv RDP (JSON) Connection
- [`rd_get_streaming_data()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_streaming_data.md)
  : Get streaming data from Refinitiv
- [`rd_get_top_news()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_get_top_news.md)
  : Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection,
  Then Fetch Stories
- [`rd_handshake()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_handshake.md)
  : Get Bearer Key from Terminal
- [`rd_streaming_debug()`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_streaming_debug.md)
  : Debug streaming connection
- [`rd_streaming_pricing`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_streaming_pricing.md)
  : Pricing Stream Definition
- [`retry()`](https://greengrassblueocean.github.io/RefinitivR/reference/retry.md)
  : Function to retry failed functions after a time out of 5 seconds.
  Especially useful for failed api calls.
- [`translate_to_iso8601_duration()`](https://greengrassblueocean.github.io/RefinitivR/reference/translate_to_iso8601_duration.md)
  : Translate Frequency to ISO 8601 Duration
