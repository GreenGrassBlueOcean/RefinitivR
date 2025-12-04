# RDP search function is a wrapper for the pyton rdp.search function

RDP search function is a wrapper for the pyton rdp.search function

## Usage

``` r
RDPsearch(
  RDP = RDConnect(),
  query = NULL,
  view = NULL,
  select = NULL,
  top = NULL,
  filter = NULL,
  boost = NULL,
  order_by = NULL,
  group_by = NULL,
  group_count = NULL,
  navigators = NULL,
  features = NULL,
  SpaceConvertor = ".",
  Arglist = list()
)
```

## Arguments

- RDP:

  Refinitiv DataPlatform Connection object

- query:

  optional character

- view:

  optional character see also RDPShowAvailableSearchViews for available
  searchviews

- select:

  optional character string of length 1 e.g/ "ContractType,RIC"

- top:

  optional numeric search result cut off

- filter:

  optional character filter e.g. "startswith(LastName,'H')"

- boost:

  optional meaning not clear from refinitiv documentation

- order_by:

  optional character string e.g. 'DocumentTitle asc'

- group_by:

  optional character string e.g. 'FirstName'

- group_count:

  optional numeric number of items displayed per group

- navigators:

  optional character string e.g.

- features:

  optional character, meaning not clear from refinitiv documentation

- SpaceConvertor:

  optional character, invokes name cleaning so that parameters can be
  easier used in r, defaults to "."

- Arglist:

  optional named list pass the above parameters as a named list
  withouding needing to use to do.call.

## Value

data.frame with search results

## Details

For additional examples see
<https://github.com/Refinitiv-API-Samples/Article.RDPLibrary.Python.Search>

## See also

RDPShowAvailableSearchViews()

## Examples

``` r
if (FALSE) { # \dontrun{
RDConnect('your api key')
test <- RDPsearch(query =  "AAPL.O", select = "ContractType,RIC")

Presidents <- RDPsearch( view = "People", query = 'president'
                       , filter = "startswith(LastName,'H')"
                       , select = 'DocumentTitle'
                       , boost = ''
                       , order_by = 'DocumentTitle asc'
                       , group_by = 'FirstName'
                       , group_count = 2
                       , top = 20
                       , navigators = 'HullType'
                       , features = 'spell' )

reporates <- RDPsearch( view = "IndicatorQuotes"
                      , query = "repo rate", group_by = "CentralBankName"
                      , group_count = 3
                      , select = paste0("CentralBankName,DocumentTitle,"
                                       ,"RIC,ObservationValue")
                      , top = 1000)

EquitiesSearch <-  RDPsearch( view = "EquityQuotes"
                            , filter = paste0("Eps gt 6.0 and "
                                      , "RCSTRBC2012Name eq 'Personal & "
                                      , "Household Products & Services' "
                                      , "and MktCapTotalUsd gt 100000000 "
                                      , "and IsPrimaryRIC eq true")
                            , top =  10000
                            , select = paste0("DocumentTitle , RIC, Eps,"
                                             ," MktCapTotalUsd"))


Vessels <- RDPsearch( view = "VesselPhysicalAssets"
                    , filter = paste0( "RCSAssetTypeLeaf eq 'tanker'"
                               , " and RCSRegionLeaf eq 'Gulf of Mexico'")
                    , top =  10000
                    , navigators = "OriginPort"
                    , select = paste0( "DocumentTitle,RIC,OriginPort"
                                     , " ,DestinationPort,RCSFlagLeaf"
                                     , ",AssetName,AISStatus,"
                                     , "VesselCurrentPortRIC,IMO")
                    )


ListedSearch <- RDPsearch(Arglist = list(query = "president", view = "People"))

SearchQuery = "aapl.o"
ListedSearch <- RDPsearch(query = SearchQuery)
} # }

if (FALSE) { # \dontrun{
  SearchQuery = "aapl.o"
  ListedSearch <- RDPsearch(RDP = RefinitivJsonConnect(), query = SearchQuery)

} # }
```
