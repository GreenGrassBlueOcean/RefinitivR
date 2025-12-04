# Show Available Search Views for seachview parameter in RDPget_search_metadata

Show Available Search Views for seachview parameter in
RDPget_search_metadata

## Usage

``` r
RDPShowAvailableSearchViews(Platform = "JSON")
```

## Arguments

- Platform:

  character vector either RD, RDP or JSON, defaults to JSON

## Value

vector with searchviews that can be used.

## See also

RDPget_search_metadata

## Examples

``` r
RDPShowAvailableSearchViews(Platform = "JSON")
#>  [1] "BondFutOptQuotes"            "CdsInstruments"             
#>  [3] "CdsQuotes"                   "CmoInstruments"             
#>  [5] "CmoQuotes"                   "CommodityQuotes"            
#>  [7] "DealsMergersAndAcquisitions" "DerivativeInstruments"      
#>  [9] "DerivativeQuotes"            "EquityDerivativeInstruments"
#> [11] "EquityDerivativeQuotes"      "EquityInstruments"          
#> [13] "EquityQuotes"                "FixedIncomeInstruments"     
#> [15] "FixedIncomeQuotes"           "FundQuotes"                 
#> [17] "GovCorpInstruments"          "GovCorpQuotes"              
#> [19] "IRDQuotes"                   "IndexInstruments"           
#> [21] "IndexQuotes"                 "IndicatorQuotes"            
#> [23] "Instruments"                 "LoanInstruments"            
#> [25] "LoanQuotes"                  "MoneyQuotes"                
#> [27] "MortQuotes"                  "MortgageInstruments"        
#> [29] "MunicipalInstruments"        "MunicipalQuotes"            
#> [31] "Organisations"               "People"                     
#> [33] "PhysicalAssets"              "Quotes"                     
#> [35] "QuotesAndSTIRs"              "STIRs"                      
#> [37] "SearchAll"                   "VesselPhysicalAssets"       
#> [39] "YieldCurveContQuotes"       
RDPShowAvailableSearchViews(Platform = "RD")
#>  [1] "BOND_FUT_OPT_QUOTES"            "CATALOG_ITEMS"                 
#>  [3] "CDS_INSTRUMENTS"                "CDS_QUOTES"                    
#>  [5] "CMO_INSTRUMENTS"                "CMO_QUOTES"                    
#>  [7] "COMMODITY_QUOTES"               "DEALS_MERGERS_AND_ACQUISITIONS"
#>  [9] "DERIVATIVE_INSTRUMENTS"         "DERIVATIVE_QUOTES"             
#> [11] "ENTITIES"                       "EQUITY_DERIVATIVE_INSTRUMENTS" 
#> [13] "EQUITY_DERIVATIVE_QUOTES"       "EQUITY_INSTRUMENTS"            
#> [15] "EQUITY_QUOTES"                  "FIXED_INCOME_INSTRUMENTS"      
#> [17] "FIXED_INCOME_QUOTES"            "FUND_QUOTES"                   
#> [19] "GOV_CORP_INSTRUMENTS"           "GOV_CORP_QUOTES"               
#> [21] "INDEX_INSTRUMENTS"              "INDEX_QUOTES"                  
#> [23] "INDICATOR_QUOTES"               "INSTRUMENTS"                   
#> [25] "INVESTORS"                      "IRD_QUOTES"                    
#> [27] "LOAN_INSTRUMENTS"               "LOAN_QUOTES"                   
#> [29] "MONEY_QUOTES"                   "MORTGAGE_INSTRUMENTS"          
#> [31] "MORT_QUOTES"                    "MUNICIPAL_INSTRUMENTS"         
#> [33] "MUNICIPAL_QUOTES"               "ORGANISATIONS"                 
#> [35] "PEOPLE"                         "PHYSICAL_ASSETS"               
#> [37] "QUOTES"                         "QUOTES_AND_STIRS"              
#> [39] "RCS"                            "SEARCH_ALL"                    
#> [41] "STIRS"                          "VESSEL_PHYSICAL_ASSETS"        
#> [43] "YIELD_CURVE_CONT_QUOTES"       
```
