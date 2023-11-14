## code to prepare `SearchViewsLookup` dataset goes here

SearchViews_RD <- c("BOND_FUT_OPT_QUOTES", "CATALOG_ITEMS", "CDS_INSTRUMENTS",
                    "CDS_QUOTES", "CMO_INSTRUMENTS", "CMO_QUOTES", "COMMODITY_QUOTES",
                    "DEALS_MERGERS_AND_ACQUISITIONS", "DERIVATIVE_INSTRUMENTS", "DERIVATIVE_QUOTES",
                    "ENTITIES", "EQUITY_DERIVATIVE_INSTRUMENTS", "EQUITY_DERIVATIVE_QUOTES",
                    "EQUITY_INSTRUMENTS", "EQUITY_QUOTES", "FIXED_INCOME_INSTRUMENTS",
                    "FIXED_INCOME_QUOTES", "FUND_QUOTES", "GOV_CORP_INSTRUMENTS",
                    "GOV_CORP_QUOTES", "INDEX_INSTRUMENTS", "INDEX_QUOTES", "INDICATOR_QUOTES",
                    "INSTRUMENTS", "INVESTORS", "IRD_QUOTES", "LOAN_INSTRUMENTS",
                    "LOAN_QUOTES", "MONEY_QUOTES", "MORTGAGE_INSTRUMENTS", "MORT_QUOTES",
                    "MUNICIPAL_INSTRUMENTS", "MUNICIPAL_QUOTES", "ORGANISATIONS",
                    "PEOPLE", "PHYSICAL_ASSETS", "QUOTES", "QUOTES_AND_STIRS", "RCS",
                    "SEARCH_ALL", "STIRS", "VESSEL_PHYSICAL_ASSETS", "YIELD_CURVE_CONT_QUOTES")
SearchViews_RD <- sort(SearchViews_RD)

SearchViews_JSON_RDP <-c("BondFutOptQuotes", "CatalogItems","CdsInstruments", "CdsQuotes", "CmoInstruments",
                         "CmoQuotes", "CommodityQuotes", "DealsMergersAndAcquisitions",
                         "DerivativeInstruments", "DerivativeQuotes", "Entities", "EquityDerivativeInstruments",
                         "EquityDerivativeQuotes", "EquityInstruments", "EquityQuotes",
                         "FixedIncomeInstruments", "FixedIncomeQuotes", "FundQuotes",
                         "GovCorpInstruments", "GovCorpQuotes", "IRDQuotes", "IndexInstruments",
                         "IndexQuotes", "IndicatorQuotes", "Instruments","Investors", "LoanInstruments",
                         "LoanQuotes", "MoneyQuotes", "MortQuotes", "MortgageInstruments",
                         "MunicipalInstruments", "MunicipalQuotes", "Organisations", "People",
                         "PhysicalAssets", "Quotes", "QuotesAndSTIRs", "STIRs", "SearchAll", "Rcs",
                         "VesselPhysicalAssets", "YieldCurveContQuotes")
SearchViews_JSON_RDP <- sort(SearchViews_JSON_RDP)

SearchViewsLookup <- data.table::data.table(SearchViews_RD, SearchViews_JSON_RDP )

usethis::use_data(SearchViewsLookup, overwrite = TRUE)
