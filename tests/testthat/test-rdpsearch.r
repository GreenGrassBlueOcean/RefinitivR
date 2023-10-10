test_that("Basic RDP search works", {

  Eikon <- check_Eikonapi()


  test1 <- try(RDPsearch(RDP = Eikon, query =  "AAPL.O"))
  test1 <- test1[order(names(test1))]

  expect_error(test1,NA)

  test1class <- lapply(test1, class)

  expect_setequal( test1class #[order(names(test1class))]
                 , list(BusinessEntity = "character", DocumentTitle = "character",
                     PI = "character", PermID = "character", RIC = "character"))
  expect_equal(class(test1), "data.frame")



  test2 <- try(RDPsearch(RDP = Eikon, query =  "AAPL.O", select = "ContractType,RIC"))
  test2 <- test2[order(names(test2))]


  expect_error(test2,NA)
  test2class <- lapply(test2, class)

  expect_setequal(test2class   #[order(names(test2class))]
                , list(ContractType = "character", RIC = "character")
  )
  expect_equal(class(test2), "data.frame")


  test3 <-  RDPsearch(RDP = Eikon, view = "People", query = 'president'
                    , filter = "startswith(LastName,'H')"
                    , select = 'DocumentTitle'
                    , boost = ''
                    , order_by = 'DocumentTitle asc'
                    , group_by = 'FirstName'
                    , group_count = 2
                    , top = 20
                    , navigators = 'HullType'
                    , features = 'spell'
                    )
  test3 <- test3[order(names(test3))]


  expect_error(test3,NA)
  test3class <- lapply(test3, class)

  expect_setequal( test3class #[order(names(test3class))]
              , list(DocumentTitle = "character")
  )
  expect_equal(class(test3), "data.frame")


  test4 <- RDPsearch(RDP = Eikon,  view = "IndicatorQuotes"
                        , query = "repo rate", group_by = "CentralBankName"
                        , group_count = 3
                        , select = paste0("CentralBankName,"
                                         ,"DocumentTitle,RIC,ObservationValue")
                        , top = 1000
                        )

  test4 <- test4[order(names(test4))]

  expect_error(test4,NA)
  test4class <- lapply(test4, class)

    expect_setequal(test4class
               , list(CentralBankName = "character", DocumentTitle = "character",
                      ObservationValue = "numeric", RIC = "character")
  )
  expect_equal(class(test4), "data.frame")



  test5 <-  RDPsearch(RDP = Eikon,  view = "EquityQuotes"
                     , filter = paste0("Eps gt 6.0 and RCSTRBC2012Name "
                                ,"eq 'Personal & Household Products & Services'"
                                ," and MktCapTotalUsd gt 100000000 "
                                ,"and IsPrimaryRIC eq true")
                     , top =  10000
                     , select = "DocumentTitle , RIC, Eps, MktCapTotalUsd"
                     )

  test5 <- test5[order(names(test5))]

  expect_error(test5,NA)

  test5class <- lapply(test5, class)
  expect_setequal(test5class #[order(names(test5class))]
               , list(DocumentTitle = "character", Eps = "numeric", MktCapTotalUsd = "numeric",
                      RIC = "character")
  )
  expect_equal(class(test5), "data.frame")




  test6 <- RDPsearch(RDP = Eikon
                      , view = "VesselPhysicalAssets"
                      , filter = paste0("RCSAssetTypeLeaf eq 'tanker' "
                                       ,"and RCSRegionLeaf eq 'Gulf of Mexico'")
                      , top =  10000
                      , navigators = "OriginPort"
                      , select = paste0("DocumentTitle,RIC,OriginPort,"
                                       , "DestinationPort,RCSFlagLeaf,"
                                       , "AssetName,AISStatus,"
                                       , "VesselCurrentPortRIC,IMO")
                      )

  test6 <- test6[order(names(test6))]


  expect_error(test6,NA)
  test6class <- lapply(test6, class)
  expect_setequal(test6class #[order(names(test6class))]
              , list(AISStatus = "character", AssetName = "character", DestinationPort = "character",
                    DocumentTitle = "character", IMO = "character", OriginPort = "character",
                     RCSFlagLeaf = "character", RIC = "character", VesselCurrentPortRIC = "character"))
  expect_equal(class(test6), "data.frame")

})


test_that("RDPget_search_metadata works", {

  for(i in c("JSON")){ #, "RD")){
  Eikon <- check_Eikonapi(ExecutionMode = i)

  if(i == "JSON"){
    searchView <- "EquityQuotes"
  } else {
    searchView <- "EQUITY_QUOTES"
  }

  test <- RDPget_search_metadata(RDP = Eikon, searchView = searchView)

  expect_error(test,NA)
  expect_equal(class(test), "data.frame")

  expect_equal(lapply(test, class)
              , list( Refinitiv_index = "character", Type = "character"
                    , Searchable = "logical", Sortable = "logical"
                    , Navigable = "logical", Groupable = "logical"
                    , Exact = "logical", Symbol = "logical"))


  test2 <- RDPget_search_metadata(RDP = Eikon)

  expect_error(test2,NA)
  expect_equal(class(test2), "data.frame")
  expect_equal(lapply(test2, class)
               , list( Refinitiv_index = "character", Type = "character"
                       , Searchable = "logical", Sortable = "logical"
                       , Navigable = "logical", Groupable = "logical"
                       , Exact = "logical", Symbol = "logical"))

  }


})


test_that("RDPShowAvailableSearchViews works",{

  expect_equal( sort(RDPShowAvailableSearchViews(Platform ="JSON" ))
              , sort(c("YieldCurveContQuotes","VesselPhysicalAssets","STIRs"
                           ,"SearchAll","QuotesAndSTIRs","Quotes","PhysicalAssets"
                           ,"People","Organisations","MunicipalQuotes"
                           ,"MunicipalInstruments","MortQuotes","MortgageInstruments"
                           ,"MoneyQuotes","LoanQuotes","LoanInstruments","IRDQuotes"
                           ,"Instruments","IndicatorQuotes","IndexQuotes"
                           ,"IndexInstruments","GovCorpQuotes","GovCorpInstruments"
                           ,"FundQuotes","FixedIncomeQuotes","FixedIncomeInstruments"
                           ,"EquityQuotes","EquityInstruments","EquityDerivativeQuotes"
                           ,"EquityDerivativeInstruments","DerivativeQuotes"
                           ,"DerivativeInstruments","DealsMergersAndAcquisitions"
                           ,"CommodityQuotes","CmoQuotes","CmoInstruments","CdsQuotes"
                           ,"CdsInstruments","BondFutOptQuotes")))


  expect_equal( sort(RDPShowAvailableSearchViews(Platform ="RD" ))
                , sort(c("BOND_FUT_OPT_QUOTES", "CATALOG_ITEMS", "CDS_INSTRUMENTS",
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
                         "SEARCH_ALL", "STIRS", "VESSEL_PHYSICAL_ASSETS", "YIELD_CURVE_CONT_QUOTES")))



})


test_that("RDPShowAvailableSearchViews fails when it should",{

  expect_error(RDPShowAvailableSearchViews(Platform = "NULL")
              , "Parameter Platform can only be 'RD', 'RDP' or 'JSON' but not: NULL"
              , fixed = TRUE)

})

test_that("ImportCustomPythonutils works",{

  utils <- ImportCustomPythonutils()
  expect_equal(class(utils), c("python.builtin.module", "python.builtin.object"))
  expect_equal(names(utils), "split_tupple_list")

})


