test_that("Basic RDP search works", {

  print(paste("getOption('.EikonApiKey')", getOption(".EikonApiKey")))

  Eikon <- check_Eikonapi(ExecutionMode = "JSON")

  #Eikon <- check_Eikonapi(ExecutionMode = "RD")

  testjson1 <- RDPsearch(RDP = Eikon, query =  "AAPL.O")
  testjson1 <- testjson1[order(names(testjson1))]

  expect_error(testjson1,NA)

  actual1class <- lapply(testjson1, class)

  expected1 <- list(BusinessEntity = "character", DocumentTitle = "character",
                      PI = "character", PermID = "character", RIC = "character")

  expected1 <- expected1[order(names(expected1))]

  expect_equal( actual1class, expected1)
  expect_equal(class(testjson1), "data.frame")


  if(!is.null(getOption(".EikonApiKey"))){
    TestRDObject <- RDConnect(PythonModule = "RD")
    test_python <- RDPsearch(RDP = TestRDObject, query =  "AAPL.O")

    expect_equal(test_python, testjson1)
  }
})


test_that("RDP search works with select statements", {

  Eikon <- check_Eikonapi(ExecutionMode = "JSON")

  test2json <- try(RDPsearch(RDP = Eikon, query =  "AAPL.O", select = "ContractType,RIC"))
  test2json <- test2json[order(names(test2json))]


  expect_error(test2json,NA)
  test2class <- lapply(test2json, class)

  expected2 <- list(ContractType = "character", RIC = "character")
  expected2 <- expected2[order(names(expected2))]

  expect_equal(test2class, expected2)
  expect_equal(class(test2json), "data.frame")

  if(!is.null(getOption(".EikonApiKey"))){

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python2 <- RDPsearch(RDP = TestRDObject, query =  "AAPL.O", select = "ContractType,RIC")

  expect_equal(test_python2, test2json)
  }

})

test_that("RDP search works with complex statements", {

  Eikon <- check_Eikonapi(ExecutionMode = "JSON")


  test3json <-  RDPsearch(RDP = Eikon, view = "People", query = 'president'
                    , filter = "startswith(LastName,'H')"
                    , select = 'DocumentTitle'
                    , boost = ''
                    , order_by = 'DocumentTitle asc'
                    , group_by = 'FirstName'
                    , group_count = 2
                    , top = 20
                    )
  test3json <- test3json[order(names(test3json))]


  expect_error(test3json,NA)
  test3class <- lapply(test3json, class)

  expect_equal( test3class #[order(names(test3class))]
              , list(DocumentTitle = "character")
  )
  expect_equal(class(test3json), "data.frame")
  if(!is.null(getOption(".EikonApiKey"))){

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python3 <- RDPsearch(RDP = TestRDObject, view = "People", query = 'president'
                            , filter = "startswith(LastName,'H')"
                            , select = 'DocumentTitle'
                            , boost = ''
                            , order_by = 'DocumentTitle asc'
                            , group_by = 'FirstName'
                            , group_count = 2
                            , top = 20
                            )

  expect_equal(test_python3, test3json)
  }

})


test_that("RDP search works with numeric values", {

  Eikon <- check_Eikonapi(ExecutionMode = "JSON")

  test4json <- RDPsearch(RDP = Eikon,  view = "IndicatorQuotes"
                        , query = "repo rate", group_by = "CentralBankName"
                        , group_count = 3
                        , select = paste0("CentralBankName,"
                                         ,"DocumentTitle,RIC,ObservationValue")
                        , top = 3
                        )

  test4json <- test4json[order(names(test4json))]

  expect_error(test4json,NA)
  actual4class <- lapply(test4json, class)

  expected4 <- list( CentralBankName = "character", DocumentTitle = "character"
                   , ObservationValue = "numeric", RIC = "character")

  expected4 <- expected4[order(names(expected4))]

  expect_equal(actual4class, expected4)


  expect_equal(class(test4json), "data.frame")

  if(!is.null(getOption(".EikonApiKey"))){

  TestRDObject <- RDConnect(PythonModule = "RD")
  test_python4 <- RDPsearch(RDP = TestRDObject,  view = "IndicatorQuotes"
                           , query = "repo rate", group_by = "CentralBankName"
                           , group_count = 3
                           , select = paste0("CentralBankName,"
                                             ,"DocumentTitle,RIC,ObservationValue")
                           , top = 3
                           )
  test_python4 <- test_python4[order(names(test_python4))]


  data.table::setorderv(test_python4, "RIC")
  data.table::setorderv(test4json, "RIC")


  expect_equal(lapply(test_python4,class), lapply(test4json,class))
  }

})

test_that("RDP search works with compelex vessel data", {

  Eikon <- check_Eikonapi(ExecutionMode = "JSON")
  test6json <- RDPsearch(RDP = Eikon
                      , view = "VesselPhysicalAssets"
                      , filter = paste0("RCSAssetTypeLeaf eq 'tanker' "
                                       ,"and RCSRegionLeaf eq 'Gulf of Mexico'")
                      , top =  10000
                      , select = paste0("DocumentTitle,RIC,OriginPort,"
                                       , "DestinationPort,RCSFlagLeaf,"
                                       , "AssetName,AISStatus,"
                                       , "VesselCurrentPortRIC,IMO")
                      )


  test6class <- lapply(test6json, class)
  actual6 <- test6class[order(names(test6class))]

  expected6 <- list(AISStatus = "character", AssetName = "character", DestinationPort = "character",
       DocumentTitle = "character", IMO = "character", OriginPort = "character",
       RCSFlagLeaf = "character", RIC = "character", VesselCurrentPortRIC = "character")


  expected6 <- expected6[order(names(expected6))]

  expect_equal(actual6, expected6)

  expect_error(test6json,NA)

  expect_equal(class(test6json), "data.frame")

  if(!is.null(getOption(".EikonApiKey"))){

    TestRDObject <- RDConnect(PythonModule = "RD")
    test_python6 <- RDPsearch(RDP = TestRDObject, view = "VesselPhysicalAssets"
                              , filter = paste0("RCSAssetTypeLeaf eq 'tanker' "
                                                ,"and RCSRegionLeaf eq 'Gulf of Mexico'")
                              , top =  10000
                              , select = paste0("DocumentTitle,RIC,OriginPort,"
                                              , "DestinationPort,RCSFlagLeaf,"
                                              , "AssetName,AISStatus,"
                                              , "VesselCurrentPortRIC,IMO")
    )


    test_python6 <- test_python6[order(names(test_python6))]
    test6json <- test6json[order(names(test6json))]

    expect_equal(test_python6, test6json)

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



test_that("RDP search can handle nested results for both python and JSON",{

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  View1 <- "FundQuotes"
  view1a <- View1
  top1 <- 25L
  filter1 <- "(AssetState ne 'DC' and SearchAllCategoryv2 eq 'Funds' and ((RCSIssuerDomicileCountry xeq 'G:7M' or RCSIssuerDomicileCountry xeq 'G:6X' or RCSIssuerDomicileCountry xeq 'G:8W' or RCSIssuerDomicileCountry xeq 'G:7J' or RCSIssuerDomicileCountry xeq 'G:7K') and RCSCurrency xeq 'C:6' and RCSAssetCategoryGenealogy in ('A:KZ')))"
  select1 <- "DTSubjectName,PI,RIC,BusinessEntity,SearchAllCategoryv3,SearchAllCategoryv2,SearchAllCategory,IssueISIN,IssueLipperGlobalSchemeName,RCSAssetCategoryLeaf,RCSIssuerDomicileCountryLeaf,RCSIssueCountryRegisteredForSale,RCSCurrencyLeaf,ExchangeName,iNAVRIC,RCSIssuerDomicileCountry,RCSCurrency,RCSAssetCategoryGenealogy"


  ETFS_python <- RDPsearch( RDP = RDConnect(PythonModule = "RD")
                          , view = view1a
                          , top = top1
                          , filter = filter1
                          , select = select1
                          ) |> data.table::as.data.table()

  ETFS_JSON <- RDPsearch( RDP = RDConnect(PythonModule = "JSON")
                          , view = view1a
                          , top = top1
                          , filter = filter1
                          , select = select1
  ) |> data.table::as.data.table()

  try(data.table::setcolorder(ETFS_JSON, colnames(ETFS_python)))

  expect_identical(ETFS_python, ETFS_JSON)

})





