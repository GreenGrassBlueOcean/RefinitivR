#' Show Available Search Views for seachview parameter in RDPget_search_metadata
#'
#' @return vector with searchviews that can be used.
#' @export
#'
#'
#' @seealso RDPget_search_metadata
#'
#' @examples
#' RDPShowAvailableSearchViews()
RDPShowAvailableSearchViews <- function(){

SearchViews <- c("YieldCurveContQuotes","VesselPhysicalAssets","STIRs"
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
                ,"CdsInstruments","BondFutOptQuotes")

return(SearchViews)
}


#' Output processor for python function RDP$get_search_metadata
#'
#' @param metadata_python_df outcome of python RDP$get_search_metadata
#'
#' @return r data.frame
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' "internal function don't use directly"
#' path = paste0("C:\\Users\\XXXX\\Documents\\GitHub",
#' "\\RefinitivR\\tests\\testthat\\PY_get_search_metadata_input.py")
#' PY_get_search_metadata_input <- reticulate::r_to_py(
#'  reticulate::py_load_object(file = path))
#'  r_df <- Process_RDP_output(PY_get_search_metadata_input)
#' }
Process_RDP_output <- function(python_json){
  r_json_dirty <- python_json %>% reticulate::py_to_r() %>% jsonlite::fromJSON()
  r_json_clean <- lapply(r_json_dirty, function(x){replaceInList(x, function(y)if(is.null(y) || identical(y,"")) NA else y)})
  r_list <- lapply(r_json_clean, function(x){data.table::transpose(data.table::as.data.table(x))})
  r_df <- data.table::setDF(data.table::as.data.table(r_list))
return(r_df)
}



#' Import The package Custom python utils for handling pre processing python rdp outputs
#'
#' @return python module
#' @keywords  internal
#'
#' @examples
#' utils <- Refinitiv:::ImportCustomPythonutils()
ImportCustomPythonutils <- function(){

refinitiv_utils <- reticulate::import_from_path( module = "refinitiv_utils"
                                               , convert = F, delay_load = F
                                               , path =  dirname(system.file("python/utils/refinitiv_utils.py"
                                                                            , package = "Refinitiv"))
)
}


#' Get search metadata from RDP
#'
#' @param RDP Refinitiv DataPlatform Connection object
#' @param searchView character choose from @seealso RDPShowAvailableSearchViews
#'
#' @return data.table with metadata search results
#' @export
#'
#' @seealso RDPShowAvailableSearchViews
#'
#' @examples
#' \dontrun{
#' test <- RDPget_search_metadata(searchView = "EquityQuotes")
#' }
RDPget_search_metadata <- function(RDP = RDPConnect(), searchView = NULL){

  # Prepare python request ----
  if(is.null(searchView)){
    searchView <- "SearchAll"
  }

  refinitiv_utils <- ImportCustomPythonutils()

  # Perform Python request ----
  metadata_python_df <- RDP$get_search_metadata(view = RDP$SearchViews[[searchView]])
  index <- metadata_python_df$index
  python_index_col <- refinitiv_utils$split_tupple_list(index)
  metadata_python_df$insert(0L, "Refinitiv_index", python_index_col)
  metadata_python_df$reset_index(drop=TRUE, inplace=TRUE)
  python_json <- metadata_python_df$to_json()

  # Save python request for unit testing ----
    #reticulate::py_save_object(object = python_json, filename = "test.py")

  # Post Process python request ----
  r_df <- Process_RDP_output(python_json)

return(r_df)
}



#' RDP search function is a wrapper for the pyton rdp.search function
#'
#' @param RDP Refinitiv DataPlatform Connection object
#' @param query optional character
#' @param view optional character see also RDPShowAvailableSearchViews for available searchviews
#' @param select optional character string of length 1 e.g/ "ContractType,RIC"
#' @param top optional numeric search result cut off
#' @param filter optional character filter e.g. "startswith(LastName,'H')"
#' @param boost optional meaning not clear from refinitiv documentation
#' @param order_by optional character string e.g. 'DocumentTitle asc'
#' @param group_by optional character string e.g. 'FirstName'
#' @param group_count optional numeric number of items displayed per group
#' @param navigators optional character string e.g.
#' @param features optional character, meaning not clear from refinitiv documentation
#'
#' @seealso RDPShowAvailableSearchViews()
#' @return data.frame with search results
#' @export
#'
#' @details For additional examples see  \url{https://github.com/Refinitiv-API-Samples/Article.RDPLibrary.Python.Search}
#'
#' @examples
#' \dontrun{
#' RDPConnect('your api key')
#` test <- RDPsearch(query =  "AAPL.O")
#' test <- RDPsearch(query =  "AAPL.O", select = "ContractType,RIC")
#'
#' Presidents <- RDPsearch( view = "People", query = 'president'
#'                        , filter = "startswith(LastName,'H')"
#'                        , select = 'DocumentTitle'
#'                        , boost = ''
#'                        , order_by = 'DocumentTitle asc'
#'                        , group_by = 'FirstName'
#'                        , group_count = 2
#'                        , top = 20
#'                        , navigators = 'HullType'
#'                        , features = 'spell' )
#'
#' reporates <- RDPsearch( view = "IndicatorQuotes"
#'                       , query = "repo rate", group_by = "CentralBankName"
#'                       , group_count = 3
#'                       , select = paste0("CentralBankName,DocumentTitle,"
#'                                        ,"RIC,ObservationValue")
#'                       , top = 1000)
#'
#' EquitiesSearch <-  RDPsearch( view = "EquityQuotes"
#'                             , filter = paste0("Eps gt 6.0 and ",
#'                                       , "RCSTRBC2012Name eq 'Personal & "
#'                                       , "Household Products & Services' "
#'                                       , "and MktCapTotalUsd gt 100000000 ",
#'                                       , "and IsPrimaryRIC eq true")
#'                             , top =  10000
#'                             , select = paste0("DocumentTitle , RIC, Eps,"
#'                                              ," MktCapTotalUsd"))
#'
#'
#' Vessels <- RDPsearch( view = "VesselPhysicalAssets"
#'                     , filter = paste0( "RCSAssetTypeLeaf eq 'tanker'",
#'                                , " and RCSRegionLeaf eq 'Gulf of Mexico'"
#'                     , top =  10000
#'                     , navigators = "OriginPort"
#'                     , select = paste0( "DocumentTitle,RIC,OriginPort"
#'                                      , " ,DestinationPort,RCSFlagLeaf"
#'                                      , ",AssetName,AISStatus,"
#'                                      , "VesselCurrentPortRIC,IMO")
#'                     )
#'}
RDPsearch <- function(RDP = RDPConnect(), query =  NULL, view = NULL, select = NULL, top = NULL, filter = NULL, boost= NULL, order_by = NULL, group_by = NULL,  group_count = NULL, navigators = NULL, features = NULL){

  # Prepare search ----

  #Build Argument list
  Arglist <- as.list(match.call(expand.dots=FALSE))
  Arglist[[1]] <- NULL

  if("view" %in% names(Arglist)){
    Arglist$view <- RDP$SearchViews[[Arglist$view]]
  }

  #Build Searcher
  Searcher <- RDP$search

  #Execute search ----
  python_SearchResult <- do.call("Searcher", Arglist)

  # check search outcome
  if(identical(class(python_SearchResult),c("python.builtin.NoneType", "python.builtin.object"))){
    warning("RDPsearch did not provide any result, check query")
    return(data.frame())
  }

  python_json <- python_SearchResult$to_json()

  # Process Output ----
  r_df <- Process_RDP_output(python_json = python_json)

  return(r_df)
}





