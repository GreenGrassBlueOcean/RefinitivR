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
try(reticulate::use_miniconda(condaenv = "r-eikon"), silent = TRUE)
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
#' @param Arglist optional named list pass the above parameters as a named list withouding needing to use to do.call.
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
#'                             , filter = paste0("Eps gt 6.0 and "
#'                                       , "RCSTRBC2012Name eq 'Personal & "
#'                                       , "Household Products & Services' "
#'                                       , "and MktCapTotalUsd gt 100000000 "
#'                                       , "and IsPrimaryRIC eq true")
#'                             , top =  10000
#'                             , select = paste0("DocumentTitle , RIC, Eps,"
#'                                              ," MktCapTotalUsd"))
#'
#'
#' Vessels <- RDPsearch( view = "VesselPhysicalAssets"
#'                     , filter = paste0( "RCSAssetTypeLeaf eq 'tanker'"
#'                                , " and RCSRegionLeaf eq 'Gulf of Mexico'")
#'                     , top =  10000
#'                     , navigators = "OriginPort"
#'                     , select = paste0( "DocumentTitle,RIC,OriginPort"
#'                                      , " ,DestinationPort,RCSFlagLeaf"
#'                                      , ",AssetName,AISStatus,"
#'                                      , "VesselCurrentPortRIC,IMO")
#'                     )
#'
#'
#' ListedSearch <- RDPsearch(Arglist = list(query = "president", view = "People"))
#'
#' SearchQuery = "aapl.o"
#' ListedSearch <- RDPsearch(query = SearchQuery)
#'
#'}
RDPsearch <- function(RDP = RDPConnect(), query =  NULL, view = NULL
                     , select = NULL, top = NULL, filter = NULL
                     , boost= NULL, order_by = NULL, group_by = NULL
                     ,  group_count = NULL, navigators = NULL, features = NULL
                     , Arglist = list()){

  #Build Argument list
  if(!exists("Arglist") || identical(list(),Arglist)){
    Arglist <- as.list(match.call(expand.dots=FALSE))
    Arglist[[1]] <- NULL
  }

  if("view" %in% names(Arglist)){
    Arglist$view <- RDP$SearchViews[[Arglist$view]]
  }

  if("top" %in% names(Arglist)){
    Arglist$top <- as.numeric(Arglist$top)
  }

  if("group_count" %in% names(Arglist)){
    Arglist$group_count <- as.numeric(Arglist$group_count)
  }

  #Make sure all arguments are evaluated before passing to the search api
  Arglist <- lapply(X = Arglist, FUN = function(x){eval(x, envir=sys.frame(-3))})



  #Execute search ----

  # Searcher <- rdp$search
  python_SearchResult <- do.call(what = RDP[["search"]], args = Arglist)
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




#' Get RDP option analytics
#'
#' @param OptionRics character vector with option rics
#' @param raw return raw data from RDP call
#' @param RDP Refinitiv DataPlatform Connection object, defaults to  RDPConnect()
#' @param verbose boolean, print download progress or not
#'
#' @return data.frame with option data
#' @export
#'
#' @examples
#' \dontrun{
#' RDPGetOptionAnalytics(OptionRics = c("AAPLL032112500.U", "AAPLL032113700.U"))
#' RDPGetOptionAnalytics(OptionRics = rics)
#' }
RDPGetOptionAnalytics <- function(RDP = RDPConnect(), OptionRics = NULL, raw = FALSE, verbose = T){
# browser()
  if(is.null(OptionRics) || !is.character(OptionRics)){
    stop("OptionRics should be supplied currently is not supplied or is in the wrong format")
  }
  ChunckedRics <- Refinitiv:::EikonChunker(OptionRics, MaxCallsPerChunk = 500, Eikonfields = c("OptionRics"))
  OptionAnalytics <- as.list(rep(NA, times = length(ChunckedRics)))
  DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
                                     , succes =  rep(FALSE, length(ChunckedRics))
                                     , retries = rep(0L, length(ChunckedRics), stringsAsFactors = FALSE)
  )

  while (!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)  ) {

    ChunckedRicsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

    for (j in ChunckedRicsTryList) {
      OptionAnalytics[[j]] <- tryCatch({RDP$ipa$FinancialContracts$get_option_analytics(universe = ChunckedRics[[j]])}
                                       , error = function(cond){return(NA)}
                                       )
      Sys.sleep(time = 0.00001)

      if (!identical(OptionAnalytics[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE
      } else { if(verbose){message("Python call failed, but managing")}
               next
             }
      if(verbose){
        message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
      }
      if(!identical(names(OptionAnalytics[[j]]$error_message), character(0))){
        warning(try(paste(OptionAnalytics[[j]]$error_code, OptionAnalytics[[j]]$error_message)))
      }
    }

    DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1
  }
  if(any(DownloadCoordinator$retries > 4L)){
    warning("RDPGetOptionAnalytics downloading data failed for one or more Rics")
  }

  r_IPA_output <- lapply(OptionAnalytics, function(x){reticulate::py_to_r(x$data$raw)})
  # browser()
  if(raw){
    return(r_IPA_output)
  } else {
    Output_DT <- data.table::rbindlist(lapply(r_IPA_output, FUN = "ProcessIPAOutput"))
    return(data.table::setDF(Output_DT))
  }
}


#' Process output of RDP IPA calls
#'
#' @param IPAoutput output to a python call RDP$ipa$FinancialContracts
#'
#' @return data.frame
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' ipa_output <- RDPGetOptionAnalytics(
#'  OptionRics = c("AAPLL032112500.U", "AAPLL032113700.U")
#' , raw = TRUE)
#' ProcessIPAOutput(ipa_output)
#' }
ProcessIPAOutput <- function(IPAoutput){

  #0. Internal functions ----
  getheaders <- function(data){

    #replace null headers with NA headers
    data[["headers"]] <- Refinitiv:::replaceInList(data[["headers"]], function(x)if(is.null(x) || identical(x,"") )NA else x)

    unlist(lapply( X = 1:length(data[["headers"]])
                   , FUN = function(x,data){data[["headers"]][[x]][["name"]] }
                   , data = data
    ))

  }

  getData <- function(data, SpaceConvertor) {

    #1. Remove NULL values and replace with NA in nested list

    data[["data"]] <- replaceInList(data[["data"]], function(x)if(is.null(x) || identical(x,""))NA else x)

    #2. put list format in uniform way (don't mix up lists and vectors in one nested list)

    data[["data"]] <- flattenNestedlist(data[["data"]])

    if (length(data[["data"]]) > 1 ) {
      RequestData <- data.table::rbindlist(data[["data"]])
    } else {
      RequestData <- data[["data"]][[1]]
    }

    Requestheaders <- EikonNameCleaner(getheaders(data), SpaceConvertor = SpaceConvertor)
    RequestData <- data.table::setDF(data.table::setnames(RequestData, Requestheaders))

    return(RequestData)
  }


  #1. Main Programme ----
  Data <- getData(IPAoutput, SpaceConvertor = ".")
  return(Data)
}

