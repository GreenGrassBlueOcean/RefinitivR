#' Show Available Search Views for seachview parameter in RDPget_search_metadata
#'
#' @param Platform character vector either RD, RDP or JSON, defaults to JSON
#'
#' @return vector with searchviews that can be used.
#' @export
#'
#'
#' @seealso RDPget_search_metadata
#'
#' @examples
#' RDPShowAvailableSearchViews(Platform = "JSON")
#' RDPShowAvailableSearchViews(Platform = "RD")
RDPShowAvailableSearchViews <- function(Platform = "JSON"){

if(!(Platform %in% c("RD", "RDP", "JSON"))){
  stop(paste("Parameter Platform can only be 'RD', 'RDP' or 'JSON' but not:"
            , Platform))
}

if(Platform == "RD"){
  # RD
  #reticulate::py_list_attributes(RD$content$search$Views)
  SearchViews <- c("BOND_FUT_OPT_QUOTES", "CATALOG_ITEMS", "CDS_INSTRUMENTS",
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

} else if(Platform %in% c("JSON", "RDP")){
  #RDP and JSON
  #dput(reticulate::py_list_attributes(RDP$SearchViews))
  SearchViews <-c("BondFutOptQuotes", "CdsInstruments", "CdsQuotes", "CmoInstruments",
                  "CmoQuotes", "CommodityQuotes", "DealsMergersAndAcquisitions",
                  "DerivativeInstruments", "DerivativeQuotes", "EquityDerivativeInstruments",
                  "EquityDerivativeQuotes", "EquityInstruments", "EquityQuotes",
                  "FixedIncomeInstruments", "FixedIncomeQuotes", "FundQuotes",
                  "GovCorpInstruments", "GovCorpQuotes", "IRDQuotes", "IndexInstruments",
                  "IndexQuotes", "IndicatorQuotes", "Instruments", "LoanInstruments",
                  "LoanQuotes", "MoneyQuotes", "MortQuotes", "MortgageInstruments",
                  "MunicipalInstruments", "MunicipalQuotes", "Organisations", "People",
                  "PhysicalAssets", "Quotes", "QuotesAndSTIRs", "STIRs", "SearchAll",
                  "VesselPhysicalAssets", "YieldCurveContQuotes")
}
  return(SearchViews)
}

#' Output processor for python function RDP$get_search_metadata
#'
#' @param python_json python json string
#' @param RemoveNA boolean remove NA value's defaults to FALSE
#' @param CleanNames boolean clean names of data.table, defaults to FALSE
#'
#' @return r data.frame
#' @keywords internal
#'
#' @seealso [get_search_metadata()]
#' @seealso [rd_GetHistory()]
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
Process_RDP_output <- function(python_json, RemoveNA = FALSE, CleanNames = FALSE){

  if(!is.logical(RemoveNA)){
    stop(paste("Parameter RemoveNA in function Process_RDP_output should be boolean but is", RemoveNA))
  }

  if(!is.logical(CleanNames)){
    stop(paste("Parameter CleanNames in function Process_RDP_output should be boolean but is", CleanNames))
  }

  r_json_dirty <- python_json |> reticulate::py_to_r() |> jsonlite::fromJSON()
  r_json_clean <- lapply(X =  r_json_dirty
                        , FUN =  function(x){replaceInList(x, function(y)if(is.null(y) || identical(y,"")) NA else y)})
  r_list <- lapply( X =  r_json_clean
                  , FUN =  function(x){data.table::transpose(data.table::as.data.table(x))})
  r_dt <- data.table::as.data.table(r_list)

  Date <- value <- NULL
  if("Date" %in% names(r_dt)){
    r_dt[, Date := lubridate::ymd_hms(Date)]
  }

  if(RemoveNA){
    r_dt <- r_dt[ !is.na(value),]
  }

  if(all(c("Date", "Instrument", "variable", "value") %in% names(r_dt))){
    data.table::setcolorder( r_dt
                           , neworder = c("Date", "Instrument", "variable", "value")
                           )
  }

  if(CleanNames){
    data.table::setnames(x = r_dt, new = EikonNameCleaner(names(r_dt)))
  }

  return(data.table::setDF(r_dt))
}



#' GetSearchView Object for RDP search for RD and RDP python libraryt
#'
#' @param ConnectionObject RDConnect() or RefinitivJsonConnect()
#' @param ConnectionMetaData defaults to PropertiesActiveRefinitivObject don't change
#' @param SearchView searcView Parameter
#'
#' @return python searchView object
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' GetSearchView(ConnectionObject = RDConnect(), SearchView = "SEARCH_ALL")
#' }
GetSearchView <- function(ConnectionObject = RDConnect()
                         , ConnectionMetaData = PropertiesActiveRefinitivObject(verbose = F)
                        , SearchView = NULL){

  if(is.null(SearchView)){
    stop("parameter SearchView can not be null in GetSearchView")
  }


  if(identical(ConnectionMetaData$name, "refinitiv.data")){

    if(SearchView %in% RDPShowAvailableSearchViews(Platform = "RD")){
      return(ConnectionObject$content$search$Views[[SearchView]])
    } else{
      stop(paste("SearchView:", SearchView, "not available for RD"))
    }


  } else if(ConnectionMetaData$name %in% c("JSON", "testing object")){
    if(SearchView %in% RDPShowAvailableSearchViews(Platform = "JSON")){
      return(SearchView)
    } else{
      stop(paste("SearchView:", SearchView, "not available for JSON"))
    }

  } else{
    stop(paste("GetSearchView only available for RD or JSON but not for:", ConnectionMetaData$name))
  }

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
                                               , convert = F, delay_load = T
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
#' test_json <- RDPget_search_metadata(RDP =  RefinitivJsonConnect()
#'                               , searchView = "EquityQuotes")
#' test_rd <- RDPget_search_metadata(RDP = RDConnect()
#'                               , searchView = "EQUITY_QUOTES")
#' }
RDPget_search_metadata <- function(RDP = RDConnect(), searchView = NULL){

  # Prepare python request ----


  #force RDP to be active
  force(RDP)


  ConnectionMetaData <- PropertiesActiveRefinitivObject(verbose = F)

  if(identical(ConnectionMetaData$name, "refinitiv.data")){

    if(is.null(searchView)){
      searchView <- "SEARCH_ALL"
    }


    if(!(searchView %in% RDPShowAvailableSearchViews(Platform = "RD"))){
      stop(paste("SearchView", searchView, "not available for RD connection object"))
    }

    Definition <- RDP$content$search$metadata$Definition(view = GetSearchView( ConnectionObject = RDP
                                                                             , SearchView = searchView
                                                                             ))
    metadata_python_df <- Definition$get_data()$data$raw

    metadata_r <- reticulate::py_to_r(metadata_python_df)$Properties

    r_DT <- lapply( X = 1:length(names(metadata_r))
                  , function(x, metadata_r){cbind( data.table::data.table(Refinitiv_index = names(metadata_r[x]))
                                                 , data.table::rbindlist(metadata_r[x])
                                                 )}
                  , metadata_r = metadata_r) |>
            data.table::rbindlist(use.names = T, fill = T)

    Properties <- NULL
    r_df <- r_DT[, Properties := NULL] |> data.table::setDF()
  } else {
    if(is.null(searchView)){
      searchView <- "SearchAll"
    }

    r_df <- RDP$get_search_metadata(searchView = searchView)


  }

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
#' RDConnect('your api key')
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
#'}
#'
#' \dontrun{
#'   SearchQuery = "aapl.o"
#'   ListedSearch <- RDPsearch(RDP = RefinitivJsonConnect(), query = SearchQuery)
#'
#' }
#'
RDPsearch <- function(RDP = RDConnect() #RefinitivJsonConnect() #
                     , query =  NULL, view = NULL
                     , select = NULL, top = NULL, filter = NULL
                     , boost= NULL, order_by = NULL, group_by = NULL
                     ,  group_count = NULL, navigators = NULL, features = NULL
                     , Arglist = list()){

  #Build Argument list
  if(!exists("Arglist") || identical(list(),Arglist)){
    Arglist <- as.list(match.call(expand.dots=FALSE))
    Arglist[[1]] <- NULL
  }

  if("view" %in% names(Arglist) && !is.null(Arglist$view) ){
    Arglist$view <- GetSearchView( ConnectionObject = RDP
                                 , SearchView = Arglist$view)



      # RDP$SearchViews[[Arglist$view]]
  }

 #RD -->  RDP$content$search$Views[[searchView]])
 #RDP --> RDP$SearchViews[[searchView]]
 ConnectionMetaData <- PropertiesActiveRefinitivObject(verbose = F)

 if(identical(ConnectionMetaData$name, "refinitiv.data")){
   RDP <- RDP$discovery
 }



  if("top" %in% names(Arglist) && !is.null(Arglist$top)){
    Arglist$top <- as.numeric(Arglist$top)
  }

  if("group_count" %in% names(Arglist) && !is.null(Arglist$group_count) ){
    Arglist$group_count <- as.numeric(Arglist$group_count)
  }

  #Make sure all arguments are evaluated before passing to the search api
  Arglist <- lapply(X = Arglist, FUN = function(x){eval(x, envir=sys.frame(-3))})

  # remove RDP from arglist if this is in it.
  if("RDP" %in% names(Arglist)){
    Arglist$RDP <- NULL
  }


  #force(RDP)

  ConnectionMetaData <- PropertiesActiveRefinitivObject(verbose = F)

  # if(identical(ConnectionMetaData$name, "refinitiv.data")){
  #  RDP <- RDP$discovery
  # }

  #Execute search ----

  # Searcher <- rdp$search
  SearchResult <- do.call(what = RDP[["search"]], args = Arglist)

  if(data.table::is.data.table(SearchResult)){
    return(data.table::setDF(SearchResult))
  } else {
    python_SearchResult <- SearchResult
  }

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
#' @param RDP Refinitiv DataPlatform Connection object, defaults to  RDConnect()
#' @param verbose boolean, print download progress or not
#'
#' @return data.frame with option data
#' @export
#'
#' @examples
#' \dontrun{
#' OPtionInstruments <- Refinitiv::RDPsearch(query = "aapl.o", view = "RelatedOption")
#' OPtionInstruments <- OPtionInstruments[grep(pattern = "*.U"
#' , x = OPtionInstruments$RIC),]$RIC
#'
#' Analytics <- RDPGetOptionAnalytics(OptionRics = OPtionInstruments)
#' }
RDPGetOptionAnalytics <- function(RDP = RDConnect(), OptionRics = NULL, raw = FALSE, verbose = T){

  if(is.null(OptionRics) || !is.character(OptionRics)){
    stop("OptionRics should be supplied currently is not supplied or is in the wrong format")
  }

  force(RDP)
  ConnectionMetaData <- PropertiesActiveRefinitivObject(verbose = F)
  if(identical(ConnectionMetaData$name, "refinitiv.data")){
    RDP <- RDP$content
  }

  ChunckedRics <- EikonChunker(OptionRics, MaxCallsPerChunk = 500, Eikonfields = c("OptionRics")) #Refinitiv:::
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
    data[["headers"]] <- replaceInList(data[["headers"]], function(x)if(is.null(x) || identical(x,"") )NA else x)  #Refinitiv:::

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

