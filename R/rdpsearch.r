#' Show Available Search Views for searchView parameter in RDPget_search_metadata
#'
#' @return vector with searchviews that can be used.
#' @export
#'
#'
#' @seealso RDPget_search_metadata
#'
#' @examples
#' RDPShowAvailableSearchViews()
RDPShowAvailableSearchViews <- function() {
  c(
    "BondFutOptQuotes", "CdsInstruments", "CdsQuotes", "CmoInstruments",
    "CmoQuotes", "CommodityQuotes", "DealsMergersAndAcquisitions",
    "DerivativeInstruments", "DerivativeQuotes", "EquityDerivativeInstruments",
    "EquityDerivativeQuotes", "EquityInstruments", "EquityQuotes",
    "FixedIncomeInstruments", "FixedIncomeQuotes", "FundQuotes",
    "GovCorpInstruments", "GovCorpQuotes", "IRDQuotes", "IndexInstruments",
    "IndexQuotes", "IndicatorQuotes", "Instruments", "LoanInstruments",
    "LoanQuotes", "MoneyQuotes", "MortQuotes", "MortgageInstruments",
    "MunicipalInstruments", "MunicipalQuotes", "Organisations", "People",
    "PhysicalAssets", "Quotes", "QuotesAndSTIRs", "STIRs", "SearchAll",
    "VesselPhysicalAssets", "YieldCurveContQuotes"
  )
}


#' Resolve and validate a search view name
#'
#' Also handles legacy RD-style view names by looking them up in SearchViewsLookup.
#'
#' @param ConnectionObject RefinitivJsonConnect() (unused, kept for backward compat)
#' @param SearchView searchView parameter
#'
#' @importFrom utils data
#' @return character search view name
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' GetSearchView(SearchView = "EquityQuotes")
#' }
GetSearchView <- function(
  ConnectionObject = rd_connection(),
  SearchView = NULL
) {
  if (is.null(SearchView)) {
    stop("parameter SearchView can not be null in GetSearchView")
  }

  if (SearchView %in% RDPShowAvailableSearchViews()) {
    return(SearchView)
  } else {
    # Try to translate from legacy RD-style view name
    SearchViewsLookup <- Refinitiv::SearchViewsLookup
    if (SearchView %in% SearchViewsLookup$SearchViews_RD) {
      SearchViews_RD <- NULL
      return(SearchViewsLookup[SearchViews_RD == SearchView]$SearchViews_JSON_RDP)
    } else {
      stop(paste("SearchView:", SearchView, "not available"))
    }
  }
}


#' Get search metadata from RDP
#'
#' @param RDP Refinitiv JSON connection object
#' @param searchView character choose from @seealso RDPShowAvailableSearchViews
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (3600 s / 1 hour). \code{FALSE} disables caching.
#'   A positive numeric value sets the cache TTL in seconds.
#'   See \code{\link{rd_ClearCache}}.
#'
#' @return data.table with metadata search results
#' @export
#'
#' @seealso RDPShowAvailableSearchViews
#'
#' @examples
#' \dontrun{
#' test_json <- RDPget_search_metadata(
#'   RDP = RefinitivJsonConnect(),
#'   searchView = "EquityQuotes"
#' )
#' }
RDPget_search_metadata <- function(RDP = rd_connection(), searchView = NULL, cache = NULL) {
  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 3600)
  if (!isFALSE(ttl)) {
    .ck <- cache_key("RDPget_search_metadata", searchView)
    .hit <- cache_get(.ck)
    if (.hit$found) {
      return(.hit$value)
    }
  }

  force(RDP)

  if (is.null(searchView)) {
    searchView <- "SearchAll"
  }

  r_df <- RDP$get_search_metadata(searchView = searchView)

  # ── Cache store (skip errors / NULL) ──
  if (!isFALSE(ttl) && !is.null(r_df) && !inherits(r_df, "try-error")) {
    cache_set(.ck, r_df, ttl)
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
#' @param SpaceConvertor optional character, invokes name cleaning so that parameters can be easier used in r, defaults to "."
#' @param Arglist optional named list pass the above parameters as a named list withouding needing to use to do.call.
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (300 s). \code{FALSE} disables caching. A positive
#'   numeric value sets the cache TTL in seconds. See \code{\link{rd_ClearCache}}.
#'
#' @seealso RDPShowAvailableSearchViews()
#' @return data.frame with search results
#' @export
#'
#' @details For additional examples see  \url{https://developers.lseg.com/en/api-catalog/refinitiv-data-platform/refinitiv-data-platform-apis/documentation}
#'
#' @examples
#' \dontrun{
#' RDConnect("your api key")
# ` test <- RDPsearch(query =  "AAPL.O")
#' test <- RDPsearch(query = "AAPL.O", select = "ContractType,RIC")
#'
#' Presidents <- RDPsearch(
#'   view = "People", query = "president",
#'   filter = "startswith(LastName,'H')",
#'   select = "DocumentTitle",
#'   boost = "",
#'   order_by = "DocumentTitle asc",
#'   group_by = "FirstName",
#'   group_count = 2,
#'   top = 20,
#'   navigators = "HullType",
#'   features = "spell"
#' )
#'
#' reporates <- RDPsearch(
#'   view = "IndicatorQuotes",
#'   query = "repo rate", group_by = "CentralBankName",
#'   group_count = 3,
#'   select = paste0(
#'     "CentralBankName,DocumentTitle,",
#'     "RIC,ObservationValue"
#'   ),
#'   top = 1000
#' )
#'
#' EquitiesSearch <- RDPsearch(
#'   view = "EquityQuotes",
#'   filter = paste0(
#'     "Eps gt 6.0 and ",
#'     "RCSTRBC2012Name eq 'Personal & ",
#'     "Household Products & Services' ",
#'     "and MktCapTotalUsd gt 100000000 ",
#'     "and IsPrimaryRIC eq true"
#'   ),
#'   top = 10000,
#'   select = paste0(
#'     "DocumentTitle , RIC, Eps,",
#'     " MktCapTotalUsd"
#'   )
#' )
#'
#'
#' Vessels <- RDPsearch(
#'   view = "VesselPhysicalAssets",
#'   filter = paste0(
#'     "RCSAssetTypeLeaf eq 'tanker'",
#'     " and RCSRegionLeaf eq 'Gulf of Mexico'"
#'   ),
#'   top = 10000,
#'   navigators = "OriginPort",
#'   select = paste0(
#'     "DocumentTitle,RIC,OriginPort",
#'     " ,DestinationPort,RCSFlagLeaf",
#'     ",AssetName,AISStatus,",
#'     "VesselCurrentPortRIC,IMO"
#'   )
#' )
#'
#'
#' ListedSearch <- RDPsearch(Arglist = list(query = "president", view = "People"))
#'
#' SearchQuery <- "aapl.o"
#' ListedSearch <- RDPsearch(query = SearchQuery)
#' }
#'
#' \dontrun{
#' SearchQuery <- "aapl.o"
#' ListedSearch <- RDPsearch(RDP = RefinitivJsonConnect(), query = SearchQuery)
#' }
RDPsearch <- function(
  RDP = rd_connection(),
  query = NULL, view = NULL,
  select = NULL, top = NULL, filter = NULL,
  boost = NULL, order_by = NULL, group_by = NULL,
  group_count = NULL, navigators = NULL, features = NULL, SpaceConvertor = ".",
  Arglist = list(),
  cache = NULL
) {
  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .ck <- cache_key(
      "RDPsearch", query, view, select, top, filter, boost,
      order_by, group_by, group_count, navigators, features,
      SpaceConvertor
    )
    .hit <- cache_get(.ck)
    if (.hit$found) {
      return(.hit$value)
    }
  }

  force(RDP)
  # Build Argument list
  if (!exists("Arglist") || identical(list(), Arglist)) {
    Arglist <- as.list(match.call(expand.dots = FALSE))
    Arglist[[1]] <- NULL
    Arglist <- lapply(Arglist, eval, envir = parent.frame(1))
  }

  if ("view" %in% names(Arglist) && !is.null(Arglist$view)) {
    Arglist$view <- eval(Arglist$view, envir = sys.frame(-1))
    Arglist$view <- GetSearchView(
      ConnectionObject = RDP,
      SearchView = Arglist$view
    )
  }

  # remove RDP from arglist if this is in it.
  if ("RDP" %in% names(Arglist)) {
    Arglist$RDP <- NULL
  }

  if ("top" %in% names(Arglist) && !is.null(Arglist$top)) {
    Arglist$top <- as.integer(Arglist$top)
  }

  if ("group_count" %in% names(Arglist) && !is.null(Arglist$group_count)) {
    Arglist$group_count <- as.integer(Arglist$group_count)
  }

  if ("SpaceConvertor" %in% names(Arglist)) {
    Arglist$SpaceConvertor <- NULL
  }

  if ("cache" %in% names(Arglist)) {
    Arglist$cache <- NULL
  }

  # Execute search ----
  SearchResult <- do.call(what = RDP[["search"]], args = Arglist)

  if (data.table::is.data.table(SearchResult)) {
    if (!is.null(SpaceConvertor)) {
      data.table::setnames(SearchResult,
        new = EikonNameCleaner(
          names = names(SearchResult),
          SpaceConvertor = SpaceConvertor
        )
      )
    }
    ReturnElement <- data.table::setDF(SearchResult)
  } else {
    warning("RDPsearch did not provide any result, check query")
    ReturnElement <- data.frame()
  }

  # ── Cache store (skip errors / NULL) ──
  if (!isFALSE(ttl) && !is.null(ReturnElement) && !inherits(ReturnElement, "try-error")) {
    cache_set(.ck, ReturnElement, ttl)
  }

  return(ReturnElement)
}


#' Get RDP option analytics
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function required a Python connection and has been removed.
#' Use the official LSEG Data Library for Python (lseg-data) directly for option analytics.
#'
#' @param ... All arguments are ignored.
#'
#' @return This function always raises an error.
#' @export
RDPGetOptionAnalytics <- function(...) {
  .Defunct(msg = "RDPGetOptionAnalytics() has been removed. It required a Python/reticulate connection which is no longer supported. Use the lseg-data Python library directly for option analytics.")
}


