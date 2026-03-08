#' Function to obtain data from Eikon/LSEG via the JSON API
#'
#' The function automatically chunks the list of rics into chunks that comply with the api limitations and in the end rebuilds the chunks again into a single data.frame.
#'
#' @param rics a vector containing the instrument RICS
#' @param Eikonformulas a vector containing character string of Eikon Formulas
#' @param Parameters a named key value list for setting parameters, Default: NULL
#' @param raw_output to return the raw list by chunk for debugging purposes, default = FALSE
#' @param time_out set the maximum timeout to the Eikon server, default = 60
#' @param verbose boolean, set to TRUE to print out the API call details with time stamp for debugging.
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is NULL
#' @param RDObject Refinitiv Data connection object, default is RefinitivJsonConnect()
#' @param use_field_names_in_headers boolean return request fieldnames in stead of titles
#' @param SyncFields boolean, synchronize fields over same time axis
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (300 s). \code{FALSE} disables caching. A positive
#'   numeric value sets the cache TTL in seconds. See \code{\link{rd_ClearCache}}.
#'
#' @return a data.frame containing data from Eikon
#' @importFrom utils capture.output
#'
#' @export
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
#' \dontrun{
#' Refinitiv <- RDConnect()
#' ex1 <- rd_GetData(
#'   RDObject = Refinitiv, rics = c("MMM", "III.L"),
#'   Eikonformulas = c(
#'     "TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/",
#'     "TR.CompanyName"
#'   ), verbose = TRUE
#' )
#'
#' ex2 <- rd_GetData(
#'   RDObject = Refinitiv, rics = "AAPL.O",
#'   Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
#' )
#'
#' rics <- c("AAPL.O")
#' fields <- c(
#'   "TR.IssueMarketCap(Scale=6,ShType=FFL)", "TR.FreeFloatPct()/100/*FreefloatWeight*/",
#'   "TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/",
#'   "TR.CLOSEPRICE(Adjusted=0)/*close*/"
#' )
#'
#' parameters <- list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01", "Fill" = "None")
#' test_json <- rd_GetData(
#'   RDObject = Refinitiv,
#'   rics = rics,
#'   Eikonformulas = fields,
#'   Parameters = parameters,
#'   use_field_names_in_headers = TRUE,
#'   SyncFields = FALSE
#' )
#' }
rd_GetData <- function(
  RDObject = rd_connection(), rics, Eikonformulas, Parameters = NULL, raw_output = FALSE,
  time_out = 60, verbose = getOption("refinitiv_progress", TRUE), SpaceConvertor = NULL, use_field_names_in_headers = FALSE, SyncFields = FALSE,
  cache = NULL
) {
  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .cl <- cache_lookup(
      "rd_GetData", rics, Eikonformulas, Parameters,
      raw_output, SpaceConvertor, use_field_names_in_headers, SyncFields
    )
    if (.cl$found) {
      if (verbose) message("[RefinitivR] Cache hit")
      return(.cl$value)
    }
  }

  # Make sure that connection object has api key
  try(RDObject$set_app_key(app_key = refinitiv_vault_get("api_key")), silent = TRUE)

  # Sort RICs for deterministic chunk boundaries when caching is active
  chunk_rics <- if (!isFALSE(ttl) && length(rics) > 1L) {
    clean <- rics[!is.na(rics)]
    sort(clean, method = if (length(clean) > 1000L) "radix" else "shell")
  } else {
    rics
  }

  # Divide RICS in chunks to satisfy api limits
  ChunckedRics <- EikonChunker(RICS = chunk_rics, Eikonfields = Eikonformulas, verbose = verbose)

  # Per-chunk cache key function (shared params pre-hashed once)
  chunk_cache_key_fn <- NULL
  if (!isFALSE(ttl)) {
    shared_parts <- list(
      "rd_GetData_chunk", get_pkg_version(),
      Eikonformulas, Parameters, SpaceConvertor,
      use_field_names_in_headers, SyncFields
    )
    chunk_cache_key_fn <- function(j) {
      rlang::hash(c(shared_parts, list(ChunckedRics[[j]])))
    }
  }

  EikonDataList <- chunked_download(
    n_chunks = length(ChunckedRics),
    fetch_fn = function(j) {
      retry(function() {
        RDObject$get_data(
          instruments = ChunckedRics[[j]],
          fields      = as.list(Eikonformulas),
          parameters  = Parameters,
          SyncFields  = SyncFields,
          debug       = FALSE,
          raw_output  = TRUE
        )
      }, max_attempts = 1L, on_failure = "NA")
    },
    sleep = 0.01,
    verbose = verbose,
    caller_label = "rd_GetData",
    chunk_sizes = lengths(ChunckedRics),
    fail_message = "rd_GetData downloading data failed",
    chunk_cache_key_fn = chunk_cache_key_fn,
    cache_ttl = ttl
  )


  if (!raw_output) {
    ReturnList <- lapply(
      X = EikonDataList,
      FUN = rd_OutputProcesser,
      use_field_names_in_headers = use_field_names_in_headers,
      SpaceConvertor = SpaceConvertor
    )
    ReturnElement <- data.table::rbindlist(ReturnList, use.names = TRUE, fill = TRUE) |> data.table::setDF()
  } else {
    ReturnElement <- EikonDataList
  }

  # ── Cache store (skip errors / NULL) ──
  if (!isFALSE(ttl) && !is.null(ReturnElement) && !inherits(ReturnElement, "try-error")) {
    cache_set(.cl$key, ReturnElement, ttl)
  }

  return(ReturnElement)
}
