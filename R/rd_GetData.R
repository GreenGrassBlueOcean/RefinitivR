#' Retrieve Snapshot Data from LSEG Workspace / Eikon via the JSON API
#'
#' Automatically chunks the requested instruments to comply with API limits and
#' recombines the results into a single \code{data.frame}.
#'
#' @param rics a vector containing the instrument RICs
#' @param Eikonformulas a vector containing character string of Eikon Formulas (deprecated, use \code{fields} instead)
#' @param Parameters a named key value list for setting parameters, Default: NULL
#' @param raw_output to return the raw list by chunk for debugging purposes, default = FALSE
#' @param time_out set the maximum timeout to the LSEG/Eikon server, default = 60
#' @param verbose boolean, set to TRUE to print out the API call details with time stamp for debugging.
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is NULL
#' @param RDObject Refinitiv Data connection object, defaults to \code{\link{rd_connection}()}
#' @param use_field_names_in_headers boolean return request fieldnames in stead of titles
#' @param SyncFields boolean, synchronize fields over same time axis
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (300 s). \code{FALSE} disables caching. A positive
#'   numeric value sets the cache TTL in seconds. See \code{\link{rd_ClearCache}}.
#' @param fields a vector containing character string of requested data fields (preferred over \code{Eikonformulas})
#'
#' @details
#' \strong{LSEG Backend AQL Error Handling and Adaptive Sub-Chunking:}
#' When LSEG's backend Asynchronous Query Library (AQL) is under heavy load and
#' receives a large multi-RIC \eqn{\times} multi-formula request (e.g. \eqn{> 28}
#' RICs \eqn{\times} 100+ \code{TR.*} formulas), the proxy may return HTTP 200
#' with a collapsed 1-column payload (\code{totalColumnsCount = 1},
#' \code{headers[[1]][[1]]$displayName = NULL}) and \code{code = 100}
#' (\code{"Asynchronous Query library internal error."}) in \code{$error} for
#' every row. \code{rd_GetData()} rejects these collapsed AQL payloads via
#' \code{eikon_data_is_success()} so they are never treated as valid data or
#' cached, and when an oversized chunk (\eqn{> 28} RICs) hits an AQL collapse it
#' automatically splits that chunk (and any remaining oversized chunks in the
#' same call) into \eqn{\le 25}-RIC sub-chunks
#' (\code{getOption("refinitiv_aql_fallback_rics", 25L)}). See
#' \code{\link{EikonGetData}} for full details on the three LSEG backend AQL
#' failure modes.
#'
#' @return a data.frame containing data from LSEG Workspace / Eikon
#' @importFrom utils capture.output
#'
#' @export
#' @references \url{https://developers.lseg.com/en/api-catalog/eikon/eikon-data-api/documentation}
#'
#' @examples
#' \dontrun{
#' ex1 <- rd_GetData(
#'   rics = c("MMM", "III.L"),
#'   fields = c(
#'     "TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/",
#'     "TR.CompanyName"
#'   ),
#'   verbose = TRUE
#' )
#'
#' ex2 <- rd_GetData(
#'   rics = "AAPL.O",
#'   fields = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
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
#'   rics = rics,
#'   fields = fields,
#'   Parameters = parameters,
#'   use_field_names_in_headers = TRUE,
#'   SyncFields = FALSE
#' )
#' }
rd_GetData <- function(
  RDObject = rd_connection(), rics, Eikonformulas = NULL, Parameters = NULL, raw_output = FALSE,
  time_out = 60, verbose = getOption("refinitiv_progress", TRUE), SpaceConvertor = NULL, use_field_names_in_headers = FALSE, SyncFields = FALSE,
  cache = NULL, fields = NULL
) {
  if (is.null(Eikonformulas) && is.null(fields)) {
    stop("Please provide either 'Eikonformulas' or 'fields'")
  }
  if (!is.null(fields) && is.null(Eikonformulas)) {
    Eikonformulas <- fields
  }

  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .cl <- cache_lookup(
      "rd_GetData", rics, Eikonformulas, Parameters,
      raw_output, SpaceConvertor, use_field_names_in_headers, SyncFields
    )
    if (.cl$found) {
      if (!isFALSE(verbose)) message("[RefinitivR] Cache hit")
      if (!raw_output) {
        return(.restore_input_order(.cl$value, rics, "Instrument"))
      }
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
  ChunckedRics <- EikonChunker(
    RICS = chunk_rics, Eikonfields = Eikonformulas,
    verbose = verbose, Parameters = Parameters
  )

  # Per-chunk cache key function (shared params pre-hashed once)
  chunk_cache_key_fn <- NULL
  if (!isFALSE(ttl)) {
    canon_formulas <- if (is.character(Eikonformulas) && length(Eikonformulas) > 1L) {
      sort(Eikonformulas)
    } else {
      Eikonformulas
    }
    canon_params <- if (is.list(Parameters)) sort_named_recursive(Parameters) else Parameters
    shared_parts <- list(
      "rd_GetData_chunk", get_pkg_version(),
      canon_formulas, canon_params, SpaceConvertor,
      use_field_names_in_headers, SyncFields
    )
    chunk_cache_key_fn <- function(j) {
      chunk_j <- ChunckedRics[[j]]
      if (is.character(chunk_j) && length(chunk_j) > 1L) {
        chunk_j <- sort(chunk_j)
      }
      rlang::hash(c(shared_parts, list(chunk_j)))
    }
  }

  n_formulas <- length(Eikonformulas)
  is_chunk_ok <- function(x) eikon_data_is_success(x, n_formulas = n_formulas)
  aql_state <- new.env(parent = emptyenv())
  aql_state$aql_degraded <- FALSE

  fetch_rics_once <- function(rics_vec) {
    retry(function() {
      RDObject$get_data(
        instruments = rics_vec,
        fields      = as.list(Eikonformulas),
        parameters  = Parameters,
        SyncFields  = SyncFields,
        debug       = FALSE,
        raw_output  = TRUE
      )
    }, max_attempts = 1L, on_failure = "NA")
  }

  EikonDataList <- chunked_download(
    n_chunks = length(ChunckedRics),
    fetch_fn = function(j) {
      .fetch_with_aql_subchunks(
        chunk_rics    = ChunckedRics[[j]],
        n_formulas    = n_formulas,
        fetch_rics_fn = fetch_rics_once,
        is_success_fn = is_chunk_ok,
        state_env     = aql_state,
        caller_label  = "rd_GetData",
        chunk_idx     = j,
        n_chunks      = length(ChunckedRics),
        verbose       = verbose
      )
    },
    is_success = is_chunk_ok,
    verbose = verbose,
    caller_label = "rd_GetData",
    chunk_sizes = lengths(ChunckedRics),
    fail_message = "rd_GetData downloading data failed",
    chunk_cache_key_fn = chunk_cache_key_fn,
    cache_ttl = ttl
  )

  any_chunk_failed <- !all(vapply(EikonDataList, is_chunk_ok, logical(1L)))

  if (!raw_output) {
    ReturnList <- lapply(
      X = EikonDataList,
      FUN = rd_OutputProcesser,
      use_field_names_in_headers = use_field_names_in_headers,
      SpaceConvertor = SpaceConvertor
    )
    ReturnElement <- data.table::rbindlist(ReturnList, use.names = TRUE, fill = TRUE) |> data.table::setDF()
    if (!isFALSE(ttl)) {
      ReturnElement <- .restore_input_order(ReturnElement, rics, "Instrument")
    }
  } else {
    ReturnElement <- EikonDataList
  }

  # ── Cache store (skip errors / NULL / partial chunk failures) ──
  if (!isFALSE(ttl) && !any_chunk_failed && !is.null(ReturnElement) && !inherits(ReturnElement, "try-error")) {
    cache_set(.cl$key, ReturnElement, ttl)
  }

  return(ReturnElement)
}
