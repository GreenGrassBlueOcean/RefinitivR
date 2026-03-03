#' Retrieve ESG Data from LSEG
#'
#' Fetches Environmental, Social, and Governance (ESG) data from the LSEG
#' Data Platform. Supports multiple views including scores, measures, basic
#' overview, and the ESG universe listing.
#'
#' @param RDObject A connection object returned by \code{\link{RefinitivJsonConnect}()}.
#'   Defaults to \code{RefinitivJsonConnect()} if not supplied.
#' @param universe Character vector of instrument codes (RICs, PermIDs, etc.).
#' @param view Character string selecting the ESG view. One of:
#'   \describe{
#'     \item{\code{"scores-full"}}{Complete ESG scores with all pillars and
#'       categories (default).}
#'     \item{\code{"scores-standard"}}{Standard ESG scores (subset of full).}
#'     \item{\code{"measures-full"}}{Detailed ESG measures across all pillars.}
#'     \item{\code{"measures-standard"}}{Standard ESG measures (subset of full).}
#'     \item{\code{"basic"}}{Basic ESG overview data.}
#'     \item{\code{"universe"}}{List of instruments covered by the ESG universe.}
#'   }
#' @param start Optional integer. Initial value of the financial year range
#'   (e.g. \code{2020}).
#' @param end Optional integer. End value of the financial year range
#'   (e.g. \code{2023}).
#' @param use_field_names_in_headers Logical. If \code{TRUE} (default), column
#'   names come from the API's \code{title} field; if \code{FALSE}, from the
#'   internal \code{name} field.
#' @param raw_output Logical. If \code{TRUE}, returns the raw parsed JSON list
#'   instead of a \code{data.frame}.
#' @param debug Logical. If \code{TRUE}, prints debug messages.
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (300 s). \code{FALSE} disables caching. A positive
#'   numeric value sets the cache TTL in seconds. See \code{\link{rd_ClearCache}}.
#'
#' @return A \code{data.frame} with ESG data, or the raw JSON list if
#'   \code{raw_output = TRUE}.
#'
#' @examples
#' \dontrun{
#'   # Full ESG scores for Apple and Microsoft
#'   rd_GetESG(universe = c("AAPL.O", "MSFT.O"))
#'
#'   # Standard measures for a specific year range
#'   rd_GetESG(universe = "AAPL.O", view = "measures-standard",
#'             start = 2020, end = 2023)
#'
#'   # Basic overview
#'   rd_GetESG(universe = "IBM.N", view = "basic")
#' }
#'
#' @export
rd_GetESG <- function(RDObject = rd_connection(),
                      universe,
                      view = c("scores-full", "scores-standard",
                               "measures-full", "measures-standard",
                               "basic", "universe"),
                      start = NULL,
                      end   = NULL,
                      use_field_names_in_headers = TRUE,
                      raw_output = FALSE,
                      debug = FALSE,
                      cache = NULL) {

  # ── Validate inputs ──
  view <- match.arg(view)
  if (missing(universe) || is.null(universe) || length(universe) == 0) {
    stop("rd_GetESG: 'universe' must be supplied.", call. = FALSE)
  }
  if (!is.null(start) && !is.numeric(start)) {
    stop("rd_GetESG: 'start' must be a numeric year (e.g. 2020).", call. = FALSE)
  }
  if (!is.null(end) && !is.numeric(end)) {
    stop("rd_GetESG: 'end' must be a numeric year (e.g. 2023).", call. = FALSE)
  }

  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .ck <- cache_key("rd_GetESG", universe, view, start, end, raw_output)
    .hit <- cache_get(.ck)
    if (.hit$found) {
      if (debug) message("[RefinitivR] Cache hit")
      return(.hit$value)
    }
  }

  force(RDObject)

  # ── Fetch data ──
  raw <- retry(function() {
    RDObject$get_esg(
      universe = universe,
      view     = view,
      start    = if (!is.null(start)) as.integer(start) else NULL,
      end      = if (!is.null(end))   as.integer(end)   else NULL,
      debug    = debug
    )
  })

  # ── Process response ──
  if (raw_output) {
    ReturnElement <- raw
  } else {
    ReturnElement <- process_adc_response(
      raw,
      use_field_names_in_headers = use_field_names_in_headers
    )
  }

  # ── Cache store ──
  if (!isFALSE(ttl) && !is.null(ReturnElement) &&
      !inherits(ReturnElement, "try-error")) {
    cache_set(.ck, ReturnElement, ttl)
  }

  ReturnElement
}
