#' Retrieve Estimates Data from LSEG
#'
#' Fetches consensus estimates (I/B/E/S) data from the LSEG Data Platform.
#' Supports actuals, summary forecasts, KPI, and historical-snapshot views
#' with configurable content packages.
#'
#' @param RDObject A connection object returned by \code{\link{RefinitivJsonConnect}()}.
#'   Defaults to \code{RefinitivJsonConnect()} if not supplied.
#' @param universe Character vector of instrument codes (RICs, PermIDs, etc.).
#' @param view Character string selecting the estimates view. Default is
#'   \code{"view-summary/annual"}. See \strong{Details} for all available views.
#' @param package Character string. Content tier controlling the breadth and
#'   depth of returned data. One of \code{"basic"}, \code{"standard"}, or
#'   \code{"professional"}. \strong{Required} for all non-KPI views; ignored
#'   for KPI views.
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
#' @return A \code{data.frame} with estimates data, or the raw JSON list if
#'   \code{raw_output = TRUE}.
#'
#' @details
#' \strong{Available views:}
#'
#' \emph{Summary (require \code{package}):}
#' \itemize{
#'   \item \code{"view-summary/annual"} — Annual consensus estimates
#'   \item \code{"view-summary/interim"} — Interim period estimates
#'   \item \code{"view-summary/recommendations"} — Analyst recommendations
#'   \item \code{"view-summary/non-periodic-measures"} — Non-periodic measures
#'   \item \code{"view-summary/historical-snapshots-periodic-measures-annual"}
#'   \item \code{"view-summary/historical-snapshots-periodic-measures-interim"}
#'   \item \code{"view-summary/historical-snapshots-non-periodic-measures"}
#'   \item \code{"view-summary/historical-snapshots-recommendations"}
#' }
#'
#' \emph{Actuals (require \code{package}):}
#' \itemize{
#'   \item \code{"view-actuals/annual"} — Reported annual actuals
#'   \item \code{"view-actuals/interim"} — Reported interim actuals
#' }
#'
#' \emph{KPI (\code{package} not required):}
#' \itemize{
#'   \item \code{"view-actuals-kpi/annual"}
#'   \item \code{"view-actuals-kpi/interim"}
#'   \item \code{"view-summary-kpi/annual"}
#'   \item \code{"view-summary-kpi/interim"}
#'   \item \code{"view-summary-kpi/historical-snapshots-kpi"}
#' }
#'
#' \strong{Package tiers:}
#' \describe{
#'   \item{\code{"basic"}}{Limited fields, single historical point (free tier).}
#'   \item{\code{"standard"}}{Common fields, limited history.}
#'   \item{\code{"professional"}}{All fields, complete history (requires
#'     entitlement).}
#' }
#'
#' @examples
#' \dontrun{
#'   # Annual summary estimates for IBM (basic package)
#'   rd_GetEstimates(universe = "IBM.N", package = "basic")
#'
#'   # Analyst recommendations for multiple companies
#'   rd_GetEstimates(universe = c("AAPL.O", "MSFT.O"),
#'                   view = "view-summary/recommendations",
#'                   package = "standard")
#'
#'   # KPI actuals (no package needed)
#'   rd_GetEstimates(universe = "TSLA.O",
#'                   view = "view-actuals-kpi/annual")
#' }
#'
#' @export
rd_GetEstimates <- function(RDObject = rd_connection(),
                            universe,
                            view = c("view-summary/annual",
                                     "view-summary/interim",
                                     "view-summary/recommendations",
                                     "view-summary/non-periodic-measures",
                                     "view-summary/historical-snapshots-periodic-measures-annual",
                                     "view-summary/historical-snapshots-periodic-measures-interim",
                                     "view-summary/historical-snapshots-non-periodic-measures",
                                     "view-summary/historical-snapshots-recommendations",
                                     "view-actuals/annual",
                                     "view-actuals/interim",
                                     "view-actuals-kpi/annual",
                                     "view-actuals-kpi/interim",
                                     "view-summary-kpi/annual",
                                     "view-summary-kpi/interim",
                                     "view-summary-kpi/historical-snapshots-kpi"),
                            package = NULL,
                            use_field_names_in_headers = TRUE,
                            raw_output = FALSE,
                            debug = FALSE,
                            cache = NULL) {

  # ── Validate inputs ──
  view <- match.arg(view)
  if (missing(universe) || is.null(universe) || length(universe) == 0) {
    stop("rd_GetEstimates: 'universe' must be supplied.", call. = FALSE)
  }

  is_kpi_view <- grepl("kpi", view, fixed = TRUE)

  if (!is_kpi_view && is.null(package)) {
    stop(
      "rd_GetEstimates: 'package' is required for non-KPI views. ",
      "Choose one of: \"basic\", \"standard\", \"professional\".",
      call. = FALSE
    )
  }
  if (!is.null(package)) {
    package <- match.arg(package, c("basic", "standard", "professional"))
  }

  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .ck <- cache_key("rd_GetEstimates", universe, view, package, raw_output)
    .hit <- cache_get(.ck)
    if (.hit$found) {
      if (debug) message("[RefinitivR] Cache hit")
      return(.hit$value)
    }
  }

  force(RDObject)

  # ── Fetch data ──
  # For KPI views, package is NULL (excluded from query string automatically)
  raw <- retry(function() {
    RDObject$get_estimates(
      universe = universe,
      view     = view,
      package  = if (is_kpi_view) NULL else package,
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
