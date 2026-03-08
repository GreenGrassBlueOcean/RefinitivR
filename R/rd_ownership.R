#' Retrieve Ownership Data from LSEG
#'
#' Fetches ownership and shareholder data from the LSEG Data Platform.
#' Supports consolidated, fund, insider, and investor views with automatic
#' pagination for large result sets.
#'
#' @param RDObject A connection object returned by \code{\link{RefinitivJsonConnect}()}.
#'   Defaults to \code{RefinitivJsonConnect()} if not supplied.
#' @param universe Character vector of instrument codes (RICs, PermIDs, etc.).
#' @param view Character string selecting the ownership view. Default is
#'   \code{"consolidated/breakdown"}. See \strong{Details} for all available views.
#' @param stat_type Optional integer (1–8) specifying the breakdown type.
#'   Required for \code{consolidated/breakdown} and \code{fund/breakdown} views:
#'   \describe{
#'     \item{1}{Investor Type}
#'     \item{2}{Investment Style}
#'     \item{3}{Region}
#'     \item{4}{Rotation}
#'     \item{5}{Country}
#'     \item{6}{Metro Area}
#'     \item{7}{Investor Type (Parent)}
#'     \item{8}{Investor Style (Parent)}
#'   }
#' @param limit Optional integer. Maximum total number of rows to retrieve.
#'   If \code{NULL} (default), fetches all available data via automatic
#'   pagination. The LSEG API returns at most 100 rows per request; this
#'   function handles paging transparently.
#' @param sort_order Optional character. Sort direction: \code{"asc"} or
#'   \code{"desc"}.
#' @param frequency Optional character. Data frequency: \code{"Q"} (quarterly)
#'   or \code{"M"} (monthly).
#' @param start Optional character. Start date (ISO 8601 format).
#' @param end Optional character. End date (ISO 8601 format).
#' @param count Optional integer. Record count (alternative to limit for some
#'   endpoints).
#' @param use_field_names_in_headers Logical. If \code{TRUE} (default), column
#'   names come from the API's \code{title} field; if \code{FALSE}, from the
#'   internal \code{name} field.
#' @param raw_output Logical. If \code{TRUE}, returns the raw parsed JSON list
#'   (or list of pages) instead of a \code{data.frame}.
#' @param debug Logical. If \code{TRUE}, prints debug messages.
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (300 s). \code{FALSE} disables caching. A positive
#'   numeric value sets the cache TTL in seconds. See \code{\link{rd_ClearCache}}.
#'
#' @return A \code{data.frame} with ownership data, or the raw JSON list if
#'   \code{raw_output = TRUE}.
#'
#' @details
#' \strong{Available views:}
#'
#' \emph{Consolidated:}
#' \itemize{
#'   \item \code{"consolidated/breakdown"} — Ownership breakdown (requires
#'     \code{stat_type})
#'   \item \code{"consolidated/concentration"}
#'   \item \code{"consolidated/top-n-concentration"}
#'   \item \code{"consolidated/investors"} — Institutional investor list
#'   \item \code{"consolidated/shareholders-report"}
#'   \item \code{"consolidated/shareholders-history-report"}
#'   \item \code{"consolidated/recent-activity"}
#' }
#'
#' \emph{Fund:}
#' \itemize{
#'   \item \code{"fund/holdings"} — Fund holdings
#'   \item \code{"fund/breakdown"} — Fund breakdown (requires \code{stat_type})
#'   \item \code{"fund/concentration"}
#'   \item \code{"fund/top-n-concentration"}
#'   \item \code{"fund/investors"}
#'   \item \code{"fund/shareholders-report"}
#'   \item \code{"fund/shareholders-history-report"}
#'   \item \code{"fund/recent-activity"}
#' }
#'
#' \emph{Insider:}
#' \itemize{
#'   \item \code{"insider/shareholders-report"}
#'   \item \code{"insider/transaction-report"}
#' }
#'
#' \emph{Investor:}
#' \itemize{
#'   \item \code{"investor/holdings"} — Individual investor holdings
#' }
#'
#' \emph{Organization:}
#' \itemize{
#'   \item \code{"org-info"} — Organization information
#' }
#'
#' @examples
#' \dontrun{
#' # Consolidated investor breakdown by type
#' rd_GetOwnership(
#'   universe = "AAPL.O",
#'   view = "consolidated/breakdown",
#'   stat_type = 1
#' )
#'
#' # Top fund holders (first 50)
#' rd_GetOwnership(
#'   universe = "MSFT.O",
#'   view = "fund/investors",
#'   limit = 50
#' )
#'
#' # Insider transactions
#' rd_GetOwnership(
#'   universe = "TSLA.O",
#'   view = "insider/transaction-report"
#' )
#'
#' # Organization info
#' rd_GetOwnership(universe = "IBM.N", view = "org-info")
#' }
#'
#' @export
rd_GetOwnership <- function(RDObject = rd_connection(),
                            universe,
                            view = c(
                              "consolidated/breakdown",
                              "consolidated/concentration",
                              "consolidated/top-n-concentration",
                              "consolidated/investors",
                              "consolidated/shareholders-report",
                              "consolidated/shareholders-history-report",
                              "consolidated/recent-activity",
                              "fund/holdings",
                              "fund/breakdown",
                              "fund/concentration",
                              "fund/top-n-concentration",
                              "fund/investors",
                              "fund/shareholders-report",
                              "fund/shareholders-history-report",
                              "fund/recent-activity",
                              "insider/shareholders-report",
                              "insider/transaction-report",
                              "investor/holdings",
                              "org-info"
                            ),
                            stat_type = NULL,
                            limit = NULL,
                            sort_order = NULL,
                            frequency = NULL,
                            start = NULL,
                            end = NULL,
                            count = NULL,
                            use_field_names_in_headers = TRUE,
                            raw_output = FALSE,
                            debug = FALSE,
                            cache = NULL) {
  # ── Validate inputs ──
  view <- match.arg(view)
  if (missing(universe) || is.null(universe) || length(universe) == 0) {
    stop("rd_GetOwnership: 'universe' must be supplied.", call. = FALSE)
  }

  is_breakdown <- grepl("/breakdown$", view)
  if (is_breakdown && is.null(stat_type)) {
    stop(
      "rd_GetOwnership: 'stat_type' is required for breakdown views. ",
      "Choose an integer from 1 (Investor Type) to 8 (Investor Style Parent).",
      call. = FALSE
    )
  }
  if (!is.null(stat_type)) {
    stat_type <- as.integer(stat_type)
    if (stat_type < 1L || stat_type > 8L) {
      stop("rd_GetOwnership: 'stat_type' must be between 1 and 8.",
        call. = FALSE
      )
    }
  }
  if (!is.null(sort_order)) {
    sort_order <- match.arg(sort_order, c("asc", "desc"))
  }
  if (!is.null(frequency)) {
    frequency <- match.arg(frequency, c("Q", "M"))
  }

  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .cl <- cache_lookup(
      "rd_GetOwnership", universe, view, stat_type, limit,
      sort_order, frequency, start, end, count, raw_output
    )
    if (.cl$found) {
      if (debug) message("[RefinitivR] Cache hit")
      return(.cl$value)
    }
  }

  force(RDObject)

  # ── Fetch data with auto-pagination ──
  MAX_PAGE <- 100L

  fetch_page <- function(page_limit, page_offset) {
    retry(function() {
      RDObject$get_ownership(
        universe  = universe,
        view      = view,
        statType  = stat_type,
        limit     = page_limit,
        offset    = if (page_offset > 0L) page_offset else NULL,
        sortOrder = sort_order,
        frequency = frequency,
        start     = start,
        end       = end,
        count     = count,
        debug     = debug
      )
    })
  }

  needs_pagination <- !is.null(limit) && limit > MAX_PAGE
  fetch_all <- is.null(limit)

  if (needs_pagination || fetch_all) {
    # ── Paginated retrieval ──
    req_limit <- if (fetch_all) Inf else as.numeric(limit)
    offset_val <- 0L
    pages_raw <- list()
    pages_df <- list()

    page_num <- 0L
    while (offset_val < req_limit) {
      page_num <- page_num + 1L
      chunk_limit <- as.integer(min(MAX_PAGE, req_limit - offset_val))
      page_raw <- fetch_page(chunk_limit, offset_val)

      if (raw_output) {
        pages_raw[[length(pages_raw) + 1L]] <- page_raw
      }

      page_df <- process_adc_response(
        page_raw,
        use_field_names_in_headers = use_field_names_in_headers
      )
      pages_df[[length(pages_df) + 1L]] <- page_df

      progress_msg(
        "rd_GetOwnership: page ", page_num, " fetched (",
        nrow(page_df), " rows, offset ", offset_val, ")"
      )

      # Early exit: data exhausted
      if (nrow(page_df) < chunk_limit || nrow(page_df) == 0L) break

      offset_val <- offset_val + chunk_limit
    }

    if (raw_output) {
      ReturnElement <- pages_raw
    } else {
      ReturnElement <- data.table::rbindlist(
        pages_df,
        use.names = TRUE, fill = TRUE
      ) |> data.table::setDF()
    }
  } else {
    # ── Single-page retrieval (limit <= 100 or limit = NULL with no pagination) ──
    raw <- fetch_page(
      page_limit  = if (!is.null(limit)) as.integer(limit) else NULL,
      page_offset = 0L
    )

    if (raw_output) {
      ReturnElement <- raw
    } else {
      ReturnElement <- process_adc_response(
        raw,
        use_field_names_in_headers = use_field_names_in_headers
      )
    }
  }

  # ── Cache store ──
  if (!isFALSE(ttl) && !is.null(ReturnElement) &&
    !inherits(ReturnElement, "try-error")) {
    cache_set(.cl$key, ReturnElement, ttl)
  }

  ReturnElement
}
