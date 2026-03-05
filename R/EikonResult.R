# ──────────────────────────────────────────────────────────────────────────────
# S3 class "EikonResult" — backward-compatible wrapper for EikonGetData output
# ──────────────────────────────────────────────────────────────────────────────
#
# EikonGetData() historically returns a list with two elements:
#   $PostProcessedEikonGetData  — the data (data.frame)
#   $Eikon_Error_Data           — cell-level API errors (data.frame)
#
# These two data.frames can have different row counts (data rows vs. error
# entries), which causes a recycling warning when users call
# as.data.table() on the result.
#
# EikonResult is a thin S3 wrapper that:
#   * Keeps the list structure so $PostProcessedEikonGetData still works
#   * Provides as.data.frame() / as.data.table() that return only the data
#   * Prints a helpful summary instead of dumping two raw data.frames
# ──────────────────────────────────────────────────────────────────────────────

#' Create an EikonResult object
#'
#' @param data_list A list with elements \code{PostProcessedEikonGetData}
#'   and \code{Eikon_Error_Data}, as returned by \code{EikonPostProcessor}.
#' @return An object of class \code{EikonResult} (also inherits from
#'   \code{list}).
#' @keywords internal
#' @noRd
new_EikonResult <- function(data_list) {
  stopifnot(is.list(data_list))
  class(data_list) <- c("EikonResult", "list")
  data_list
}


#' Convert EikonResult to data.frame
#'
#' Returns only the data portion, discarding the error metadata.
#' This matches the documented \code{@return} of \code{EikonGetData}
#' ("a data.frame containing data from Eikon").
#'
#' @param x An \code{EikonResult} object.
#' @param ... Ignored.
#' @return A \code{data.frame}.
#' @export
as.data.frame.EikonResult <- function(x, ...) {
  x[["PostProcessedEikonGetData"]]
}


#' Convert EikonResult to data.table
#'
#' Returns only the data portion as a \code{data.table}, avoiding the
#' recycling warning that occurs when the data and error frames have
#' different row counts.
#'
#' @param x An \code{EikonResult} object.
#' @param ... Passed to \code{data.table::as.data.table()}.
#' @return A \code{data.table}.
#' @export
as.data.table.EikonResult <- function(x, ...) {
  data.table::as.data.table(x[["PostProcessedEikonGetData"]], ...)
}


#' Print an EikonResult
#'
#' @param x An \code{EikonResult} object.
#' @param ... Ignored.
#' @return \code{x} (invisibly).
#' @export
print.EikonResult <- function(x, ...) {
  df <- x[["PostProcessedEikonGetData"]]
  err <- x[["Eikon_Error_Data"]]
  cat(sprintf("<EikonResult: %d rows, %d cols", nrow(df), ncol(df)))
  if (!is.null(err) && nrow(err) > 0L) {
    cat(sprintf(", %d API error(s)", nrow(err)))
  }
  cat(">\n")
  print(df, ...)
  invisible(x)
}
