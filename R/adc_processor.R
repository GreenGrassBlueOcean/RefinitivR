#' Process an ADC (Advanced Data Content) API Response
#'
#' Internal helper that converts the standard ADC JSON response
#' (with \code{headers} and \code{data} arrays) into a \code{data.frame}.
#' Used by \code{rd_GetESG}, \code{rd_GetEstimates}, and \code{rd_GetOwnership}.
#'
#' @param x A named list parsed from the JSON response body. Expected to
#'   contain \code{headers} (list of column descriptors with \code{name},
#'   \code{title}, and \code{type}) and \code{data} (list of row vectors).
#' @param use_field_names_in_headers Logical. If \code{TRUE} (default), column
#'   names come from each header's \code{title} field; if \code{FALSE}, from
#'   the \code{name} field.
#'
#' @return A \code{data.frame}. Returns an empty \code{data.frame()} when
#'   \code{x$data} is missing or empty.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Check for API-level errors (\code{x$error}, \code{x$fault}).
#'   \item Extract column metadata from \code{x$headers}.
#'   \item Replace \code{NULL} and \code{""} values with \code{NA} in the
#'         data rows.
#'   \item Bind rows into a \code{data.table} via
#'         \code{\link[data.table]{rbindlist}}.
#'   \item Apply type coercion based on header \code{type}:
#'         \code{"Float"}/\code{"Double"} \to \code{as.numeric()},
#'         \code{"Int32"}/\code{"Int64"}/\code{"Integer"} \to
#'         \code{as.integer()}, \code{"Date"} \to \code{as.Date()},
#'         \code{"DateTime"} \to \code{as.POSIXct()}.
#'   \item Set column names from the selected header field.
#' }
#'
#' @noRd
process_adc_response <- function(x, use_field_names_in_headers = TRUE) {
  # -- 0. Guard: non-list input (e.g. NA from failed retry) --
  if (!is.list(x)) {
    stop("API request failed -- no response received.", call. = FALSE)
  }

  # -- 0b. Error detection --
  if (!is.null(x$error)) {
    if (is.list(x$error)) {
      msg <- x$error$message %||% x$error$description %||%
        as.character(x$error)
    } else {
      msg <- as.character(x$error)
    }
    stop(sprintf("LSEG API error: %s", msg), call. = FALSE)
  }
  if (!is.null(x$fault)) {
    msg <- x$fault$faultstring %||% as.character(x$fault)
    stop(sprintf("LSEG API fault: %s", msg), call. = FALSE)
  }

  # -- 1. Validate structure --
  hdrs <- x$headers
  rows <- x$data

  if (is.null(hdrs) || length(hdrs) == 0) {
    return(data.frame())
  }
  if (is.null(rows) || length(rows) == 0) {
    return(data.frame())
  }

  # -- 2. Extract column metadata --
  col_names <- vapply(hdrs, function(h) {
    if (use_field_names_in_headers && !is.null(h$title)) {
      h$title
    } else {
      h$name %||% "Unknown"
    }
  }, character(1L))

  col_types <- vapply(hdrs, function(h) {
    h$type %||% "String"
  }, character(1L))

  # -- 3. NULL / "" -> NA sanitisation --
  safe_rows <- lapply(rows, function(row) {
    lapply(row, function(val) {
      if (is.null(val) || identical(val, "")) NA else val
    })
  })

  # -- 4. Bind into data.table --
  dt <- data.table::rbindlist(safe_rows, use.names = FALSE, fill = FALSE)

  # -- 5. Column naming --
  if (ncol(dt) == length(col_names)) {
    data.table::setnames(dt, new = col_names)
  }

  # -- 6. Type coercion from header metadata --
  for (j in seq_along(col_types)) {
    if (j > ncol(dt)) break
    cname <- names(dt)[j]
    ctype <- tolower(col_types[j])

    tryCatch(
      suppressWarnings({
        if (ctype %in% c("float", "double", "number")) {
          data.table::set(dt,
            j = cname,
            value = as.numeric(dt[[cname]])
          )
        } else if (ctype %in% c("int32", "integer")) {
          data.table::set(dt,
            j = cname,
            value = as.integer(dt[[cname]])
          )
        } else if (ctype == "int64") {
          # as.numeric (double) handles integers up to 2^53 exactly;
          # as.integer is only 32-bit and silently overflows large values
          data.table::set(dt,
            j = cname,
            value = as.numeric(dt[[cname]])
          )
        } else if (ctype == "date") {
          data.table::set(dt,
            j = cname,
            value = as.Date(as.character(dt[[cname]]))
          )
        } else if (ctype == "datetime") {
          data.table::set(dt,
            j = cname,
            value = as.POSIXct(as.character(dt[[cname]]),
              format = "%Y-%m-%dT%H:%M:%S",
              tz = "UTC"
            )
          )
        }
        # "String" / unrecognised -> leave as-is (character)
      }),
      error = function(e) {
        # If coercion fails, leave the column unchanged
        NULL
      }
    )
  }

  data.table::setDF(dt)
}
