#' Check LSEG Workspace Connectivity
#'
#' Verifies that the LSEG Workspace Data API proxy is reachable on port 9000
#' and sets the \code{eikon_port} option accordingly.
#'
#' Auto-detection is skipped when \code{eikon_port} is already set.
#' Pre-configure via \code{options(eikon_port = 9000L)} or the
#' \code{REFINITIV_PORT} environment variable (read at package load).
#'
#' @param verbose Logical; if \code{TRUE}, prints status messages. Defaults to \code{FALSE}.
#' @param force Logical; if \code{TRUE}, rechecks even when \code{eikon_port} is
#'   already set. Defaults to \code{FALSE}.
#'
#' @return Invisibly \code{NULL}. Sets \code{options(eikon_port = 9000L)} on
#'   success; issues a warning if the proxy is not reachable.
#'
#' @examples
#' \dontrun{
#' CheckTerminalType(verbose = TRUE, force = TRUE)
#' }
#'
#' @keywords internal
CheckTerminalType <- function(verbose = FALSE, force = FALSE) {
  if (!force && !is.null(getOption("eikon_port"))) {
    return(invisible(NULL))
  }

  # Probe the configured port first, then fall back to known defaults.
  # This allows env-var or option-based override for remote terminals.
  configured_port <- getOption("eikon_port")
  ports_to_try <- unique(c(
    if (!is.null(configured_port)) configured_port,
    9000L, 9060L
  ))

  for (port in ports_to_try) {
    ok <- tryCatch(
      {
        rd_check_proxy_url(port = port, debug = FALSE)
        TRUE
      },
      error = function(e) {
        if (verbose) message("No connectivity on port ", port)
        FALSE
      }
    )

    if (ok) {
      options(eikon_port = port)
      if (verbose) message("LSEG Workspace detected on port ", port)
      return(invisible(NULL))
    }
  }

  warning("No terminal connection. Please make sure LSEG Workspace is running.")
  invisible(NULL)
}
