# R/connection.R
# =====================================================================
# Connection management for RefinitivR.
#
# - rd_connection()  : lazy singleton (zero-config default for all API fns)
# - EikonConnect()   : user-facing connector (legacy Eikon name)
# - RDConnect()      : user-facing connector (RD name)
# - RDPConnect()     : alias for RDConnect()
# =====================================================================

#' @keywords internal
.connection_cache <- new.env(parent = emptyenv())


# ------------------------------------------------------------------
# Internal: shared connection logic
# ------------------------------------------------------------------
#' @keywords internal
.resolve_and_connect <- function(application_id = NA, UUID = NA) {
  # Resolve API key: explicit arg > vault > default workspace key
  if (is.na(application_id)) {
    application_id <- refinitiv_vault_get("api_key")
    if (is.null(application_id)) {
      application_id <- "DEFAULT_WORKSPACE_APP_KEY"
    }
  }

  if (is.na(UUID)) {
    UUID <- getOption(".RefinitivUUID")
  }

  refinitiv_vault_set("api_key", application_id)
  options(.RefinitivAPI = "JSON")
  options(.RefinitivUUID = UUID)

  conn <- RefinitivJsonConnect()
  .connection_cache$conn <- conn
  conn
}


# ==================================================================
# Lazy singleton
# ==================================================================

#' Get or create the default Refinitiv connection
#'
#' Returns a cached connection object, creating it on first use.
#' The connection auto-detects the LSEG terminal and uses the
#' \code{DEFAULT_WORKSPACE_APP_KEY}. Call with \code{reset = TRUE} to force
#' re-creation (e.g. after terminal restart).
#'
#' @details
#' Most users of LSEG Workspace do not need to supply an API key ---
#' the default \code{DEFAULT_WORKSPACE_APP_KEY} is used automatically.
#' If you need a custom key, call
#' \code{EikonConnect(Eikonapplication_id = "your_key")} once; the
#' singleton is updated automatically for all subsequent calls.
#'
#' The connection object is lightweight (an environment of closures with
#' no network I/O on creation). Terminal detection happens on first use
#' of an API function.
#'
#' To skip terminal auto-detection (e.g., in CI or with a remote terminal),
#' set \code{REFINITIV_PORT} and optionally \code{REFINITIV_BASE_URL} in your
#' \code{.Renviron} file.
#'
#' @param reset Logical; if \code{TRUE}, discard cached connection and
#'   create a fresh one. Default \code{FALSE}.
#' @return A \code{RefinitivConnection} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Most users never need to call this directly.
#' # All API functions default to rd_connection():
#' data <- EikonGetData(rics = "AAPL.O", Eikonformulas = "TR.CompanyName")
#'
#' # Force re-creation after terminal restart:
#' rd_connection(reset = TRUE)
#' }
rd_connection <- function(reset = FALSE) {
  if (reset || is.null(.connection_cache$conn)) {
    .connection_cache$conn <- RefinitivJsonConnect()
  }
  .connection_cache$conn
}


# ==================================================================
# User-facing connection constructors
# ==================================================================

#' Connect to LSEG Workspace via JSON
#'
#' Establishes a connection to the LSEG Workspace Data API proxy running on
#' localhost.  When called with no arguments the connection uses the terminal's
#' built-in \code{DEFAULT_WORKSPACE_APP_KEY} --- no user-supplied API key is
#' required as long as LSEG Workspace is running.
#'
#' For most workflows you do not need to call this function at all ---
#' API functions default to \code{\link{rd_connection}()}, which auto-creates
#' a connection on first use.  Call \code{EikonConnect()} explicitly when you
#' need a custom API key or want to verify the connection with
#' \code{TestConnection = TRUE}.
#'
#' @param Eikonapplication_id Optional application key.  When \code{NA}
#'   (the default) the function first checks the credential vault, then falls
#'   back to \code{"DEFAULT_WORKSPACE_APP_KEY"}.
#' @param Eikonapplication_port Deprecated and ignored.
#' @param PythonModule Deprecated. Ignored; JSON is always used.
#' @param TestConnection Logical; if \code{TRUE}, performs a test query after
#'   connecting.  Defaults to \code{FALSE}.
#' @param UUID Optional character for custom instruments.
#'
#' @return A \code{RefinitivConnection} object (also cached as the
#'   default singleton for subsequent API calls).
#' @export
#'
#' @examples
#' \dontrun{
#' # Zero-argument connection (recommended):
#' Eikon <- EikonConnect()
#'
#' # Explicit key:
#' Eikon <- EikonConnect(Eikonapplication_id = "your key")
#' }
EikonConnect <- function(Eikonapplication_id = NA, Eikonapplication_port = 9000L,
                         UUID = NA, PythonModule = "JSON", TestConnection = FALSE) {
  if (!identical(PythonModule, "JSON")) {
    warning("PythonModule parameter is deprecated. RefinitivR now always uses JSON. Ignoring PythonModule = '", PythonModule, "'.")
  }

  if (!is.logical(TestConnection)) {
    stop("TestConnection should be TRUE or FALSE")
  }

  CheckTerminalType()
  conn <- .resolve_and_connect(application_id = Eikonapplication_id, UUID = UUID)

  if (TestConnection) {
    tryCatch(
      conn$get_data(instruments = "SPY", fields = "DSPLY_NAME", raw_output = TRUE),
      error = function(e) {
        stop("Connection test failed. Check if LSEG Workspace is running.")
      }
    )
  }

  conn
}


#' Connect to LSEG / Refinitiv Data via JSON
#'
#' Establishes a connection to the LSEG Workspace Data API proxy.
#' When called with no arguments the terminal's built-in
#' \code{DEFAULT_WORKSPACE_APP_KEY} is used automatically.
#'
#' This is functionally equivalent to \code{\link{EikonConnect}()}.
#' For most workflows you do not need to call either --- API functions
#' default to \code{\link{rd_connection}()}.
#'
#' @param application_id Optional application key.  When \code{NA}
#'   (the default) the function checks the credential vault, then falls back
#'   to \code{"DEFAULT_WORKSPACE_APP_KEY"}.
#' @param PythonModule Deprecated. Ignored; JSON is always used.
#' @param UUID Optional character for custom instruments.
#'
#' @return A \code{RefinitivConnection} object (also cached as the
#'   default singleton for subsequent API calls).
#' @export
#'
#' @examples
#' \dontrun{
#' # Zero-argument connection (recommended):
#' rd <- RDConnect()
#'
#' # Explicit key:
#' rd <- RDConnect(application_id = "your key")
#' }
RDConnect <- function(application_id = NA, PythonModule = "JSON", UUID = NA) {
  if (!identical(PythonModule, "JSON")) {
    warning("PythonModule parameter is deprecated. RefinitivR now always uses JSON. Ignoring PythonModule = '", PythonModule, "'.")
  }

  .resolve_and_connect(application_id = application_id, UUID = UUID)
}


#' RDPConnect alias for backward compatibility
#'
#' @rdname RDConnect
#' @examples
#' \dontrun{
#' rd <- RDPConnect()
#' }
#' @export
RDPConnect <- RDConnect
