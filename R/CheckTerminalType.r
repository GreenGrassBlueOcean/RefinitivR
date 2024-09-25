#' Check Terminal Type and Connectivity for Eikon or Workspace
#'
#' This function checks the connectivity to Eikon or Workspace and sets the corresponding port
#' in the global options. It first verifies if there is a terminal connection available and
#' then checks if Eikon or Workspace Desktop is running by testing specific ports.
#'
#' @param verbose Logical; if `TRUE`, the function will print messages about the detection process. Defaults to `FALSE`.
#' @param force Logical; if `TRUE`, the function will recheck and reset the terminal type even if the \code{eikon_port}
#' option is already set. Defaults to `FALSE`.
#'
#' @return This function sets a global option \code{eikon_port} to either 9000 (Workspace) or 9060 (Eikon),
#' depending on the detected connection. If there is no connection, the function will stop with an error message.
#'
#' @details
#' The function checks for the existence of a terminal connection by attempting to connect to the proxy on port 9000.
#' If successful, it then checks for Eikon connectivity by attempting to connect to port 9060. If Eikon is running,
#' the port is set to 9060, and if Workspace is detected, the port is set to 9000.
#'
#' @examples
#' \dontrun{
#' # Check terminal connection and detect terminal type
#' CheckTerminalType(verbose = TRUE, force = TRUE)
#' }
#'
#' @export
CheckTerminalType <- function(verbose = FALSE, force = FALSE) {

  #0. helper functions ----
  check_connectivity <- function(port, verbose) {
    tryCatch({
      rd_check_proxy_url(port = port, debug = FALSE)
      TRUE
    }, error = function(e) {
      if (verbose) message("No connectivity on port ", port)
      FALSE
    })
  }


  #1. main function ----
  if (force || is.null(getOption("eikon_port"))) {
    if (check_connectivity(9060, verbose)) {
      options(eikon_port = 9060L)
      if (verbose) message("Eikon detected, setting port 9060 for Eikon/UDF use.")
    } else if (check_connectivity(9000, verbose)) {
      options(eikon_port = 9000L)
      if (verbose) message("Workspace detected, setting port 9000 for Eikon/UDF use.")
    } else {
      stop("There is no terminal connection. Please make sure Eikon or Workspace Desktop is running.")
    }
  }
}
