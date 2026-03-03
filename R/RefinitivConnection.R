# R/RefinitivConnection.R
# =====================================================================
# S3 class infrastructure for the connection object.
#
# The connection object is an rlang::env() with closure methods.
# This file adds a class tag ("RefinitivConnection") plus print()
# and names() methods so the object is inspectable without leaking
# secrets.
# =====================================================================

#' Print a RefinitivConnection object
#'
#' Displays connection metadata (base URL, port, credential status)
#' without revealing API keys or tokens.
#'
#' @param x A \code{RefinitivConnection} object.
#' @param ... Ignored.
#' @return \code{x} (invisibly).
#' @export
print.RefinitivConnection <- function(x, ...) {
  port <- getOption("eikon_port")
  base_url <- getOption("refinitiv_base_url", "http://localhost")
  has_key <- refinitiv_vault_has("api_key")
  has_token <- refinitiv_vault_has("access_token")

  cat("<RefinitivConnection>\n")
  cat("  Base URL :", base_url, "\n")
  cat("  Port     :", if (!is.null(port)) port else "(not set)", "\n")
  cat("  API key  :", if (has_key) "(set)" else "(not set)", "\n")
  cat("  Token    :", if (has_token) "(set)" else "(not set)", "\n")
  cat("  Methods  :", length(names(x)), "available\n")
  invisible(x)
}

#' List methods on a RefinitivConnection object
#'
#' Returns only the user-facing API method names, filtering out any
#' environment internals.
#'
#' @param x A \code{RefinitivConnection} object.
#' @return Character vector of method names.
#' @export
names.RefinitivConnection <- function(x) {
  all_names <- ls(envir = x)
  setdiff(all_names, c(".__enclos_env__", "clone", "initialize", ".__active__"))
}
