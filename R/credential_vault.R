# R/credential_vault.R
# =====================================================================
# Private credential vault for sensitive authentication state.
#
# Addresses audit finding C3: bearer tokens and API keys were stored in
# options(), making them visible to any code in the session via
# getOption() and str(options()).
#
# The vault is a private environment inside the package namespace.
# It is invisible to search(), ls(), save.image(), and getOption().
# Access is through exported getter/setter functions only.
# =====================================================================

#' @keywords internal
.vault <- new.env(parent = emptyenv())

# ------------------------------------------------------------------
# Internal helper: write-through for API-key backward compatibility
# ------------------------------------------------------------------
# During the transition period (0.2.x), the API key is mirrored to
# options(.EikonApiKey) so that user code calling getOption(".EikonApiKey")
# still works.  This write-through will be removed in 0.3.0.
.sync_api_key_to_options <- function(value) {
  options(.EikonApiKey = value)
  invisible(value)
}

# ==================================================================
# Public accessor functions
# ==================================================================

#' Set a credential in the secure vault
#'
#' Stores a value under a named key in the package-private credential vault.
#' The vault is not visible via \code{getOption()} or \code{options()}.
#'
#' For backward compatibility the \code{"api_key"} key is also written through
#' to \code{options(.EikonApiKey)}.  This behaviour will be removed in a future
#' release.
#'
#' @param key Character scalar — the credential name
#'   (e.g. \code{"api_key"}, \code{"access_token"}).
#' @param value The value to store.
#' @return \code{value} (invisibly).
#' @keywords internal
refinitiv_vault_set <- function(key, value) {
  if (!is.character(key) || length(key) != 1L) {
    stop("`key` must be a single character string", call. = FALSE)
  }
  assign(key, value, envir = .vault, inherits = FALSE)

  # Write-through for legacy compat (0.2.x only)
  if (identical(key, "api_key")) {
    .sync_api_key_to_options(value)
  }
  invisible(value)
}

#' Retrieve a credential from the secure vault
#'
#' Returns the stored value for \code{key}, or \code{NULL} if not present.
#' For the \code{"api_key"} key a fallback to \code{getOption(".EikonApiKey")}
#' is provided for backward compatibility.
#'
#' @param key Character scalar.
#' @return Stored value, or \code{NULL}.
#' @keywords internal
refinitiv_vault_get <- function(key) {
  if (!is.character(key) || length(key) != 1L) {
    stop("`key` must be a single character string", call. = FALSE)
  }
  if (exists(key, envir = .vault, inherits = FALSE)) {
    return(get(key, envir = .vault, inherits = FALSE))
  }
  # Legacy fallback: user may still have .EikonApiKey in .Rprofile / options()

  if (identical(key, "api_key")) {
    return(getOption(".EikonApiKey", default = NULL))
  }
  NULL
}

#' Check whether a credential exists in the vault
#'
#' @param key Character scalar.
#' @return Logical.
#' @keywords internal
refinitiv_vault_has <- function(key) {
  if (!is.character(key) || length(key) != 1L) {
    stop("`key` must be a single character string", call. = FALSE)
  }
  exists(key, envir = .vault, inherits = FALSE)
}

#' Clear credentials from the vault
#'
#' When called with no arguments, removes \strong{all} stored credentials.
#' When \code{keys} is supplied, only those keys are removed.
#'
#' @param keys Character vector of key names, or \code{NULL} (default) to
#'   clear everything.
#' @return \code{TRUE} (invisibly).
#' @keywords internal
refinitiv_vault_clear <- function(keys = NULL) {
  if (is.null(keys)) {
    rm(list = ls(envir = .vault, all.names = TRUE), envir = .vault)
    options(.EikonApiKey = NULL)
  } else {
    existing <- intersect(keys, ls(envir = .vault, all.names = TRUE))
    if (length(existing) > 0L) {
      rm(list = existing, envir = .vault)
    }
    if ("api_key" %in% keys) options(.EikonApiKey = NULL)
  }
  invisible(TRUE)
}

#' List keys currently stored in the vault
#'
#' Returns key names only — values are never shown.
#'
#' @return Character vector of key names.
#' @keywords internal
refinitiv_vault_keys <- function() {
  ls(envir = .vault, all.names = TRUE)
}

# ------------------------------------------------------------------
# Safe inspector (prevents accidental secret leakage)
# ------------------------------------------------------------------

#' Inspect the credential vault safely
#'
#' Returns a summary object that prints key names without exposing values.
#'
#' @return An object of class \code{"refinitiv_vault"}.
#' @export
refinitiv_vault <- function() {
  structure(
    list(keys = refinitiv_vault_keys()),
    class = "refinitiv_vault"
  )
}

#' @export
print.refinitiv_vault <- function(x, ...) {
  cat("<Refinitiv Secure Credential Vault>\n")
  if (length(x$keys) == 0L) {
    cat("  (empty)\n")
  } else {
    cat("  Stored keys: ", paste(x$keys, collapse = ", "), "\n", sep = "")
  }
  cat("  Use refinitiv_vault_get() to retrieve values securely.\n")
  invisible(x)
}
