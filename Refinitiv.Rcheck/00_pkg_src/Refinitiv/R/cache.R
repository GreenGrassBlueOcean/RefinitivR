# ──────────────────────────────────────────────────────────────────────────────
# Session-scoped request cache for RefinitivR
# ──────────────────────────────────────────────────────────────────────────────
#
# All cached data lives in a package-private environment that dies when the R
# session ends.  No disk persistence, no cross-session staleness.
#
# Caching is opt-in:
#   * Global:   options(refinitiv_cache = TRUE)   — uses per-function default TTLs
#               options(refinitiv_cache = 600)    — uniform 600 s TTL everywhere
#   * Per-call: cache = TRUE / FALSE / <seconds>  — overrides global setting
#
# See ?rd_ClearCache and ?rd_CacheInfo for user-facing helpers.
# ──────────────────────────────────────────────────────────────────────────────

# Package-private cache store (session-scoped, not serialised)
.refinitiv_cache <- new.env(parent = emptyenv())


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Recursively sort named lists for stable hashing
#'
#' Ensures \code{list(a = 1, b = 2)} and \code{list(b = 2, a = 1)} (and nested
#' equivalents) produce the same hash key.
#'
#' @param x Any R object.  Only named lists are reordered.
#' @return The same object with named list elements sorted alphabetically at
#'   every nesting level.
#' @noRd
sort_named_recursive <- function(x) {
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && all(nzchar(nms))) {
      x <- x[order(nms)]
    }
    x <- lapply(x, sort_named_recursive)
  }
  x
}


#' Build a deterministic cache key
#'
#' Includes the package version so that a mid-session upgrade automatically
#' invalidates stale entries (response format may have changed).
#'
#' @param fn_name Character scalar — the calling function's name.
#' @param ... Arbitrary R objects whose values affect the result.
#'   Named list arguments are recursively sorted before hashing.
#' @return A character scalar (hex hash).
#' @noRd
cache_key <- function(fn_name, ...) {
  args <- list(...)
  args <- lapply(args, function(a) {
    if (is.list(a)) sort_named_recursive(a) else a
  })
  pkg_version <- as.character(utils::packageVersion("Refinitiv"))
  rlang::hash(c(list(fn_name, pkg_version), args))
}


#' Resolve the effective TTL for this call
#'
#' Priority: per-call \code{cache} param > global \code{refinitiv_cache} option.
#'
#' @param cache_param Value of the function's \code{cache} argument
#'   (\code{NULL}, \code{TRUE}, \code{FALSE}, or numeric seconds).
#' @param fn_default_ttl Numeric — the function-specific default TTL in seconds.
#' @return \code{FALSE} (no caching) or a positive numeric TTL.
#' @noRd
resolve_cache <- function(cache_param, fn_default_ttl) {
  if (is.null(cache_param)) {
    cache_param <- getOption("refinitiv_cache", FALSE)
  }
  if (isFALSE(cache_param))                           return(FALSE)
  if (isTRUE(cache_param))                            return(fn_default_ttl)
  if (is.numeric(cache_param) && cache_param > 0)     return(cache_param)
  FALSE
}


#' Store a value in the session cache
#'
#' @param key Character scalar (hash from \code{cache_key()}).
#' @param value Any R object to cache.
#' @param ttl Numeric seconds until expiry (\code{Inf} = session lifetime).
#' @noRd
cache_set <- function(key, value, ttl) {
  expires_at <- if (is.infinite(ttl)) Inf else as.numeric(Sys.time()) + ttl
  assign(
    key,
    list(value = value, expires_at = expires_at),
    envir = .refinitiv_cache
  )
  invisible(NULL)
}


#' Retrieve a value from the session cache
#'
#' Returns \code{list(found = TRUE, value = ...)} on a hit and
#' \code{list(found = FALSE)} on a miss.
#' Expired entries are deleted immediately (lazy eviction) to free memory.
#'
#' @param key Character scalar.
#' @return A list with element \code{found} (logical) and, on a hit,
#'   \code{value}.
#' @noRd
cache_get <- function(key) {
  if (!exists(key, envir = .refinitiv_cache, inherits = FALSE)) {
    return(list(found = FALSE))
  }
  entry <- get(key, envir = .refinitiv_cache, inherits = FALSE)
  if (is.finite(entry$expires_at) && as.numeric(Sys.time()) > entry$expires_at) {
    rm(list = key, envir = .refinitiv_cache)
    return(list(found = FALSE))
  }
  list(found = TRUE, value = entry$value)
}


# ── Exported helpers ─────────────────────────────────────────────────────────

#' Clear the RefinitivR Session Cache
#'
#' Removes all cached API responses from the current R session.
#'
#' @return Invisibly \code{TRUE}.
#' @export
#' @seealso \code{\link{rd_CacheInfo}}
#' @examples
#' rd_ClearCache()
rd_ClearCache <- function() {
  keys <- ls(envir = .refinitiv_cache, all.names = TRUE)
  if (length(keys) > 0L) {
    rm(list = keys, envir = .refinitiv_cache)
  }
  # Also reset the connection singleton
  .connection_cache$conn <- NULL
  message("[RefinitivR] Cache cleared.")
  invisible(TRUE)
}


#' Show RefinitivR Cache Statistics
#'
#' Reports the number of active and expired entries in the session cache,
#' together with an estimated total size in megabytes.
#'
#' @return A list with elements \code{total_keys}, \code{active_keys},
#'   \code{expired_keys}, and \code{estimated_size_mb}, returned invisibly.
#'   A summary is also printed via \code{message()}.
#' @export
#' @seealso \code{\link{rd_ClearCache}}
#' @examples
#' rd_CacheInfo()
rd_CacheInfo <- function() {
  keys <- ls(envir = .refinitiv_cache, all.names = TRUE)
  now  <- as.numeric(Sys.time())

  active  <- 0L
  expired <- 0L
  size_bytes <- 0

  for (k in keys) {
    entry <- get(k, envir = .refinitiv_cache, inherits = FALSE)
    if (is.infinite(entry$expires_at) || now <= entry$expires_at) {
      active <- active + 1L
      size_bytes <- size_bytes + as.numeric(utils::object.size(entry$value))
    } else {
      expired <- expired + 1L
    }
  }

  info <- list(
    total_keys       = length(keys),
    active_keys      = active,
    expired_keys     = expired,
    estimated_size_mb = round(size_bytes / (1024^2), 2)
  )

  message(
    sprintf(
      "[RefinitivR] Cache: %d active, %d expired, ~%.2f MB",
      active, expired, info$estimated_size_mb
    )
  )
  invisible(info)
}
