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
# Performance:
#   A two-tier lookup avoids expensive sort() on repeated same-order calls:
#     Tier 1 — "fast key" (raw input order, no sort) → alias table → cache
#     Tier 2 — "canonical key" (sorted vectors/lists) → cache (creates alias)
#   Package version is cached at load time (see zzz.r) to avoid disk I/O.
#
# See ?rd_ClearCache and ?rd_CacheInfo for user-facing helpers.
# ──────────────────────────────────────────────────────────────────────────────

# Package-private cache store (session-scoped, not serialised)
.refinitiv_cache <- new.env(parent = emptyenv())

# Fast-key → canonical-key alias table (avoids re-sorting on same-order hits)
.fast_key_aliases <- new.env(parent = emptyenv())


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Get cached package version (set in .onLoad)
#'
#' Falls back to reading DESCRIPTION if called before .onLoad (e.g., in tests).
#' @noRd
get_pkg_version <- function() {
  ver <- .pkgglobalenv$pkg_version
  if (is.null(ver)) {
    ver <- as.character(utils::packageVersion("Refinitiv"))
    .pkgglobalenv$pkg_version <- ver
  }
  ver
}


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


#' Build a fast (unsorted) cache key
#'
#' Hashes arguments in their original order — no sorting, minimal overhead.
#' Used as the first tier of the two-tier lookup.
#'
#' @param fn_name Character scalar — the calling function's name.
#' @param ... Arbitrary R objects whose values affect the result.
#' @return A character scalar (hex hash).
#' @noRd
cache_key_fast <- function(fn_name, ...) {
  rlang::hash(c(list(fn_name, get_pkg_version()), list(...)))
}


#' Build a canonical (sorted) cache key
#'
#' Sorts character vectors and named lists so that reordered inputs produce
#' the same key.  Includes package version for invalidation on upgrade.
#' Uses radix sort for large character vectors (>1000 elements).
#'
#' @param fn_name Character scalar — the calling function's name.
#' @param ... Arbitrary R objects whose values affect the result.
#' @return A character scalar (hex hash).
#' @noRd
cache_key_canonical <- function(fn_name, ...) {
  args <- list(...)
  args <- lapply(args, function(a) {
    if (is.list(a)) {
      sort_named_recursive(a)
    } else if (is.character(a) && length(a) > 1L) {
      if (length(a) > 1000L) sort(a, method = "radix") else sort(a)
    } else {
      a
    }
  })
  rlang::hash(c(list(fn_name, get_pkg_version()), args))
}


#' Two-tier cache lookup
#'
#' Tries a fast (unsorted) key first, falling back to the canonical (sorted)
#' key on miss.  Creates an alias from fast → canonical on canonical hit so
#' that subsequent same-order lookups skip the expensive sort.
#'
#' @param fn_name Character scalar — the calling function's name.
#' @param ... Arguments that determine the cache key (same as \code{cache_key}).
#' @return A list with elements:
#'   \describe{
#'     \item{\code{found}}{Logical — whether a cache hit occurred.}
#'     \item{\code{value}}{The cached value (only when \code{found = TRUE}).}
#'     \item{\code{key}}{The canonical cache key (for use with
#'       \code{cache_set()} on a miss).}
#'   }
#' @noRd
cache_lookup <- function(fn_name, ...) {
  verbose <- isTRUE(getOption("refinitiv_verbose_cache", FALSE))
  fast <- cache_key_fast(fn_name, ...)

  # Tier 1: check if we have a fast-key → canonical-key alias
  if (exists(fast, envir = .fast_key_aliases, inherits = FALSE)) {
    can_key <- get(fast, envir = .fast_key_aliases, inherits = FALSE)
    hit <- cache_get(can_key)
    if (hit$found) {
      if (verbose) message("[RefinitivR cache] FAST-HIT via alias")
      return(list(found = TRUE, value = hit$value, key = can_key))
    }
    # Alias pointed to an expired/evicted entry — clean it up
    rm(list = fast, envir = .fast_key_aliases)
  }

  # Tier 2: compute canonical key (expensive for large vectors)
  can_key <- cache_key_canonical(fn_name, ...)

  # Fast key and canonical key might be identical (already sorted input)
  if (identical(fast, can_key)) {
    hit <- cache_get(can_key)
    return(list(found = hit$found, value = hit$value, key = can_key))
  }

  hit <- cache_get(can_key)
  if (hit$found) {
    # Create alias so next same-order call skips the sort
    assign(fast, can_key, envir = .fast_key_aliases)
    if (verbose) message("[RefinitivR cache] CANONICAL-HIT, alias created")
  }
  list(found = hit$found, value = hit$value, key = can_key)
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
  if (isFALSE(cache_param)) {
    return(FALSE)
  }
  if (isTRUE(cache_param)) {
    return(fn_default_ttl)
  }
  if (is.numeric(cache_param) && cache_param > 0) {
    return(cache_param)
  }
  FALSE
}


#' Store a value in the session cache
#'
#' @param key Character scalar (hash from \code{cache_key_canonical()} or
#'   \code{cache_lookup()$key}).
#' @param value Any R object to cache.
#' @param ttl Numeric seconds until expiry (\code{Inf} = session lifetime).
#' @noRd
cache_set <- function(key, value, ttl) {
  expires_at <- if (is.infinite(ttl)) Inf else as.numeric(Sys.time()) + ttl
  stored_val <- data.table::copy(value)
  assign(
    key,
    list(value = stored_val, expires_at = expires_at),
    envir = .refinitiv_cache
  )
  if (isTRUE(getOption("refinitiv_verbose_cache", FALSE))) {
    short_key <- substr(key, 1L, 8L)
    size_kb <- round(as.numeric(utils::object.size(stored_val)) / 1024, 1)
    message("[RefinitivR cache] STORE (key ", short_key, "..., TTL ", ttl, "s, ~", size_kb, " KB)")
  }
  invisible(NULL)
}


#' Retrieve a value from the session cache
#'
#' Returns \code{list(found = TRUE, value = ...)} on a hit and
#' \code{list(found = FALSE)} on a miss.
#' Expired entries are deleted immediately (lazy eviction) to free memory.
#'
#' When \code{getOption("refinitiv_verbose_cache", FALSE)} is \code{TRUE},
#' logs cache hits, misses, and expirations via \code{message()}.
#'
#' @param key Character scalar.
#' @return A list with element \code{found} (logical) and, on a hit,
#'   \code{value}.
#' @noRd
cache_get <- function(key) {
  verbose <- isTRUE(getOption("refinitiv_verbose_cache", FALSE))

  if (!exists(key, envir = .refinitiv_cache, inherits = FALSE)) {
    if (verbose) {
      short_key <- substr(key, 1L, 8L)
      message("[RefinitivR cache] MISS (key ", short_key, "...)")
    }
    return(list(found = FALSE))
  }
  entry <- get(key, envir = .refinitiv_cache, inherits = FALSE)
  if (is.finite(entry$expires_at) && as.numeric(Sys.time()) > entry$expires_at) {
    rm(list = key, envir = .refinitiv_cache)
    if (verbose) {
      short_key <- substr(key, 1L, 8L)
      message("[RefinitivR cache] EXPIRED (key ", short_key, "...)")
    }
    return(list(found = FALSE))
  }
  if (verbose) {
    short_key <- substr(key, 1L, 8L)
    ttl_left <- if (is.infinite(entry$expires_at)) Inf else round(entry$expires_at - as.numeric(Sys.time()))
    message("[RefinitivR cache] HIT (key ", short_key, "..., ", ttl_left, "s remaining)")
  }
  list(found = TRUE, value = data.table::copy(entry$value))
}


#' Restore caller's input instrument order on a post-processed data.frame
#'
#' When caching is active, RICs/symbols are sorted before chunking so that
#' chunk boundaries are deterministic regardless of caller ordering. This
#' helper reorders the resulting data.frame rows back to the caller's input
#' sequence while preserving the relative order of multiple rows per instrument.
#'
#' @param df A \code{data.frame} (or non-data.frame returned unchanged).
#' @param input_vec Character vector of requested instruments in caller order.
#' @param col_name Name of the instrument column in \code{df}.
#' @return \code{df} reordered to match \code{input_vec}.
#' @noRd
.restore_input_order <- function(df, input_vec, col_name = "Instrument") {
  is_eikon_res <- inherits(df, "EikonResult") ||
    (is.list(df) && !is.data.frame(df) && "PostProcessedEikonGetData" %in% names(df))

  target_df <- if (is_eikon_res) df$PostProcessedEikonGetData else df

  if (!is.data.frame(target_df) || nrow(target_df) <= 1L || !(col_name %in% names(target_df))) {
    return(df)
  }
  clean_input <- as.character(input_vec[!is.na(input_vec)])
  if (length(unique(clean_input)) <= 1L) {
    return(df)
  }
  col_idxs <- which(names(target_df) == col_name)
  col_vals <- as.character(target_df[[col_idxs[1L]]])
  if (length(col_idxs) > 1L) {
    for (ci in col_idxs) {
      cand <- as.character(target_df[[ci]])
      if (all(cand[!is.na(cand)] %in% clean_input)) {
        col_vals <- cand
        break
      }
    }
  }

  if (!anyDuplicated(clean_input)) {
    pos <- match(col_vals, clean_input)
  } else {
    pos <- rep(NA_integer_, length(col_vals))
    for (u in unique(clean_input)) {
      in_locs <- which(clean_input == u)
      df_locs <- which(!is.na(col_vals) & col_vals == u)
      n_df <- length(df_locs)
      if (n_df > 0L) {
        n_in <- length(in_locs)
        if (n_in == 1L) {
          pos[df_locs] <- in_locs[1L]
        } else {
          block_idx <- pmin(n_in, ceiling(seq_len(n_df) * n_in / n_df))
          pos[df_locs] <- in_locs[block_idx]
        }
      }
    }
  }

  if (all(is.na(pos))) {
    return(df)
  }
  ord <- order(pos, seq_len(nrow(target_df)), na.last = TRUE)
  if (identical(ord, seq_len(nrow(target_df)))) {
    return(df)
  }

  target_df <- target_df[ord, , drop = FALSE]
  rownames(target_df) <- NULL

  if (!is_eikon_res) {
    return(target_df)
  }

  df$PostProcessedEikonGetData <- target_df
  err_df <- df$Eikon_Error_Data
  if (is.data.frame(err_df) && nrow(err_df) > 0L &&
      "row" %in% names(err_df) && is.numeric(err_df$row)) {
    old_1based <- as.integer(err_df$row) + 1L
    valid <- !is.na(old_1based) & old_1based >= 1L & old_1based <= length(ord)
    if (any(valid)) {
      err_df$row[valid] <- match(old_1based[valid], ord) - 1L
      err_ord <- order(err_df$row, seq_len(nrow(err_df)))
      err_df <- err_df[err_ord, , drop = FALSE]
      rownames(err_df) <- NULL
      df$Eikon_Error_Data <- err_df
    }
  }
  df
}


# ── Exported helpers ─────────────────────────────────────────────────────────

#' Clear the RefinitivR Session Cache
#'
#' Removes all cached API responses, fast-key aliases, and resets the cached
#' package version from the current R session.
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
  # Clear fast-key aliases
  alias_keys <- ls(envir = .fast_key_aliases, all.names = TRUE)
  if (length(alias_keys) > 0L) {
    rm(list = alias_keys, envir = .fast_key_aliases)
  }
  # Reset cached package version (re-read on next access)
  .pkgglobalenv$pkg_version <- NULL
  # Also reset the connection singleton
  .connection_cache$conn <- NULL
  message("[RefinitivR] Cache cleared.")
  invisible(TRUE)
}


#' Show RefinitivR Cache Statistics
#'
#' Reports the number of active and expired entries in the session cache,
#' together with an estimated total size in megabytes and the number of
#' fast-key aliases.
#'
#' @return A list with elements \code{total_keys}, \code{active_keys},
#'   \code{expired_keys}, \code{aliases}, and \code{estimated_size_mb},
#'   returned invisibly.
#'   A summary is also printed via \code{message()}.
#' @export
#' @seealso \code{\link{rd_ClearCache}}
#' @examples
#' rd_CacheInfo()
rd_CacheInfo <- function() {
  keys <- ls(envir = .refinitiv_cache, all.names = TRUE)
  now <- as.numeric(Sys.time())

  active <- 0L
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

  n_aliases <- length(ls(envir = .fast_key_aliases, all.names = TRUE))

  info <- list(
    total_keys = length(keys),
    active_keys = active,
    expired_keys = expired,
    aliases = n_aliases,
    estimated_size_mb = round(size_bytes / (1024^2), 2)
  )

  message(
    sprintf(
      "[RefinitivR] Cache: %d active, %d expired, %d aliases, ~%.2f MB",
      active, expired, n_aliases, info$estimated_size_mb
    )
  )
  invisible(info)
}
