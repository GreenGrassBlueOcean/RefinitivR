#' Emit a progress message respecting the \code{refinitiv_progress} option
#'
#' @param ... Passed to \code{message()} after the \code{[RefinitivR]} prefix.
#' @param verbose_only If \code{TRUE}, only emit when the option is
#'   \code{"verbose"}.
#' @param force If \code{TRUE}, emit regardless of the option value (except
#'   when explicitly \code{FALSE}).
#'
#' @return Invisible \code{NULL}.
#' @keywords internal
#' @noRd
progress_msg <- function(..., verbose_only = FALSE, force = FALSE) {
  mode <- getOption("refinitiv_progress", TRUE)
  if (!force) {
    if (isFALSE(mode)) return(invisible())
    if (verbose_only && !identical(mode, "verbose")) return(invisible())
  }
  # force = TRUE still respects explicit FALSE

  if (force && isFALSE(mode)) return(invisible())
  message("[RefinitivR] ", ...)
}


#' Execute a fetch function over chunks with retry coordination
#'
#' Iterates over chunks, calling \code{fetch_fn(i)} for each chunk index
#' \code{i}. Failed chunks are retried up to \code{max_retries} times
#' with jittered exponential backoff between retry rounds.
#'
#' \code{fetch_fn} should use \code{retry(fn, max_attempts = 1L,
#' on_failure = "NA")} internally so that errors are converted to \code{NA}
#' without inner retrying — this function owns the full retry lifecycle.
#'
#' @param n_chunks Integer. Number of chunks to process.
#' @param fetch_fn Function taking a single integer index and returning
#'   the result for that chunk. Use \code{retry(fn, max_attempts = 1L,
#'   on_failure = "NA")} inside to convert errors to \code{NA}.
#' @param is_success Function taking a result and returning \code{TRUE} or
#'   \code{FALSE}. Default checks that the result is not identical to
#'   \code{NA}.
#' @param max_retries Maximum number of retry rounds for failed chunks.
#'   Total attempts per chunk = 1 (initial) + max_retries. Default: 3L.
#' @param sleep Seconds to sleep between individual chunk fetches within
#'   a single round. Default: 0.1.
#' @param backoff Initial backoff in seconds between retry rounds.
#'   Doubles after each round (exponential) with uniform jitter
#'   (x0.5--1.5). Set to 0 to disable inter-round backoff.
#'   Default: 1.0.
#' @param on_failure Either \code{"stop"} or \code{"warning"}. Action to
#'   take when chunks are still failing after \code{max_retries} rounds.
#'   Default: \code{"stop"}.
#' @param verbose Controls progress output. \code{TRUE} / \code{FALSE} /
#'   \code{"verbose"} — see \code{progress_msg()}.  The default
#'   (\code{getOption("refinitiv_progress", TRUE)}) provides compact
#'   progress for multi-chunk requests; \code{"verbose"} adds the full
#'   coordinator dump.
#' @param fail_message Character string used in the stop/warning on
#'   failure.
#' @param caller_label Optional character label used in progress messages
#'   (e.g. \code{"rd_GetData"}). If \code{NULL}, messages omit the label.
#' @param chunk_sizes Optional integer vector of per-chunk element counts,
#'   used in progress messages. Must have length \code{n_chunks} when
#'   supplied.
#'
#' @return A list of length \code{n_chunks} with results from
#'   \code{fetch_fn}.
#'
#' @importFrom utils capture.output
#' @importFrom stats runif
#' @keywords internal
#' @noRd
chunked_download <- function(n_chunks,
                             fetch_fn,
                             is_success = function(x) !identical(x, NA),
                             max_retries = 3L,
                             sleep = 0.1,
                             backoff = 1.0,
                             on_failure = c("stop", "warning"),
                             verbose = getOption("refinitiv_progress", TRUE),
                             fail_message = "downloading data failed",
                             caller_label = NULL,
                             chunk_sizes = NULL,
                             chunk_cache_key_fn = NULL,
                             cache_ttl = FALSE) {
  on_failure <- match.arg(on_failure)

  # Resolve progress mode from the verbose parameter
  mode <- verbose
  show_progress <- !isFALSE(mode)
  show_verbose   <- identical(mode, "verbose")
  # One-chunk rule: skip routine progress when n_chunks == 1 and mode is
  # plain TRUE (not "verbose")
  one_chunk_quiet <- (n_chunks == 1L && isTRUE(mode))

  use_chunk_cache <- !isFALSE(cache_ttl) && !is.null(chunk_cache_key_fn)

  results <- as.list(rep(NA, n_chunks))

  coordinator <- data.frame(
    index = seq_len(n_chunks),
    success = rep(FALSE, n_chunks),
    retries = rep(0L, n_chunks),
    stringsAsFactors = FALSE
  )

  # Prefix for labelled messages
  lbl <- if (!is.null(caller_label)) paste0(caller_label, ": ") else ""

  # ── Per-chunk cache: check all chunks up front ──
  if (use_chunk_cache) {
    for (j in seq_len(n_chunks)) {
      ck <- chunk_cache_key_fn(j)
      hit <- cache_get(ck)
      if (hit$found) {
        results[[j]] <- hit$value
        coordinator$success[j] <- TRUE
        if (show_progress && !one_chunk_quiet) {
          progress_msg(lbl, "Chunk ", j, "/", n_chunks, " [cached]")
        }
      }
    }
  }

  t_start <- proc.time()[["elapsed"]]

  while (!all(coordinator$success) && !any(coordinator$retries > max_retries)) {
    pending <- which(!coordinator$success)

    for (j in pending) {
      t_chunk <- proc.time()[["elapsed"]]
      results[[j]] <- try(fetch_fn(j))
      chunk_elapsed <- proc.time()[["elapsed"]] - t_chunk
      Sys.sleep(sleep)

      if (is_success(results[[j]])) {
        coordinator$success[j] <- TRUE

        # Cache this chunk on success
        if (use_chunk_cache) {
          cache_set(chunk_cache_key_fn(j), results[[j]], cache_ttl)
        }

        # Per-chunk success message (skip for one-chunk-quiet)
        if (show_progress && !one_chunk_quiet) {
          size_info <- if (!is.null(chunk_sizes) && length(chunk_sizes) >= j) {
            paste0(" (", format(chunk_sizes[j], big.mark = ","), " items, ",
                   sprintf("%.1fs", chunk_elapsed), ")")
          } else {
            paste0(" (", sprintf("%.1fs", chunk_elapsed), ")")
          }
          progress_msg(lbl, "Chunk ", j, "/", n_chunks, " done", size_info)
        }
      } else if (show_progress && !one_chunk_quiet) {
        round_num <- coordinator$retries[j]
        progress_msg(lbl, "Chunk ", j, "/", n_chunks,
                     " failed, will retry (attempt ", round_num + 1L,
                     "/", max_retries + 1L, ")")
      }

      if (show_verbose) {
        message(
          "Download Status:\n",
          paste(capture.output(coordinator), collapse = "\n")
        )
      }
    }

    coordinator$retries[!coordinator$success] <-
      coordinator$retries[!coordinator$success] + 1

    # Jittered exponential backoff before retrying failed chunks
    if (backoff > 0 && !all(coordinator$success)) {
      round <- max(coordinator$retries)
      if (round <= max_retries) {
        wait <- backoff * (2^(round - 1)) * stats::runif(1, 0.5, 1.5)
        Sys.sleep(wait)
      }
    }
  }

  total_elapsed <- proc.time()[["elapsed"]] - t_start
  n_success <- sum(coordinator$success)

  if (any(coordinator$retries > max_retries)) {
    if (show_progress && !one_chunk_quiet) {
      progress_msg(lbl, "Download incomplete: ", n_success, "/", n_chunks,
                   " chunks succeeded in ", sprintf("%.1fs", total_elapsed))
    }
    if (on_failure == "stop") {
      stop(fail_message)
    } else {
      warning(fail_message)
    }
  } else if (show_progress && !one_chunk_quiet) {
    n_from_cache <- sum(coordinator$success) - sum(coordinator$retries > 0L | !coordinator$success)
    cache_note <- if (use_chunk_cache && n_from_cache > 0L) {
      paste0(" (", n_from_cache, " from cache)")
    } else {
      ""
    }
    progress_msg(lbl, "Download complete: ", n_chunks, "/", n_chunks,
                 " chunks succeeded in ", sprintf("%.1fs", total_elapsed),
                 cache_note)
  }

  results
}
