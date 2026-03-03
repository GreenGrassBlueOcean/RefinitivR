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
#' @param verbose Logical. If \code{TRUE}, print download status after each
#'   chunk. Default: \code{FALSE}.
#' @param fail_message Character string used in the stop/warning on
#'   failure.
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
                             verbose = FALSE,
                             fail_message = "downloading data failed") {
  on_failure <- match.arg(on_failure)

  results <- as.list(rep(NA, n_chunks))

  coordinator <- data.frame(
    index = seq_len(n_chunks),
    success = rep(FALSE, n_chunks),
    retries = rep(0L, n_chunks),
    stringsAsFactors = FALSE
  )

  while (!all(coordinator$success) && !any(coordinator$retries > max_retries)) {
    pending <- which(!coordinator$success)

    for (j in pending) {
      results[[j]] <- try(fetch_fn(j))
      Sys.sleep(sleep)

      if (is_success(results[[j]])) {
        coordinator$success[j] <- TRUE
      }

      if (verbose) {
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

  if (any(coordinator$retries > max_retries)) {
    if (on_failure == "stop") {
      stop(fail_message)
    } else {
      warning(fail_message)
    }
  }

  results
}
