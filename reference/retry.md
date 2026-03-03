# Retry a function call with exponential backoff

Retries a zero-argument function up to `max_attempts` times. On failure,
waits with exponential backoff before retrying.

## Usage

``` r
retry(
  fn,
  max_attempts = getOption("refinitiv_max_retries", 2L),
  backoff = 0.5,
  on_failure = c("stop", "NA")
)
```

## Arguments

- fn:

  A zero-argument function to execute. Wrap your call in
  `function() your_call()` before passing.

- max_attempts:

  Maximum number of attempts (including the first). Defaults to
  `getOption("refinitiv_max_retries", 2L)`.

- backoff:

  Initial backoff time in seconds. Doubles after each failure, default =
  0.5.

- on_failure:

  What to do when all attempts are exhausted. `"stop"` (default) throws
  an error containing the last failure message. `"NA"` issues a warning
  and returns `NA` — use this inside `chunked_download()` where
  per-chunk success is tracked externally.

## Value

The return value of `fn()` on success.

## Details

`retry()` is the **client-side** retry layer. It guards against
transient network errors and unexpected exceptions.

**Inside `chunked_download()`:** Use
`retry(fn, max_attempts = 1L, on_failure = "NA")` so that `retry()` acts
solely as an error-to-`NA` converter. The coordinator owns the full
retry lifecycle (jittered exponential backoff between rounds, per-chunk
tracking).

**Standalone callers** (e.g., `rd_GetESG`): Use the default
`max_attempts` (or configure via the `refinitiv_max_retries` option) for
direct retrying with exponential backoff.

It is distinct from two other retry/polling mechanisms in the package:

- **[`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)**
  inside `send_json_request()` handles HTTP-level transient errors (429
  Too Many Requests, 503 Service Unavailable) transparently during
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html).

- The **server polling loop** in `send_json_request()` handles
  LSEG-specific JSON responses: async ticket/estimated-duration polling
  and retriable error codes (2504, 500, 400).

## Examples

``` r
# Succeeds immediately:
Refinitiv:::retry(function() sum(1, 1))
#> [1] 2

# Fails and retries, then gives up:
if (FALSE) { # \dontrun{
retry(function() sum(1, "a"), max_attempts = 2)
} # }
```
