# Refinitiv 0.2.1

## New Features

* **`rd_ConvertSymbol()`** — New function for converting between symbology
  types (RIC, ISIN, CUSIP, SEDOL, etc.) with multi-tiered fallback:
  SymbologySearch → EikonGetSymbology → GetData. Supports DelistingDate
  natively and handles ISIN→RIC, RIC→ISIN, RIC→MIC, and more.

* **`fields` parameter alias in `rd_GetData()`** — `fields` can now be used
  as an alias for `Eikonformulas`, aligning the R API with the `lseg-data`
  Python library. `Eikonformulas` continues to work but is soft-deprecated.

* **`EikonResult` S3 class** — `EikonGetData()` now returns an `EikonResult`
  object that wraps both data and error components. `as.data.table()` and
  `as.data.frame()` methods extract only the data portion, eliminating
  recycling warnings. Fully backward-compatible: `$PostProcessedEikonGetData`
  and `$Eikon_Error_Data` still work.

## Bug Fixes

* **Cache key canonicalization (STOXX-2)** — `cache_key()` now sorts character
  vectors before hashing, ensuring the same set of RICs produces the same
  cache key regardless of input order. Diagnostic logging available via
  `options(refinitiv_verbose_cache = TRUE)`.

* **`EikonGetSymbology()` multi-chunk fix (STOXX-3)** —
  `EikonGetSymbology()` now correctly combines results across multiple
  chunks. Previously only the first chunk was processed, silently discarding
  all subsequent chunks for requests with >300 symbols.

## CI/CD & Infrastructure

* **Windows MAX_PATH fix** — Shortened httptest2 API fixture paths in the
  redactor (`api/rdp/data/` → `d/`, `news/v1/top-news` → `tn`, etc.) and
  removed an accidentally committed `Refinitiv.Rcheck/` directory (456 files)
  that contained old long-path fixtures. Package download size dropped from
  6.23 MB to 2.83 MB.

* **Installation test workflow** — New `install-test.yaml` validates that
  the package installs and loads successfully on Ubuntu, macOS, and Windows.

* **Node.js 24 migration** — Bumped `actions/checkout` to v5 and
  `codecov/codecov-action` to v6 (both native Node 24). Removed the
  `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24` workaround.

* **Modern installation docs** — README now recommends `pak::pak()` instead
  of the deprecated `devtools::install_github()`.

* **`.gitignore` / `.Rbuildignore` hardened** — Added `*.Rcheck/` patterns
  to prevent future accidental commits of R CMD check output.

# Refinitiv 0.2.0

## Breaking Changes

* **Python/reticulate support removed.** The package now uses direct JSON
  API calls exclusively. No Python installation is required.
  - `install_eikon()` and `RDPGetOptionAnalytics()` are now defunct
    (`.Defunct()`).
  - The `PythonModule` parameter in `RDConnect()` and `EikonConnect()` is
    deprecated and ignored; passing it triggers a warning.
  - `reticulate`, `archivist`, and `digest` removed from dependencies.
  - Deleted `R/PyJsonConvertor.r` (Python-only helper).

## New Features

* **Real-time streaming data** — New WebSocket-based streaming API via R6
  classes (`rd_get_streaming_data()`, `StreamManager`, `Stream`,
  `StreamDefinition`). Supports multi-instrument subscriptions, callbacks
  (`on_refresh`, `on_update`, `on_error`), data history buffering, summary
  statistics, and built-in live plotting with Shiny.
  - Auto-reconnect with jittered exponential backoff and automatic
    re-subscription on disconnect.
  - Sequential stream IDs for multi-instrument support.
  - Debug logging via `options(refinitiv_streaming_debug = TRUE)`.

* **ESG data** — `rd_GetESG()` retrieves ESG scores, measures, basic
  overviews, and universe data with optional fiscal year filtering.

* **Consensus estimates** — `rd_GetEstimates()` retrieves I/B/E/S actuals,
  summary, and KPI data across 15 views with package-level validation.

* **Ownership data** — `rd_GetOwnership()` retrieves consolidated, fund,
  insider, investor, and organizational ownership data across 19 views with
  automatic pagination.

* **Session-scoped request cache** — Opt-in caching layer (`R/cache.R`)
  with configurable TTLs per function. Enable globally with
  `options(refinitiv_cache = TRUE)` or per-call with `cache = TRUE`.
  Helpers: `rd_ClearCache()`, `rd_CacheInfo()`. Instrumented across 13
  exported functions.

* **Custom instruments** — `rd_ManageCustomInstruments()` for create, read,
  update, and delete operations, including basket instruments with custom
  holidays.

* **Zero-config connection** — `rd_connection()` provides a lazy, cached
  singleton connection. All 20+ API functions default to it, eliminating the
  need to pass connection objects explicitly.

* **Environment variable configuration** — Set `REFINITIV_PORT`,
  `REFINITIV_BASE_URL`, and/or `REFINITIV_APP_KEY` in `.Renviron` for
  headless/CI environments. Skips interactive terminal detection.

* **`EikonRepairMic()` updated** — Operating MIC lookup data refreshed for
  current exchange codes.

## Bug Fixes & Improvements

* **`retry()` rewritten (C2)** — Now accepts a zero-argument closure with
  iterative loop and exponential backoff. Max attempts configurable via
  `options(refinitiv_max_retries)`. Previously the retry mechanism did not
  actually re-execute failed calls.

* **`retry()` throws on exhaustion by default (#17)** — `retry()` now
  defaults to `on_failure = "stop"`, throwing an error with the last failure
  message when all attempts are exhausted. Previously it returned `NA` with a
  warning, causing confusing downstream errors. Use `on_failure = "NA"` for
  the `chunked_download()` use case where per-chunk success is tracked
  externally.

* **HTTP 429/503 rate-limit handling (#18)** — `send_json_request()` now uses
  `httr2::req_retry()` to automatically retry on HTTP 429 (Too Many Requests)
  and 503 (Service Unavailable) responses with jittered exponential backoff.
  Respects `Retry-After` headers in both seconds and HTTP-date formats.
  Configurable via `options(refinitiv_rate_limit_max_wait)` (default 60s).

* **Retry architecture documented (#19)** — The three-layer retry system
  (`retry()`, `httr2::req_retry()`, server polling loop) is now documented in
  `ARCHITECTURE.md` and `retry()` roxygen.

* **DELETE dead assignment fixed (#21)** — Removed unreachable `results <- NA`
  after `break` in `send_json_request()` DELETE code path.

* **Package version in cache key (#22)** — `cache_key()` now includes the
  package version, so a mid-session upgrade automatically invalidates stale
  cached entries.

* **Credential vault (C3)** — Bearer tokens are now stored in a
  package-private environment invisible to `getOption()`, `save.image()`,
  and `str(options())`, replacing the previous plaintext `options()` storage.

* **Token expiry buffer (H1)** — `rd_VerifyToken()` subtracts a 60-second
  buffer (configurable via `options(refinitiv_token_buffer_seconds)`) before
  checking token expiration, preventing clock-skew failures.

* **Unified HTTP request handling (H2)** — Extracted `build_request()`
  helper to DRY four HTTP method branches in `send_json_request()`. All
  methods now use consistent error handling. Debug output changed from
  `print()` to `message()`. Timeouts normalized to 30s across all methods.

* **Honest handshake identity (H5)** — Handshake now identifies as
  `RefinitivR` with the actual package version, replacing the previously
  spoofed Python library identity.

* **Hostname leakage fixed (M4)** — Streaming login uses
  `127.0.0.1`/`localhost` instead of the real hostname.

* **`NA_cleaning` default flipped (M3)** — `rd_OutputProcesser()` now
  defaults to `NA_cleaning = FALSE`, matching the behavior of all internal
  callers.

* **`chunked_download()` extracted (M2)** — Generic chunk-retry coordinator
  replaces 7 duplicated download-coordinator patterns. Supports
  parameterized sleep, max retries, success predicates, and configurable
  failure behavior.

* **Search port bug fixed** — `Construct_url("rdp")` and bearer token
  handshake URL now correctly use `eikon_port` instead of `rdp_port`,
  fixing silent fallback to port 80 when `rdp_port` was `NULL`.

* **`HistoricalPricing.r` `setnames` crash fixed** — No longer fails when
  multiple V-prefixed columns are present in the response.

* **`EikonTimeSeriesPreprocessor` NA handling** — Defensive checks prevent
  crashes when `Duration` or `Limit` are `NA`.

* **Int64 overflow fixed (Q1)** — Large integer values (exceeding 2^31-1)
  from LSEG responses are now converted with `as.numeric()` instead of
  `as.integer()`, preventing silent truncation.

* **`retry()` handles NULL returns (A1)** — `retry()` now correctly treats
  `NULL` as a valid return value rather than a failure signal.

* **`rd_GetHistory()` merge improvements (Q2)** — Duplicate column
  deduplication, granularity mismatch warning, NA-row filtering, and
  `merge_info` attribute on merged results.

* **Type coercion guard tightened (Q4)** — Column type coercion now
  requires all non-NA values to be parseable before converting.

* **Optimal chunking (Q5)** — Even-distribution chunking eliminates runt
  chunks and unnecessary extra API calls.

* **Linear backoff in polling loop (Q6)** — Server polling now uses linear
  backoff between iterations instead of fixed sleep.

* **UTC timezone standardization (Q7)** — All date conversions now
  explicitly use `tz = "UTC"`.

* **`EikonGetSymbology()` multi-chunk fix (STOXX-3)** —
  `EikonGetSymbology()` now correctly combines results across multiple
  chunks. Previously, `ProcessSymbology()` only processed the first
  chunk, silently discarding all subsequent chunks. Requests with >300
  symbols returned only ~250 rows instead of the full result set.

## Test Infrastructure

* **httptest2 integration** — 24 fixture sets recorded against live
  Workspace terminal with a custom redactor (strips auth headers, normalizes
  port to 9000). All fixtures verified clean of API keys and real ports.

* **Golden sample cross-validation** — 11 scenarios with both R
  (`golden_samples_r.rds`) and Python (`truth_lseg_data.json`) truth files,
  23 cross-validation tests.

* **Test suite results** — FAIL 0 | WARN 0 | SKIP 42 | PASS 1382 (1424
  expectations, measured via `covr::package_coverage()`). 86.8% code
  coverage across 35 source files. 12 files at 100% coverage.

* **Option pollution fixed** — All `options()` mutations in tests wrapped
  with `withr::local_options()` to prevent cross-test contamination.

## CI

* **Removed Python/Conda/reticulate** from `R-CMD-check.yaml` and
  `test-coverage.yaml` workflows. Both now use standard `r-lib/actions`
  patterns with automatic R package caching.

# Refinitiv 0.1.3

* Previous release with Python/reticulate support.
