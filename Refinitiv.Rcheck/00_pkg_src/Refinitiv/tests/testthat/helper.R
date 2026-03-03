# Helper functions for RefinitivR test suite
#
# Provides utilities for setting up httptest2-based tests.
#
# NOTE: pkgload::load_all() (called by devtools::test()) sources this file
# into the package:Refinitiv environment, NOT into .GlobalEnv.  When tests
# run in-process (same PID as the interactive session), a stale version of
# helper functions in .GlobalEnv can shadow the ones sourced here.  We
# therefore explicitly assign critical helpers to .GlobalEnv below.

#' Create a mock JSON connection environment for httptest2 tests.
#'
#' Sets the required global options and returns a RefinitivJsonConnect object.
#' Use inside test_that() blocks wrapped with httptest2::with_mock_dir().
#'
#' @param api_key Character. Dummy API key (default: "dummy_key").
#' @param port Integer. Port to use (default: 9000, matches fixtures).
#' @return An environment from RefinitivJsonConnect().
#' @noRd
setup_mock_json_env <- function(api_key = "dummy_key", port = 9000L) {
  refinitiv_vault_set("api_key", api_key)
  options(
    eikon_port = port,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  )
  RefinitivJsonConnect(api_key)
}

#' Check if a live Eikon/Workspace API is available for testing.
#'
#' Performs a lightweight handshake against the local proxy to verify that
#' (a) the proxy is running, (b) the API key is valid, and (c) the terminal
#' backend is reachable.  The /api/status endpoint is not sufficient because
#' it returns 200 ("ST_PROXY_READY") even when the terminal is disconnected.
#'
#' As a side effect, when the handshake succeeds this function ensures that
#' `eikon_port` and `refinitiv_base_url` are set so that downstream
#' functions (CheckTerminalType, RefinitivJsonConnect, etc.) work correctly.
#'
#' @return TRUE if a live, authenticated terminal session is available.
#' @noRd
has_live_api <- function() {
  env_host <- Sys.getenv("REFINITIV_TEST_HOST", "")
  env_port <- Sys.getenv("REFINITIV_TEST_PORT", "")

  if (nzchar(env_host)) {
    options(refinitiv_base_url = paste0("http://", env_host))
  }
  if (nzchar(env_port)) {
    options(eikon_port = as.integer(env_port))
  }

  # Ensure sane defaults — other test files may leave these as NULL or bogus.
  if (is.null(getOption("refinitiv_base_url")) ||
      !nzchar(getOption("refinitiv_base_url", ""))) {
    options(refinitiv_base_url = "http://localhost")
  }

  # Use rd_handshake() to probe for a live terminal.  This runs
  # CheckTerminalType() (port probing) + a real handshake using
  # DEFAULT_WORKSPACE_APP_KEY — no user-specific API key required.
  tryCatch({
    resp <- suppressWarnings(rd_handshake(debug = FALSE, force = TRUE))
    if (is.null(resp) || is.null(resp$access_token)) return(FALSE)

    # Ensure the API key is available for downstream calls.
    # Always set it — earlier tests may leave a bogus key in the vault.
    refinitiv_vault_set("api_key", "DEFAULT_WORKSPACE_APP_KEY")
    # Also set the .EikonApiKey option so that live tests calling
    # RefinitivJsonConnect(getOption(".EikonApiKey")) get a valid key.
    options(.EikonApiKey = "DEFAULT_WORKSPACE_APP_KEY")
    TRUE
  }, error = function(e) FALSE)
}

#' Print the state of all Refinitiv-related options for debugging.
#'
#' Call at the end of each test file to diagnose option pollution between files.
#'
#' @param label A descriptive label (typically the test file name).
#' @noRd
dump_refinitiv_options <- function(label) {
  opts <- list(
    refinitiv_base_url     = getOption("refinitiv_base_url"),
    eikon_port             = getOption("eikon_port"),
    rdp_port               = getOption("rdp_port"),
    eikon_api              = getOption("eikon_api"),
    rdp_api                = getOption("rdp_api"),
    streaming_port         = getOption("streaming_port"),
    .RefinitivAPI          = getOption(".RefinitivAPI"),
    .RefinitivPyModuleName = getOption(".RefinitivPyModuleName")
  )
  vault_keys <- c("api_key", "access_token", "token_expiration", "token_type")
  vault_state <- vapply(vault_keys, function(k) {
    v <- refinitiv_vault_get(k)
    if (is.null(v)) "NULL" else if (is.character(v)) paste0(substr(v, 1, 12), "...") else as.character(v)
  }, character(1))

  msg <- paste0(
    "\n=== OPTIONS STATE [", label, "] ===\n",
    paste0("  ", format(names(opts), width = 24), " = ",
           vapply(opts, function(x) if (is.null(x)) "NULL" else as.character(x), character(1)),
           collapse = "\n"),
    "\n  --- vault ---\n",
    paste0("  ", format(vault_keys, width = 24), " = ", vault_state, collapse = "\n"),
    "\n=== END [", label, "] ===\n"
  )
  cat(msg, file = stderr())
  # Also append to a log file for post-run analysis
  log_path <- Sys.getenv("REFINITIV_OPTIONS_LOG", "")
  if (nzchar(log_path)) {
    cat(msg, file = log_path, append = TRUE)
  }
}

#' Snapshot and auto-restore all Refinitiv-related options and vault state.
#'
#' Call at the top of any test file that modifies options or vault entries.
#' Uses [withr::defer()] so state is restored when the target environment exits.
#'
#' @param env The environment to attach the deferred cleanup to.
#'   Use `teardown_env()` at file level, or `parent.frame()` inside a
#'   `test_that()` block.
#' @noRd
local_refinitiv_state <- function(env = parent.frame()) {
  # Save Refinitiv-specific options
  opt_keys <- c("refinitiv_base_url", "eikon_port", "rdp_port", "eikon_api",
                 "rdp_api", "streaming_port", ".RefinitivAPI",
                 ".RefinitivPyModuleName", ".EikonApiKey")
  saved_opts <- setNames(lapply(opt_keys, getOption), opt_keys)

  # Save vault
  vault_keys <- refinitiv_vault_keys()
  saved_vault <- setNames(lapply(vault_keys, refinitiv_vault_get), vault_keys)

  withr::defer({
    # Restore options
    do.call(options, saved_opts)
    # Restore vault
    refinitiv_vault_clear()
    for (k in names(saved_vault)) {
      refinitiv_vault_set(k, saved_vault[[k]])
    }
  }, envir = env)
}

# Force-assign to .GlobalEnv so that in-process devtools::test() runs
# always pick up the current version, even when a stale copy already
# exists in .GlobalEnv from a prior interactive session.
assign("has_live_api",    has_live_api,    envir = .GlobalEnv)
assign("setup_mock_json_env", setup_mock_json_env, envir = .GlobalEnv)
assign("dump_refinitiv_options", dump_refinitiv_options, envir = .GlobalEnv)
assign("local_refinitiv_state", local_refinitiv_state, envir = .GlobalEnv)
