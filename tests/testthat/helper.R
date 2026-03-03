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
setup_mock_json_env <- function(api_key = "dummy_key", port = 9000L,
                                .env = parent.frame()) {
  # Save and auto-restore vault + options in the caller's scope
  saved_api_key <- refinitiv_vault_get("api_key")
  saved_opts <- options(
    eikon_port = port,
    refinitiv_base_url = "http://lh",
    .RefinitivPyModuleName = "JSON",
    .RefinitivAPI = "JSON"
  )
  withr::defer(
    {
      options(saved_opts)
      if (is.null(saved_api_key)) {
        refinitiv_vault_clear(keys = "api_key")
      } else {
        refinitiv_vault_set("api_key", saved_api_key)
      }
    },
    envir = .env
  )

  refinitiv_vault_set("api_key", api_key)
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
  tryCatch(
    {
      resp <- suppressWarnings(rd_handshake(debug = FALSE, force = TRUE))
      if (is.null(resp) || is.null(resp$access_token)) {
        return(FALSE)
      }

      # Ensure the API key is available for downstream calls.
      # Always set it — earlier tests may leave a bogus key in the vault.
      refinitiv_vault_set("api_key", "DEFAULT_WORKSPACE_APP_KEY")
      # Also set the .EikonApiKey option so that live tests calling
      # RefinitivJsonConnect(getOption(".EikonApiKey")) get a valid key.
      options(.EikonApiKey = "DEFAULT_WORKSPACE_APP_KEY")
      TRUE
    },
    error = function(e) FALSE
  )
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
      collapse = "\n"
    ),
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

#' Snapshot all Refinitiv-related options and vault state.
#'
#' Call at the TOP of any test file that modifies options or vault entries.
#' Returns the captured state, which must be passed to
#' [restore_refinitiv_state()] at the BOTTOM of the same file.
#'
#' @return A list with elements `opts` and `vault`.
#' @noRd
save_refinitiv_state <- function() {
  opt_keys <- c(
    "refinitiv_base_url", "eikon_port", "rdp_port", "eikon_api",
    "rdp_api", "streaming_port", ".RefinitivAPI",
    ".RefinitivPyModuleName", ".EikonApiKey"
  )
  saved_opts <- setNames(lapply(opt_keys, getOption), opt_keys)
  vault_keys <- refinitiv_vault_keys()
  saved_vault <- setNames(lapply(vault_keys, refinitiv_vault_get), vault_keys)
  list(opts = saved_opts, vault = saved_vault)
}

#' Restore Refinitiv options and vault state, then dump.
#'
#' Call at the BOTTOM of any test file that called [save_refinitiv_state()]
#' at the top.  Restores both options and vault to the captured snapshot,
#' then prints a diagnostic dump.  The dump now serves as a leak detector:
#' vault entries should be NULL when every test uses [local_refinitiv_state()].
#' Non-NULL vault entries indicate a test that forgot cleanup.
#'
#' @param saved The list returned by [save_refinitiv_state()].
#' @param label A descriptive label for the dump (typically the test file name).
#' @noRd
restore_refinitiv_state <- function(saved, label) {
  # Dump before restoring — shows what the tests left behind.
  # With local_refinitiv_state() in each test, vault should be clean here.
  # Non-NULL vault entries = a test forgot local_refinitiv_state().
  dump_refinitiv_options(label)
  # Restore options
  do.call(options, saved$opts)
  # Restore vault
  refinitiv_vault_clear()
  for (k in names(saved$vault)) {
    refinitiv_vault_set(k, saved$vault[[k]])
  }
}

#' Snapshot and auto-restore all Refinitiv-related options and vault state.
#'
#' Uses [withr::defer()] — suitable for use inside `test_that()` blocks or
#' `local()` scopes.  NOT suitable at file scope with `teardown_env()` when
#' running via `devtools::test()` (teardowns are batched at the end of the
#' test run, not between files).
#'
#' For file-level state isolation, use the [save_refinitiv_state()] /
#' [restore_refinitiv_state()] pair instead.
#'
#' @param env The environment to attach the deferred cleanup to.
#' @noRd
local_refinitiv_state <- function(env = parent.frame()) {
  saved <- save_refinitiv_state()
  withr::defer(
    {
      do.call(options, saved$opts)
      refinitiv_vault_clear()
      for (k in names(saved$vault)) {
        refinitiv_vault_set(k, saved$vault[[k]])
      }
    },
    envir = env
  )
}

#' Capture all messages emitted by an expression into a character vector.
#'
#' Useful for asserting that specific debug messages appear without letting
#' them pollute test output.  All messages are muffled so testthat's
#' `expect_message()` will not see them; compare the returned vector directly.
#'
#' @param expr An R expression to evaluate.
#' @return A character vector of message strings (one per `message()` call).
#' @noRd
capture_messages <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr,
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  msgs
}

# Force-assign to .GlobalEnv so that in-process devtools::test() runs
# always pick up the current version, even when a stale copy already
# exists in .GlobalEnv from a prior interactive session.
assign("has_live_api", has_live_api, envir = .GlobalEnv)
assign("setup_mock_json_env", setup_mock_json_env, envir = .GlobalEnv)
assign("dump_refinitiv_options", dump_refinitiv_options, envir = .GlobalEnv)
assign("local_refinitiv_state", local_refinitiv_state, envir = .GlobalEnv)
assign("save_refinitiv_state", save_refinitiv_state, envir = .GlobalEnv)
assign("restore_refinitiv_state", restore_refinitiv_state, envir = .GlobalEnv)
assign("capture_messages", capture_messages, envir = .GlobalEnv)
