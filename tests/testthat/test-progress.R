# tests/testthat/test-progress.R
# Tests for progress_msg() and chunked_download() progress reporting

# ── progress_msg() ──────────────────────────────────────────────────────────

test_that("progress_msg emits message when mode is TRUE", {
  withr::local_options(refinitiv_progress = TRUE)
  expect_message(progress_msg("hello"), "\\[RefinitivR\\] hello")
})

test_that("progress_msg is silent when mode is FALSE", {
  withr::local_options(refinitiv_progress = FALSE)
  expect_silent(progress_msg("hello"))
})

test_that("progress_msg emits message when mode is 'verbose'", {
  withr::local_options(refinitiv_progress = "verbose")
  expect_message(progress_msg("hello"), "\\[RefinitivR\\] hello")
})

test_that("progress_msg verbose_only = TRUE only fires in verbose mode", {
  withr::local_options(refinitiv_progress = TRUE)
  expect_silent(progress_msg("detail", verbose_only = TRUE))

  withr::local_options(refinitiv_progress = "verbose")
  expect_message(progress_msg("detail", verbose_only = TRUE), "\\[RefinitivR\\] detail")
})

test_that("progress_msg force = TRUE emits in TRUE mode", {
  withr::local_options(refinitiv_progress = TRUE)
  expect_message(progress_msg("forced", force = TRUE), "\\[RefinitivR\\] forced")
})

test_that("progress_msg force = TRUE is still silent when mode is FALSE", {
  withr::local_options(refinitiv_progress = FALSE)
  expect_silent(progress_msg("forced", force = TRUE))
})

test_that("progress_msg defaults to TRUE when option is NULL", {
  withr::local_options(refinitiv_progress = NULL)
  expect_message(progress_msg("default"), "\\[RefinitivR\\] default")
})

# ── chunked_download() progress: one-chunk rule ────────────────────────────

test_that("chunked_download is silent for single chunk in TRUE mode", {
  withr::local_options(refinitiv_progress = TRUE)
  expect_silent(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) j,
      sleep = 0,
      backoff = 0,
      verbose = TRUE
    )
  )
})

test_that("chunked_download emits progress for single chunk in verbose mode", {
  withr::local_options(refinitiv_progress = "verbose")
  expect_message(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) j,
      sleep = 0,
      backoff = 0,
      verbose = "verbose"
    ),
    "Download Status"
  )
})

test_that("chunked_download is silent for single chunk when verbose = FALSE", {
  expect_silent(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) j,
      sleep = 0,
      backoff = 0,
      verbose = FALSE
    )
  )
})

# ── chunked_download() progress: multi-chunk ───────────────────────────────

test_that("chunked_download emits per-chunk and completion messages for multi-chunk", {
  withr::local_options(refinitiv_progress = TRUE)
  msgs <- character()
  withCallingHandlers(
    chunked_download(
      n_chunks = 2,
      fetch_fn = function(j) j * 10,
      sleep = 0,
      backoff = 0,
      verbose = TRUE,
      caller_label = "test_fn"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  # Should see chunk 1, chunk 2, and completion
  expect_true(any(grepl("Chunk 1/2 done", msgs)))
  expect_true(any(grepl("Chunk 2/2 done", msgs)))
  expect_true(any(grepl("Download complete: 2/2", msgs)))
  # caller_label should appear

  expect_true(any(grepl("test_fn", msgs)))
})

test_that("chunked_download includes chunk_sizes in messages", {
  withr::local_options(refinitiv_progress = TRUE)
  msgs <- character()
  withCallingHandlers(
    chunked_download(
      n_chunks = 2,
      fetch_fn = function(j) j,
      sleep = 0,
      backoff = 0,
      verbose = TRUE,
      chunk_sizes = c(300L, 250L)
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("300 items", msgs)))
  expect_true(any(grepl("250 items", msgs)))
})

test_that("chunked_download emits retry failure message", {
  withr::local_options(refinitiv_progress = TRUE)
  msgs <- character()
  withCallingHandlers(
    tryCatch(
      chunked_download(
        n_chunks = 2,
        fetch_fn = function(j) if (j == 2) NA else j,
        max_retries = 1L,
        sleep = 0,
        backoff = 0,
        verbose = TRUE,
        caller_label = "retry_test"
      ),
      error = function(e) NULL
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("failed, will retry", msgs)))
  expect_true(any(grepl("Download incomplete", msgs)))
})

test_that("chunked_download emits no progress when verbose = FALSE", {
  expect_silent(
    chunked_download(
      n_chunks = 3,
      fetch_fn = function(j) j,
      sleep = 0,
      backoff = 0,
      verbose = FALSE
    )
  )
})

test_that("chunked_download verbose mode includes coordinator dump", {
  msgs <- character()
  withCallingHandlers(
    chunked_download(
      n_chunks = 2,
      fetch_fn = function(j) j,
      sleep = 0,
      backoff = 0,
      verbose = "verbose"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("Download Status", msgs)))
})

# ── .onLoad REFINITIV_PROGRESS env var parsing ─────────────────────────────

test_that(".onLoad parses REFINITIV_PROGRESS=TRUE", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = NULL)
  withr::local_envvar(REFINITIV_PROGRESS = "TRUE")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_true(isTRUE(getOption("refinitiv_progress")))
})

test_that(".onLoad parses REFINITIV_PROGRESS=FALSE", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = NULL)
  withr::local_envvar(REFINITIV_PROGRESS = "FALSE")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_false(getOption("refinitiv_progress"))
})

test_that(".onLoad parses REFINITIV_PROGRESS=VERBOSE", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = NULL)
  withr::local_envvar(REFINITIV_PROGRESS = "VERBOSE")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_identical(getOption("refinitiv_progress"), "verbose")
})

test_that(".onLoad does not set refinitiv_progress when env var is unset", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = NULL)
  withr::local_envvar(REFINITIV_PROGRESS = NA)
  Refinitiv:::.onLoad(NULL, NULL)
  expect_null(getOption("refinitiv_progress"))
})

test_that(".onLoad does not overwrite existing refinitiv_progress option", {
  local_refinitiv_state()
  withr::local_options(refinitiv_progress = FALSE)
  withr::local_envvar(REFINITIV_PROGRESS = "TRUE")
  Refinitiv:::.onLoad(NULL, NULL)
  expect_false(getOption("refinitiv_progress"))
})

dump_refinitiv_options("test-progress")
