test_that("chunked_download succeeds when all chunks succeed on first try", {
  results <- chunked_download(
    n_chunks = 3,
    fetch_fn = function(j) j * 10,
    sleep    = 0,
    backoff  = 0
  )
  expect_equal(results, list(10, 20, 30))
})

test_that("chunked_download retries failed chunks", {
  # Chunk 2 fails twice then succeeds
  call_counts <- integer(3)
  results <- chunked_download(
    n_chunks = 3,
    fetch_fn = function(j) {
      call_counts[j] <<- call_counts[j] + 1L
      if (j == 2 && call_counts[j] <= 2) {
        return(NA)
      }
      j * 10
    },
    sleep = 0,
    backoff = 0
  )
  expect_equal(results, list(10, 20, 30))
  expect_equal(call_counts[1], 1L) # chunk 1: called once
  expect_gte(call_counts[2], 3L) # chunk 2: failed twice, succeeded on third
  expect_equal(call_counts[3], 1L) # chunk 3: called once
})

test_that("chunked_download stops after max_retries with on_failure='stop'", {
  expect_error(
    chunked_download(
      n_chunks = 2,
      fetch_fn = function(j) if (j == 2) NA else j,
      max_retries = 2L,
      sleep = 0,
      backoff = 0,
      on_failure = "stop",
      fail_message = "test failure"
    ),
    "test failure"
  )
})

test_that("chunked_download warns after max_retries with on_failure='warning'", {
  expect_warning(
    result <- chunked_download(
      n_chunks = 2,
      fetch_fn = function(j) if (j == 2) NA else j,
      max_retries = 1L,
      sleep = 0,
      backoff = 0,
      on_failure = "warning",
      fail_message = "partial failure"
    ),
    "partial failure"
  )
  # Chunk 1 should still have its result
  expect_equal(result[[1]], 1)
  expect_identical(result[[2]], NA)
})

test_that("chunked_download respects custom is_success predicate", {
  # By default, returning list() would be "success" since it's not NA.
  # With a custom predicate, we can reject it.
  call_count <- 0L
  expect_error(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) {
        call_count <<- call_count + 1L
        list()
      },
      is_success = function(x) !identical(x, list()),
      max_retries = 1L,
      sleep = 0,
      backoff = 0,
      fail_message = "empty list rejected"
    ),
    "empty list rejected"
  )
  # Should have been called at least twice (initial + 1 retry round)
  expect_gte(call_count, 2L)
})

test_that("chunked_download emits verbose messages when verbose='verbose'", {
  expect_message(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) j,
      sleep    = 0,
      backoff  = 0,
      verbose  = "verbose"
    ),
    "Download Status"
  )
})

test_that("chunked_download does not emit messages when verbose=FALSE", {
  expect_silent(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) j,
      sleep    = 0,
      backoff  = 0,
      verbose  = FALSE
    )
  )
})

test_that("chunked_download default max_retries is 3", {
  # A permanently-failing chunk should be attempted 4 times total (1 initial + 3 retries)
  call_count <- 0L
  expect_error(
    chunked_download(
      n_chunks = 1,
      fetch_fn = function(j) {
        call_count <<- call_count + 1L
        NA
      },
      sleep = 0,
      backoff = 0
    )
  )
  expect_equal(call_count, 4L)
})

test_that("chunked_download applies backoff between retry rounds", {
  # Track Sys.sleep calls to verify backoff is applied
  sleep_times <- numeric(0)
  mockery::stub(chunked_download, "Sys.sleep", function(t) {
    sleep_times <<- c(sleep_times, t)
  })

  expect_error(
    chunked_download(
      n_chunks    = 1,
      fetch_fn    = function(j) NA,
      max_retries = 2L,
      sleep       = 0,
      backoff     = 1.0
    )
  )

  # sleep=0 means no per-chunk sleeps; all recorded sleeps are from backoff
  # With max_retries=2, we get 2 backoff sleeps (after round 1 and round 2)
  # Round 1: backoff * 2^0 * jitter = ~1.0 (between 0.5 and 1.5)
  # Round 2: backoff * 2^1 * jitter = ~2.0 (between 1.0 and 3.0)
  backoff_sleeps <- sleep_times[sleep_times > 0]
  expect_length(backoff_sleeps, 2L)
  expect_true(backoff_sleeps[1] >= 0.5 && backoff_sleeps[1] <= 1.5)
  expect_true(backoff_sleeps[2] >= 1.0 && backoff_sleeps[2] <= 3.0)
})

test_that("chunked_download backoff = 0 disables inter-round waiting", {
  sleep_times <- numeric(0)
  mockery::stub(chunked_download, "Sys.sleep", function(t) {
    sleep_times <<- c(sleep_times, t)
  })

  expect_error(
    chunked_download(
      n_chunks    = 1,
      fetch_fn    = function(j) NA,
      max_retries = 2L,
      sleep       = 0,
      backoff     = 0
    )
  )

  # All sleep calls should be 0 (from per-chunk sleep=0); no backoff sleeps
  expect_true(all(sleep_times == 0))
})

test_that("chunked_download does not backoff after all chunks succeed", {
  sleep_times <- numeric(0)
  mockery::stub(chunked_download, "Sys.sleep", function(t) {
    sleep_times <<- c(sleep_times, t)
  })

  results <- chunked_download(
    n_chunks = 2,
    fetch_fn = function(j) j * 10,
    sleep    = 0,
    backoff  = 1.0
  )

  expect_equal(results, list(10, 20))
  # Only per-chunk sleeps (0s each), no backoff sleeps
  expect_true(all(sleep_times == 0))
})

test_that("chunked_download backoff only fires for failed rounds", {
  sleep_times <- numeric(0)
  mockery::stub(chunked_download, "Sys.sleep", function(t) {
    sleep_times <<- c(sleep_times, t)
  })

  # Chunk 1 always succeeds, chunk 2 fails once then succeeds
  call_counts <- integer(2)
  results <- chunked_download(
    n_chunks = 2,
    fetch_fn = function(j) {
      call_counts[j] <<- call_counts[j] + 1L
      if (j == 2 && call_counts[j] == 1) {
        return(NA)
      }
      j * 10
    },
    sleep = 0,
    backoff = 1.0
  )

  expect_equal(results, list(10, 20))
  # Exactly 1 backoff sleep (after round 1 when chunk 2 failed)
  backoff_sleeps <- sleep_times[sleep_times > 0]
  expect_length(backoff_sleeps, 1L)
  expect_true(backoff_sleeps[1] >= 0.5 && backoff_sleeps[1] <= 1.5)
})

# ══════════════════════════════════════════════════════════════════════════════
# Per-chunk caching
# ══════════════════════════════════════════════════════════════════════════════

test_that("chunked_download uses per-chunk cache on second call", {
  env <- Refinitiv:::.refinitiv_cache
  rm(list = ls(env, all.names = TRUE), envir = env)

  call_count <- 0L
  fetch_fn <- function(j) {
    call_count <<- call_count + 1L
    paste0("result_", j)
  }

  keys <- c("chunk_test_1", "chunk_test_2", "chunk_test_3")
  key_fn <- function(j) keys[j]

  # First call: all 3 chunks downloaded
  res1 <- Refinitiv:::chunked_download(
    n_chunks = 3L,
    fetch_fn = fetch_fn,
    chunk_cache_key_fn = key_fn,
    cache_ttl = 60,
    sleep = 0,
    verbose = FALSE
  )
  expect_equal(call_count, 3L)
  expect_equal(res1, list("result_1", "result_2", "result_3"))

  # Second call: all from cache, fetch_fn not called
  call_count <- 0L
  res2 <- Refinitiv:::chunked_download(
    n_chunks = 3L,
    fetch_fn = fetch_fn,
    chunk_cache_key_fn = key_fn,
    cache_ttl = 60,
    sleep = 0,
    verbose = FALSE
  )
  expect_equal(call_count, 0L)
  expect_equal(res2, list("result_1", "result_2", "result_3"))

  rm(list = ls(env, all.names = TRUE), envir = env)
})

test_that("chunked_download partial cache hit downloads only missing chunks", {
  env <- Refinitiv:::.refinitiv_cache
  rm(list = ls(env, all.names = TRUE), envir = env)

  # Pre-cache chunks 1 and 3
  Refinitiv:::cache_set("partial_1", "cached_1", 60)
  Refinitiv:::cache_set("partial_3", "cached_3", 60)

  downloaded_indices <- integer(0)
  fetch_fn <- function(j) {
    downloaded_indices <<- c(downloaded_indices, j)
    paste0("fresh_", j)
  }

  key_fn <- function(j) paste0("partial_", j)

  res <- Refinitiv:::chunked_download(
    n_chunks = 3L,
    fetch_fn = fetch_fn,
    chunk_cache_key_fn = key_fn,
    cache_ttl = 60,
    sleep = 0,
    verbose = FALSE
  )

  # Only chunk 2 should have been downloaded
  expect_equal(downloaded_indices, 2L)
  expect_equal(res, list("cached_1", "fresh_2", "cached_3"))

  rm(list = ls(env, all.names = TRUE), envir = env)
})

test_that("chunked_download without cache params works unchanged", {
  call_count <- 0L
  fetch_fn <- function(j) {
    call_count <<- call_count + 1L
    j * 10
  }

  res <- Refinitiv:::chunked_download(
    n_chunks = 2L,
    fetch_fn = fetch_fn,
    sleep = 0,
    verbose = FALSE
  )
  expect_equal(call_count, 2L)
  expect_equal(res, list(10, 20))
})

test_that("chunked_download cache_ttl = FALSE disables chunk caching", {
  env <- Refinitiv:::.refinitiv_cache
  rm(list = ls(env, all.names = TRUE), envir = env)

  fetch_fn <- function(j) paste0("val_", j)
  key_fn <- function(j) paste0("nope_", j)

  Refinitiv:::chunked_download(
    n_chunks = 2L,
    fetch_fn = fetch_fn,
    chunk_cache_key_fn = key_fn,
    cache_ttl = FALSE,
    sleep = 0,
    verbose = FALSE
  )

  # Nothing should be cached
  expect_false(exists("nope_1", envir = env, inherits = FALSE))
  expect_false(exists("nope_2", envir = env, inherits = FALSE))
})

test_that("chunked_download emits [cached] message for cache hits", {
  env <- Refinitiv:::.refinitiv_cache
  rm(list = ls(env, all.names = TRUE), envir = env)

  # Pre-cache chunk 1
  Refinitiv:::cache_set("verbose_cached_1", "cached_val", 60)

  fetch_fn <- function(j) paste0("fresh_", j)
  key_fn <- function(j) paste0("verbose_cached_", j)

  expect_message(
    Refinitiv:::chunked_download(
      n_chunks = 2L,
      fetch_fn = fetch_fn,
      chunk_cache_key_fn = key_fn,
      cache_ttl = 60,
      sleep = 0,
      verbose = TRUE
    ),
    "\\[cached\\]"
  )

  rm(list = ls(env, all.names = TRUE), envir = env)
})

test_that("chunked_download reports 'from cache' in completion message", {
  env <- Refinitiv:::.refinitiv_cache
  rm(list = ls(env, all.names = TRUE), envir = env)

  # Pre-cache both chunks
  Refinitiv:::cache_set("fromcache_1", "val_1", 60)
  Refinitiv:::cache_set("fromcache_2", "val_2", 60)

  key_fn <- function(j) paste0("fromcache_", j)

  expect_message(
    Refinitiv:::chunked_download(
      n_chunks = 2L,
      fetch_fn = function(j) stop("should not be called"),
      chunk_cache_key_fn = key_fn,
      cache_ttl = 60,
      sleep = 0,
      verbose = TRUE
    ),
    "from cache"
  )

  rm(list = ls(env, all.names = TRUE), envir = env)
})

dump_refinitiv_options("test-chunked_download")
