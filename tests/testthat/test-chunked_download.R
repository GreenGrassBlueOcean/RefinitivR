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

dump_refinitiv_options("test-chunked_download")
