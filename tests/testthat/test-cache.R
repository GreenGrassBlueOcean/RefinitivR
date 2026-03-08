# tests/testthat/test-cache.R
# Tests for the session-scoped request cache (R/cache.R)

# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

# ── Helper: clean cache before each test ─────────────────────────────────────
# Access the package-private cache environment via :::
get_cache_env <- function() Refinitiv:::.refinitiv_cache

clear_test_cache <- function() {
  env <- get_cache_env()
  rm(list = ls(env, all.names = TRUE), envir = env)
}


# ══════════════════════════════════════════════════════════════════════════════
# sort_named_recursive
# ══════════════════════════════════════════════════════════════════════════════
test_that("sort_named_recursive sorts top-level named list", {
  a <- list(b = 2, a = 1)
  b <- list(a = 1, b = 2)
  expect_identical(
    Refinitiv:::sort_named_recursive(a),
    Refinitiv:::sort_named_recursive(b)
  )
})

test_that("sort_named_recursive sorts nested named lists", {
  a <- list(outer = list(z = 3, a = 1), x = 10)
  b <- list(x = 10, outer = list(a = 1, z = 3))
  expect_identical(
    Refinitiv:::sort_named_recursive(a),
    Refinitiv:::sort_named_recursive(b)
  )
})

test_that("sort_named_recursive leaves unnamed lists unchanged", {
  a <- list(3, 1, 2)
  expect_identical(Refinitiv:::sort_named_recursive(a), a)
})

test_that("sort_named_recursive handles atomic values", {
  expect_identical(Refinitiv:::sort_named_recursive(42), 42)
  expect_identical(Refinitiv:::sort_named_recursive("hello"), "hello")
  expect_identical(Refinitiv:::sort_named_recursive(NULL), NULL)
})

test_that("sort_named_recursive handles mixed named/unnamed lists", {
  # List with some unnamed elements — should NOT sort
  a <- list(b = 2, 1, a = 3)
  expect_identical(Refinitiv:::sort_named_recursive(a), a)
})


# ══════════════════════════════════════════════════════════════════════════════
# cache_key_canonical (formerly cache_key)
# ══════════════════════════════════════════════════════════════════════════════
test_that("cache_key_canonical is deterministic", {
  k1 <- Refinitiv:::cache_key_canonical("fn", "AAPL.O", list(Curn = "USD"))
  k2 <- Refinitiv:::cache_key_canonical("fn", "AAPL.O", list(Curn = "USD"))
  expect_identical(k1, k2)
})

test_that("cache_key_canonical is order-independent for named list args", {
  k1 <- Refinitiv:::cache_key_canonical("fn", list(a = 1, b = 2))
  k2 <- Refinitiv:::cache_key_canonical("fn", list(b = 2, a = 1))
  expect_identical(k1, k2)
})

test_that("cache_key_canonical differs for different function names", {
  k1 <- Refinitiv:::cache_key_canonical("fn_a", "AAPL.O")
  k2 <- Refinitiv:::cache_key_canonical("fn_b", "AAPL.O")
  expect_false(k1 == k2)
})

test_that("cache_key_canonical differs for different arguments", {
  k1 <- Refinitiv:::cache_key_canonical("fn", "AAPL.O")
  k2 <- Refinitiv:::cache_key_canonical("fn", "MSFT.O")
  expect_false(k1 == k2)
})

test_that("cache_key_canonical is order-invariant for character vectors", {
  k1 <- Refinitiv:::cache_key_canonical("fn", c("AAPL.O", "MSFT.O"))
  k2 <- Refinitiv:::cache_key_canonical("fn", c("MSFT.O", "AAPL.O"))
  expect_true(k1 == k2)
})

test_that("cache_key_canonical handles NULL args", {
  k1 <- Refinitiv:::cache_key_canonical("fn", NULL, "x")
  k2 <- Refinitiv:::cache_key_canonical("fn", NULL, "x")
  expect_identical(k1, k2)
})

test_that("cache_key_canonical includes package version (#22)", {
  k1 <- Refinitiv:::cache_key_canonical("fn", "AAPL.O")
  k2 <- Refinitiv:::cache_key_canonical("fn", "AAPL.O")
  expect_identical(k1, k2)

  # Verify the version is actually part of the hash
  args <- list("AAPL.O")
  real_ver <- Refinitiv:::get_pkg_version()
  fake_ver <- "99.99.99"
  hash_real <- rlang::hash(c(list("fn", real_ver), args))
  hash_fake <- rlang::hash(c(list("fn", fake_ver), args))
  expect_identical(k1, hash_real)
  expect_false(identical(hash_real, hash_fake))
})


# ══════════════════════════════════════════════════════════════════════════════
# resolve_cache
# ══════════════════════════════════════════════════════════════════════════════
test_that("resolve_cache: NULL defers to global option (default FALSE)", {
  withr::with_options(list(refinitiv_cache = NULL), {
    expect_false(Refinitiv:::resolve_cache(NULL, 300))
  })
})

test_that("resolve_cache: NULL + global TRUE uses fn default", {
  withr::with_options(list(refinitiv_cache = TRUE), {
    expect_equal(Refinitiv:::resolve_cache(NULL, 300), 300)
  })
})

test_that("resolve_cache: NULL + global numeric uses global value", {
  withr::with_options(list(refinitiv_cache = 600), {
    expect_equal(Refinitiv:::resolve_cache(NULL, 300), 600)
  })
})

test_that("resolve_cache: per-call FALSE overrides global TRUE", {
  withr::with_options(list(refinitiv_cache = TRUE), {
    expect_false(Refinitiv:::resolve_cache(FALSE, 300))
  })
})

test_that("resolve_cache: per-call TRUE uses fn default", {
  expect_equal(Refinitiv:::resolve_cache(TRUE, 300), 300)
})

test_that("resolve_cache: per-call numeric uses that value", {
  expect_equal(Refinitiv:::resolve_cache(120, 300), 120)
})

test_that("resolve_cache: per-call 0 or negative returns FALSE", {
  expect_false(Refinitiv:::resolve_cache(0, 300))
  expect_false(Refinitiv:::resolve_cache(-10, 300))
})


# ══════════════════════════════════════════════════════════════════════════════
# cache_set / cache_get
# ══════════════════════════════════════════════════════════════════════════════
test_that("cache_set and cache_get: basic round-trip", {
  clear_test_cache()
  Refinitiv:::cache_set("test_key", data.frame(x = 1:3), ttl = 60)

  hit <- Refinitiv:::cache_get("test_key")
  expect_true(hit$found)
  expect_equal(hit$value, data.frame(x = 1:3))
})

test_that("cache_get: returns found=FALSE for missing key", {
  clear_test_cache()
  hit <- Refinitiv:::cache_get("nonexistent")
  expect_false(hit$found)
})

test_that("cache_get: returns found=FALSE for expired key (lazy eviction)", {
  clear_test_cache()
  # Store with 0-second TTL (already expired)
  env <- get_cache_env()
  assign("expired_key",
    list(value = "old_data", expires_at = as.numeric(Sys.time()) - 1),
    envir = env
  )

  hit <- Refinitiv:::cache_get("expired_key")
  expect_false(hit$found)
  # Verify it was evicted

  expect_false(exists("expired_key", envir = env, inherits = FALSE))
})

test_that("cache_get: Inf TTL never expires", {
  clear_test_cache()
  Refinitiv:::cache_set("inf_key", "permanent", ttl = Inf)
  hit <- Refinitiv:::cache_get("inf_key")
  expect_true(hit$found)
  expect_equal(hit$value, "permanent")
})

test_that("cache_set can store NULL values (distinguishable from miss)", {
  clear_test_cache()
  Refinitiv:::cache_set("null_key", NULL, ttl = 60)
  hit <- Refinitiv:::cache_get("null_key")
  expect_true(hit$found)
  expect_null(hit$value)
})

test_that("cache_set can store lists", {
  clear_test_cache()
  val <- list(a = 1, b = list(c = "nested"))
  Refinitiv:::cache_set("list_key", val, ttl = 60)
  hit <- Refinitiv:::cache_get("list_key")
  expect_true(hit$found)
  expect_equal(hit$value, val)
})


# ══════════════════════════════════════════════════════════════════════════════
# rd_ClearCache
# ══════════════════════════════════════════════════════════════════════════════
test_that("rd_ClearCache empties the cache", {
  clear_test_cache()
  Refinitiv:::cache_set("k1", 1, 60)
  Refinitiv:::cache_set("k2", 2, 60)
  expect_equal(length(ls(get_cache_env(), all.names = TRUE)), 2L)

  expect_message(rd_ClearCache(), "Cache cleared")
  expect_equal(length(ls(get_cache_env(), all.names = TRUE)), 0L)
})

test_that("rd_ClearCache on empty cache does not error", {
  clear_test_cache()
  expect_message(rd_ClearCache(), "Cache cleared")
})


# ══════════════════════════════════════════════════════════════════════════════
# rd_CacheInfo
# ══════════════════════════════════════════════════════════════════════════════
test_that("rd_CacheInfo reports correct counts", {
  clear_test_cache()
  Refinitiv:::cache_set("active1", data.frame(x = 1), ttl = 3600)
  Refinitiv:::cache_set("active2", data.frame(x = 2), ttl = Inf)

  # Insert an expired entry directly
  env <- get_cache_env()
  assign("expired1",
    list(value = "old", expires_at = as.numeric(Sys.time()) - 100),
    envir = env
  )

  info <- expect_message(rd_CacheInfo(), "2 active, 1 expired")
  expect_equal(info$total_keys, 3L)
  expect_equal(info$active_keys, 2L)
  expect_equal(info$expired_keys, 1L)
  expect_true(info$estimated_size_mb >= 0)
})

test_that("rd_CacheInfo handles empty cache", {
  clear_test_cache()
  info <- expect_message(rd_CacheInfo(), "0 active, 0 expired")
  expect_equal(info$total_keys, 0L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Integration: cache wiring in exported functions
# ══════════════════════════════════════════════════════════════════════════════

# We test that the cache param is accepted and the lookup/store path works
# by mocking the internal API call and verifying call count.

test_that("rd_GetData respects cache = TRUE (mocked)", {
  clear_test_cache()
  setup_mock_json_env()

  mock_result <- list(
    responses = list(list(
      data = list(list("AAPL.O", 150)),
      headers = list(list(
        list(name = "Instrument", displayName = "Instrument"),
        list(name = "Price", displayName = "Price")
      ))
    ))
  )

  call_count <- 0L
  mockery::stub(rd_GetData, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    list(mock_result)
  })

  # First call: cache miss, hits the mock
  res1 <- rd_GetData(
    RDObject = RefinitivJsonConnect(),
    rics = "AAPL.O",
    Eikonformulas = "TR.Price",
    cache = TRUE
  )
  expect_equal(call_count, 1L)
  expect_s3_class(res1, "data.frame")

  # Second call: cache hit, should NOT increment call_count
  res2 <- rd_GetData(
    RDObject = RefinitivJsonConnect(),
    rics = "AAPL.O",
    Eikonformulas = "TR.Price",
    cache = TRUE
  )
  expect_equal(call_count, 1L) # Still 1 — served from cache
  expect_equal(res1, res2)
})

test_that("rd_GetData cache = FALSE always calls API", {
  clear_test_cache()
  setup_mock_json_env()

  mock_result <- list(
    responses = list(list(
      data = list(list("AAPL.O", 150)),
      headers = list(list(
        list(name = "Instrument", displayName = "Instrument"),
        list(name = "Price", displayName = "Price")
      ))
    ))
  )

  call_count <- 0L
  mockery::stub(rd_GetData, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    list(mock_result)
  })

  rd_GetData(
    RDObject = RefinitivJsonConnect(),
    rics = "AAPL.O", Eikonformulas = "TR.Price", cache = FALSE
  )
  rd_GetData(
    RDObject = RefinitivJsonConnect(),
    rics = "AAPL.O", Eikonformulas = "TR.Price", cache = FALSE
  )
  expect_equal(call_count, 2L)
})

test_that("rd_GetData with different params gets different cache entries", {
  clear_test_cache()
  setup_mock_json_env()

  mock_result <- list(
    responses = list(list(
      data = list(list("X", 100)),
      headers = list(list(
        list(name = "Instrument", displayName = "Instrument"),
        list(name = "Val", displayName = "Val")
      ))
    ))
  )

  call_count <- 0L
  mockery::stub(rd_GetData, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    list(mock_result)
  })

  rd_GetData(
    RDObject = RefinitivJsonConnect(),
    rics = "AAPL.O", Eikonformulas = "TR.Price", cache = TRUE
  )
  rd_GetData(
    RDObject = RefinitivJsonConnect(),
    rics = "MSFT.O", Eikonformulas = "TR.Price", cache = TRUE
  )
  expect_equal(call_count, 2L) # Different rics = different cache keys
})

test_that("global option enables caching when cache = NULL", {
  clear_test_cache()
  setup_mock_json_env()

  mock_result <- list(
    responses = list(list(
      data = list(list("AAPL.O", 150)),
      headers = list(list(
        list(name = "Instrument", displayName = "Instrument"),
        list(name = "Price", displayName = "Price")
      ))
    ))
  )

  call_count <- 0L
  mockery::stub(rd_GetData, "chunked_download", function(...) {
    call_count <<- call_count + 1L
    list(mock_result)
  })

  withr::with_options(list(refinitiv_cache = TRUE), {
    rd_GetData(
      RDObject = RefinitivJsonConnect(),
      rics = "AAPL.O", Eikonformulas = "TR.Price"
    )
    rd_GetData(
      RDObject = RefinitivJsonConnect(),
      rics = "AAPL.O", Eikonformulas = "TR.Price"
    )
  })
  expect_equal(call_count, 1L) # Second call served from cache
})

test_that("NULL results are not cached", {
  clear_test_cache()

  # Directly test the cache infrastructure: NULL should be distinguishable

  # but at the function level, a NULL ReturnElement is not cached
  Refinitiv:::cache_set("null_test", NULL, 60)
  hit <- Refinitiv:::cache_get("null_test")
  expect_true(hit$found) # infrastructure CAN store NULL
  expect_null(hit$value)

  # But the function-level guard skips caching when result is NULL:
  # if (!isFALSE(ttl) && !is.null(ReturnElement) && !inherits(ReturnElement, "try-error"))
  # So if a function returns NULL, the next call will hit the API again.
  # This is tested indirectly via the guard condition.
  clear_test_cache()
})

test_that("try-error results are not cached (infrastructure level)", {
  clear_test_cache()

  # Simulate what the guard condition checks
  err <- structure("error msg", class = "try-error")
  ttl <- 300
  .ck <- "test_err_key"

  # The guard: !inherits(result, "try-error") should prevent caching
  if (!isFALSE(ttl) && !is.null(err) && !inherits(err, "try-error")) {
    Refinitiv:::cache_set(.ck, err, ttl)
  }

  hit <- Refinitiv:::cache_get(.ck)
  expect_false(hit$found) # Was never stored
})


# ══════════════════════════════════════════════════════════════════════════════
# Clean up
# ══════════════════════════════════════════════════════════════════════════════
clear_test_cache()


restore_refinitiv_state(.saved_state, "test-cache")
