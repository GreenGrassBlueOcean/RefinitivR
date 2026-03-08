# ── Cache key canonicalization tests ──────────────────────────────────────────

test_that("cache_key_canonical is order-invariant for character vectors", {
  k1 <- Refinitiv:::cache_key_canonical("fn", c("AAPL.O", "MSFT.O", "GOOG.O"))
  k2 <- Refinitiv:::cache_key_canonical("fn", c("MSFT.O", "AAPL.O", "GOOG.O"))
  k3 <- Refinitiv:::cache_key_canonical("fn", c("GOOG.O", "MSFT.O", "AAPL.O"))
  expect_equal(k1, k2)
  expect_equal(k1, k3)
})

test_that("cache_key_canonical is order-invariant for large RIC vectors", {
  rics_a <- paste0("RIC", sprintf("%05d", 1:20000))
  rics_b <- rev(rics_a)
  rics_c <- sample(rics_a)
  k_a <- Refinitiv:::cache_key_canonical("EikonGetData", rics_a, "TR.CommonName")
  k_b <- Refinitiv:::cache_key_canonical("EikonGetData", rics_b, "TR.CommonName")
  k_c <- Refinitiv:::cache_key_canonical("EikonGetData", rics_c, "TR.CommonName")
  expect_equal(k_a, k_b)
  expect_equal(k_a, k_c)
})

test_that("cache_key_canonical preserves duplicates (different from unique set)", {
  k_dup <- Refinitiv:::cache_key_canonical("fn", c("A", "A", "B"))
  k_uniq <- Refinitiv:::cache_key_canonical("fn", c("A", "B"))
  expect_false(k_dup == k_uniq)
})

test_that("cache_key_canonical does NOT sort single-element character vectors", {
  k1 <- Refinitiv:::cache_key_canonical("fn", "AAPL.O")
  k2 <- Refinitiv:::cache_key_canonical("fn", "MSFT.O")
  expect_false(k1 == k2)
})

test_that("cache_key_canonical still differentiates by function name", {
  k1 <- Refinitiv:::cache_key_canonical("EikonGetData", c("A", "B"))
  k2 <- Refinitiv:::cache_key_canonical("rd_GetData", c("A", "B"))
  expect_false(k1 == k2)
})

test_that("cache_key_canonical still sorts named lists recursively", {
  k1 <- Refinitiv:::cache_key_canonical("fn", list(a = 1, b = 2))
  k2 <- Refinitiv:::cache_key_canonical("fn", list(b = 2, a = 1))
  expect_equal(k1, k2)
})

test_that("cache_key_canonical handles numeric vectors without sorting", {
  k1 <- Refinitiv:::cache_key_canonical("fn", c(1, 2, 3))
  k2 <- Refinitiv:::cache_key_canonical("fn", c(3, 2, 1))
  expect_false(k1 == k2)
})


# ── Fast key tests ────────────────────────────────────────────────────────────

test_that("cache_key_fast is deterministic", {
  k1 <- Refinitiv:::cache_key_fast("fn", "AAPL.O", "TR.Close")
  k2 <- Refinitiv:::cache_key_fast("fn", "AAPL.O", "TR.Close")
  expect_identical(k1, k2)
})

test_that("cache_key_fast is order-sensitive (unlike canonical)", {
  k1 <- Refinitiv:::cache_key_fast("fn", c("A", "B"))
  k2 <- Refinitiv:::cache_key_fast("fn", c("B", "A"))
  expect_false(k1 == k2)
})

test_that("cache_key_fast uses cached package version", {
  # Should not error — get_pkg_version() must work
  k <- Refinitiv:::cache_key_fast("fn", "x")
  expect_type(k, "character")
  expect_true(nzchar(k))
})


# ── Two-tier cache_lookup tests ───────────────────────────────────────────────

get_cache_env <- function() Refinitiv:::.refinitiv_cache
get_alias_env <- function() Refinitiv:::.fast_key_aliases

clear_all_caches <- function() {
  env <- get_cache_env()
  rm(list = ls(env, all.names = TRUE), envir = env)
  alias_env <- get_alias_env()
  rm(list = ls(alias_env, all.names = TRUE), envir = alias_env)
}

test_that("cache_lookup returns found=FALSE on empty cache", {
  clear_all_caches()
  result <- Refinitiv:::cache_lookup("fn", "AAPL.O")
  expect_false(result$found)
  expect_type(result$key, "character")
  clear_all_caches()
})

test_that("cache_lookup returns cache hit after cache_set", {
  clear_all_caches()
  cl <- Refinitiv:::cache_lookup("fn", "AAPL.O")
  expect_false(cl$found)

  Refinitiv:::cache_set(cl$key, data.frame(x = 1), 60)

  cl2 <- Refinitiv:::cache_lookup("fn", "AAPL.O")
  expect_true(cl2$found)
  expect_equal(cl2$value, data.frame(x = 1))
  clear_all_caches()
})

test_that("cache_lookup creates alias on canonical hit with different order", {
  clear_all_caches()

  # Store under canonical key (sorted order)
  cl <- Refinitiv:::cache_lookup("fn", c("A", "B", "C"))
  Refinitiv:::cache_set(cl$key, "result_abc", 60)

  # Look up with different order — should find via canonical, create alias
  cl2 <- Refinitiv:::cache_lookup("fn", c("C", "A", "B"))
  expect_true(cl2$found)
  expect_equal(cl2$value, "result_abc")

  # Alias should now exist
  alias_count <- length(ls(get_alias_env(), all.names = TRUE))
  expect_true(alias_count >= 1L)

  # Third lookup with same reordered input — should hit via alias (fast path)
  cl3 <- Refinitiv:::cache_lookup("fn", c("C", "A", "B"))
  expect_true(cl3$found)
  expect_equal(cl3$value, "result_abc")

  clear_all_caches()
})

test_that("cache_lookup alias is cleaned up when canonical entry expires", {
  clear_all_caches()

  # Store with very short TTL
  cl <- Refinitiv:::cache_lookup("fn", c("X", "Y"))
  env <- get_cache_env()
  assign(cl$key, list(value = "data", expires_at = as.numeric(Sys.time()) - 1), envir = env)

  # Create an alias pointing to the expired entry
  fast <- Refinitiv:::cache_key_fast("fn", c("Y", "X"))
  assign(fast, cl$key, envir = get_alias_env())

  # Lookup should NOT return the expired data, and should clean up alias
  cl2 <- Refinitiv:::cache_lookup("fn", c("Y", "X"))
  expect_false(cl2$found)

  clear_all_caches()
})

test_that("rd_ClearCache clears aliases and version cache", {
  clear_all_caches()

  # Create some data + an alias
  cl <- Refinitiv:::cache_lookup("fn", c("A", "B"))
  Refinitiv:::cache_set(cl$key, "data", 60)
  Refinitiv:::cache_lookup("fn", c("B", "A"))  # creates alias

  expect_true(length(ls(get_alias_env(), all.names = TRUE)) >= 1L)

  suppressMessages(Refinitiv::rd_ClearCache())

  expect_equal(length(ls(get_cache_env(), all.names = TRUE)), 0L)
  expect_equal(length(ls(get_alias_env(), all.names = TRUE)), 0L)
  # Version cache was reset
  expect_null(Refinitiv:::.pkgglobalenv[["pkg_version"]])
})

test_that("rd_CacheInfo reports alias count", {
  clear_all_caches()

  cl <- Refinitiv:::cache_lookup("fn", c("A", "B"))
  Refinitiv:::cache_set(cl$key, "data", 60)
  Refinitiv:::cache_lookup("fn", c("B", "A"))  # creates alias

  info <- expect_message(Refinitiv::rd_CacheInfo(), "alias")
  expect_true(info$aliases >= 1L)

  suppressMessages(Refinitiv::rd_ClearCache())
})


# ── get_pkg_version tests ─────────────────────────────────────────────────────

test_that("get_pkg_version returns a version string", {
  ver <- Refinitiv:::get_pkg_version()
  expect_type(ver, "character")
  expect_true(nzchar(ver))
  # Should match the actual package version
  expect_equal(ver, as.character(utils::packageVersion("Refinitiv")))
})

test_that("get_pkg_version caches result", {
  pkg_env <- Refinitiv:::.pkgglobalenv
  # Reset
  pkg_env$pkg_version <- NULL
  v1 <- Refinitiv:::get_pkg_version()
  # Should have been cached
  expect_false(is.null(pkg_env$pkg_version))
  v2 <- Refinitiv:::get_pkg_version()
  expect_identical(v1, v2)
})


# ── Verbose cache diagnostics ───────────────────────────────────────────────

test_that("verbose_cache option logs MISS/STORE/HIT", {
  withr::local_options(refinitiv_verbose_cache = TRUE)
  clear_all_caches()

  key <- Refinitiv:::cache_key_canonical("test_verbose", "x")

  # MISS
  expect_message(Refinitiv:::cache_get(key), "MISS")

  # STORE
  expect_message(Refinitiv:::cache_set(key, data.frame(a = 1), 60), "STORE")

  # HIT
  expect_message(Refinitiv:::cache_get(key), "HIT")

  suppressMessages(Refinitiv::rd_ClearCache())
})

test_that("verbose_cache is silent by default", {
  clear_all_caches()

  key <- Refinitiv:::cache_key_canonical("test_silent", "y")
  expect_silent(Refinitiv:::cache_get(key))
  expect_silent(Refinitiv:::cache_set(key, 42, 10))
  expect_silent(Refinitiv:::cache_get(key))
  suppressMessages(Refinitiv::rd_ClearCache())
})

test_that("verbose cache_lookup logs FAST-HIT and CANONICAL-HIT", {
  withr::local_options(refinitiv_verbose_cache = TRUE)
  clear_all_caches()

  # Store data
  cl <- suppressMessages(Refinitiv:::cache_lookup("fn", c("A", "B")))
  suppressMessages(Refinitiv:::cache_set(cl$key, "data", 60))

  # Canonical hit with different order — logs CANONICAL-HIT
  expect_message(
    Refinitiv:::cache_lookup("fn", c("B", "A")),
    "CANONICAL-HIT"
  )

  # Fast hit via alias — logs FAST-HIT
  expect_message(
    Refinitiv:::cache_lookup("fn", c("B", "A")),
    "FAST-HIT"
  )

  suppressMessages(Refinitiv::rd_ClearCache())
})
