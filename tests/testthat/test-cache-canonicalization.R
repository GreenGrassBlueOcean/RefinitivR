# ── Cache key canonicalization tests ──────────────────────────────────────────

test_that("cache_key is order-invariant for character vectors", {
  k1 <- Refinitiv:::cache_key("fn", c("AAPL.O", "MSFT.O", "GOOG.O"))
  k2 <- Refinitiv:::cache_key("fn", c("MSFT.O", "AAPL.O", "GOOG.O"))
  k3 <- Refinitiv:::cache_key("fn", c("GOOG.O", "MSFT.O", "AAPL.O"))
  expect_equal(k1, k2)
  expect_equal(k1, k3)
})

test_that("cache_key is order-invariant for large RIC vectors", {
  rics_a <- paste0("RIC", sprintf("%05d", 1:20000))
  rics_b <- rev(rics_a)
  rics_c <- sample(rics_a)
  k_a <- Refinitiv:::cache_key("EikonGetData", rics_a, "TR.CommonName")
  k_b <- Refinitiv:::cache_key("EikonGetData", rics_b, "TR.CommonName")
  k_c <- Refinitiv:::cache_key("EikonGetData", rics_c, "TR.CommonName")
  expect_equal(k_a, k_b)
  expect_equal(k_a, k_c)
})

test_that("cache_key preserves duplicates (different from unique set)", {
  k_dup <- Refinitiv:::cache_key("fn", c("A", "A", "B"))
  k_uniq <- Refinitiv:::cache_key("fn", c("A", "B"))
  expect_false(k_dup == k_uniq)
})

test_that("cache_key does NOT sort single-element character vectors", {
  # Single-element: no sorting needed, but different values → different keys
  k1 <- Refinitiv:::cache_key("fn", "AAPL.O")
  k2 <- Refinitiv:::cache_key("fn", "MSFT.O")
  expect_false(k1 == k2)
})

test_that("cache_key still differentiates by function name", {
  k1 <- Refinitiv:::cache_key("EikonGetData", c("A", "B"))
  k2 <- Refinitiv:::cache_key("rd_GetData", c("A", "B"))
  expect_false(k1 == k2)
})

test_that("cache_key still sorts named lists recursively", {
  k1 <- Refinitiv:::cache_key("fn", list(a = 1, b = 2))
  k2 <- Refinitiv:::cache_key("fn", list(b = 2, a = 1))
  expect_equal(k1, k2)
})

test_that("cache_key handles numeric vectors without sorting", {
  # Numeric vector order SHOULD matter (e.g. parameter values)
  k1 <- Refinitiv:::cache_key("fn", c(1, 2, 3))
  k2 <- Refinitiv:::cache_key("fn", c(3, 2, 1))
  # Numeric vectors are NOT sorted, so these should differ
  expect_false(k1 == k2)
})


# ── Verbose cache diagnostics ───────────────────────────────────────────────

test_that("verbose_cache option logs MISS/STORE/HIT", {
  withr::local_options(refinitiv_verbose_cache = TRUE)

  key <- Refinitiv:::cache_key("test_verbose", "x")

  # MISS
  expect_message(Refinitiv:::cache_get(key), "MISS")

  # STORE
  expect_message(Refinitiv:::cache_set(key, data.frame(a = 1), 60), "STORE")

  # HIT
  expect_message(Refinitiv:::cache_get(key), "HIT")

  # Clean up
  suppressMessages(Refinitiv::rd_ClearCache())
})

test_that("verbose_cache is silent by default", {
  key <- Refinitiv:::cache_key("test_silent", "y")
  expect_silent(Refinitiv:::cache_get(key))
  expect_silent(Refinitiv:::cache_set(key, 42, 10))
  expect_silent(Refinitiv:::cache_get(key))
  Refinitiv::rd_ClearCache()
})
