library(httptest2)

# Live tests require a running Eikon Desktop or LSEG Workspace terminal.
# has_live_api() in helper.R performs a real handshake against the local
# proxy — no API key or env var needed. Without a running terminal, all
# live tests are skipped.

# Neutralise the proxy-resilience delays for the whole test session so the
# suite runs fast. These options drive the inter-chunk sleep, retry-round
# backoff (chunked_download), and transport-level connection-reset retries
# (send_json_request). Production defaults (sleep 1-2s, backoff 3s, 3 reset
# retries with 5/10/20s waits) are intentionally high to survive a flaky
# LSEG proxy, but they would make tests crawl and offline error-path tests
# sleep needlessly. Tests that specifically exercise these delays can
# override the options locally.
options(
  refinitiv_chunk_sleep     = 0,
  refinitiv_chunk_backoff   = 0,
  refinitiv_reset_retries   = 0L,
  refinitiv_reset_base_wait = 0
)
