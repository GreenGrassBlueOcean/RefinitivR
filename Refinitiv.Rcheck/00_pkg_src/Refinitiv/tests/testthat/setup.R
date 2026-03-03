library(httptest2)

# Live tests require a running Eikon Desktop or LSEG Workspace terminal.
# has_live_api() in helper.R performs a real handshake against the local
# proxy — no API key or env var needed. Without a running terminal, all
# live tests are skipped.
