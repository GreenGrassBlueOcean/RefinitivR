# Package-level redactor for httptest2 recorded fixtures.
#
# Strips sensitive headers and normalises the port so that fixtures
# recorded against Eikon (9060) or Workspace (9000) are interchangeable.

function(resp) {
  # 1. Remove sensitive headers (request side)
  resp <- redact_headers(resp, headers = c(
    "x-tr-applicationid",
    "Authorization"
  ))

  # 2. Normalise host to "lh" (short) and port to 9000 so fixtures are
  #    portable between Eikon (9060) and Workspace (9000), and paths
  #    stay shorter for tar portability.
  resp$url <- gsub("localhost(:\\d+)?", "lh:9000", resp$url)

  # 3. Scrub bearer tokens from response bodies
  resp <- gsub_response(resp, "Bearer [^\" ]+", "Bearer REDACTED")

  resp
}
