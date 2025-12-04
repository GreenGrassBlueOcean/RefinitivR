# Get Bearer Key from Terminal

The \`rd_handshake\` function manages bearer tokens by verifying the
existing token's validity using \`rd_VerifyToken\`. If the token is
invalid or expired, or if \`force = TRUE\`, it performs a handshake to
request a new token from the Refinitiv API. The function stores the new
token and its expiration time for future use.

## Usage

``` r
rd_handshake(debug = FALSE, force = TRUE)
```

## Arguments

- debug:

  Logical. If \`TRUE\`, prints URLs and additional debugging information
  for JSON requests. Defaults to \`FALSE\`.

- force:

  Logical. If \`TRUE\`, forces fetching a new token even if an existing
  valid token is present. Defaults to \`TRUE\`.

## Value

A list with the following fields:

- access_token:

  Bearer token (key).

- expires_in:

  Number of seconds until the token expires.

- token_type:

  Type of token (e.g., "bearer").

## Details

The function first checks if an existing bearer token is present and
valid. If so, and if \`force = FALSE\`, it reuses the existing token.
Otherwise, it initiates a handshake with the Refinitiv API to obtain a
new token. The new token and its expiration time are stored in global
options for subsequent use.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch a new token regardless of existing tokens
response <- rd_handshake(force = TRUE, debug = TRUE)
print(response)
} # }
```
