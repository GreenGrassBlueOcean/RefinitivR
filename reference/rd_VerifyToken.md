# Verify the Validity of a JWT Access Token

The \`rd_VerifyToken\` function checks whether a given JWT (JSON Web
Token) is still valid by inspecting its expiration time (\`exp\` claim).
It decodes the token's payload without verifying its signature.

## Usage

``` r
rd_VerifyToken(token)
```

## Arguments

- token:

  A character string representing the JWT access token.

## Value

A logical value:

- \`TRUE\`:

  if the token is valid (i.e., not expired).

- \`FALSE\`:

  if the token is expired or invalid.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example token (replace with a real token)
access_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
is_valid <- rd_VerifyToken(access_token)
print(is_valid)
} # }
```
