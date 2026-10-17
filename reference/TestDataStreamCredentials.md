# Test if the DataStream Credentials are valid

This function checks the validity of the provided DataStream username
and password by sending a request to the DataStream API.

## Usage

``` r
TestDataStreamCredentials(DatastreamUsername = NULL, DatastreamPassword = NULL)
```

## Arguments

- DatastreamUsername:

  Character. The DataStream username.

- DatastreamPassword:

  Character. The DataStream password.

## Value

Logical. Returns \`TRUE\` if the credentials are valid. If invalid,
returns \`FALSE\` and issues a warning with the error message.

## Examples

``` r
if (FALSE) { # \dontrun{
TestDataStreamCredentials(
  DatastreamUsername = "wrongusername",
  DatastreamPassword = "wrongPassword"
)
} # }
```
