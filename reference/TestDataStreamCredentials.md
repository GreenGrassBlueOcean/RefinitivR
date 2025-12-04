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
TestDataStreamCredentials(DatastreamUsername = "wrongusername"
, DatastreamPassword = "wrongPassword")
#> URL does not seem to exist: http://product.datastream.com/DSWSClient/V1/DSService.svc/rest/Token?username=wrongusername&password=wrongPassword Either the service is down or the credentials are invalid.
#> Original error message:
#> HTTP 403 Forbidden.
#> Warning: The provided DataStream credentials (username: "wrongusername", password: "wrongPassword") are invalid.
#> [1] FALSE
```
