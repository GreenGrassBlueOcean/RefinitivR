# Build GET Query String

This function takes a named list (which may include `NULL` elements) and
converts the non-`NULL` elements into a URL-encoded query string
suitable for use in an HTTP GET request. All reserved characters (for
example, spaces, colons) are encoded. If the input list is empty (or
becomes empty after omitting `NULL` elements), the function returns an
empty string.

## Usage

``` r
build_get_query_string(params)
```

## Arguments

- params:

  A named list of parameters. Any element that is `NULL` is omitted.

## Value

A character string representing the URL-encoded query string. If any
parameters remain, the string is prepended with a "?".

## Examples

``` r
if (FALSE) { # \dontrun{
  query_list <- list(
    query    = "R:TSLA.O AND Language:EN",
    limit    = 5,
    dateFrom = "2023-01-01T00:00:00Z",
    extra    = NULL
  )
  qs <- build_get_query_string(query_list)
  # qs will be:
  # "?query=R%3ATSLA.O%20AND%20Language%3AEN&limit=5&dateFrom=2023-01-01T00%3A00%3A00Z"

  # Passing an empty list returns an empty string:
  build_get_query_string(list())
} # }
```
