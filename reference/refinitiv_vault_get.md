# Retrieve a credential from the secure vault

Returns the stored value for `key`, or `NULL` if not present. For the
`"api_key"` key a fallback to `getOption(".EikonApiKey")` is provided
for backward compatibility.

## Usage

``` r
refinitiv_vault_get(key)
```

## Arguments

- key:

  Character scalar.

## Value

Stored value, or `NULL`.
