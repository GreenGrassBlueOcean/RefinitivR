# Clear credentials from the vault

When called with no arguments, removes **all** stored credentials. When
`keys` is supplied, only those keys are removed.

## Usage

``` r
refinitiv_vault_clear(keys = NULL)
```

## Arguments

- keys:

  Character vector of key names, or `NULL` (default) to clear
  everything.

## Value

`TRUE` (invisibly).
