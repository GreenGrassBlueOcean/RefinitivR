# Set a credential in the secure vault

Stores a value under a named key in the package-private credential
vault. The vault is not visible via
[`getOption()`](https://rdrr.io/r/base/options.html) or
[`options()`](https://rdrr.io/r/base/options.html).

## Usage

``` r
refinitiv_vault_set(key, value)
```

## Arguments

- key:

  Character scalar — the credential name (e.g. `"api_key"`,
  `"access_token"`).

- value:

  The value to store.

## Value

`value` (invisibly).

## Details

For backward compatibility the `"api_key"` key is also written through
to `options(.EikonApiKey)`. This behaviour will be removed in a future
release.
