# Convert plain URLs in a string to clickable HTML links

This internal helper function finds URL substrings in the input text and
wraps them in HTML anchor tags with a line break so that they become
clickable when rendered.

## Usage

``` r
make_links_clickable(text)
```

## Arguments

- text:

  A character string.

## Value

A character string with plain URLs replaced by clickable links.
