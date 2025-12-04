# Output processor for python function RDP\$get_search_metadata

Output processor for python function RDP\$get_search_metadata

## Usage

``` r
Process_RDP_output(python_json, RemoveNA = FALSE, SpaceConvertor = NULL)
```

## Arguments

- python_json:

  python json string

- RemoveNA:

  boolean remove NA value's defaults to FALSE

## Value

r data.frame

## See also

\[get_search_metadata()\]

\[rd_GetHistory()\]

## Examples

``` r
if (FALSE) { # \dontrun{
"internal function don't use directly"
path = paste0("C:\\Users\\XXXX\\Documents\\GitHub",
"\\RefinitivR\\tests\\testthat\\PY_get_search_metadata_input.py")
PY_get_search_metadata_input <- reticulate::r_to_py(
 reticulate::py_load_object(file = path))
 r_df <- Process_RDP_output(PY_get_search_metadata_input)
} # }
```
