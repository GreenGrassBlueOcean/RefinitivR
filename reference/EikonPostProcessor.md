# A postprocessor to process Eikon (python) get_Data into r data.frames

As the Python eikon get_data structure contains various list that are
null these should be replaced with NA to prevent disasters when later
handling the data.frame. For example when using the unlist function Null
elements are removed from the lists causing shorter vectors than
expected.

## Usage

``` r
EikonPostProcessor(Eikon_get_dataOuput, SpaceConvertor = ".")
```

## Arguments

- Eikon_get_dataOuput:

  a single Eikon\$get_data result or a list of Eikon\$get_data results.

- SpaceConvertor:

  converts spaces in variables name into one of the following characters
  ".", "," , "-", "\_", default is "."

## Value

a list of a data.frame with cleaned output and the resulting error
data.frame

## See also

EikonNameCleaner

## Examples

``` r
if (FALSE) "internal function no examples" # \dontrun{}
```
