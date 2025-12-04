# Helper function to build the Eikonformulas parameter for the EikonGetData function.

Helper function to build the Eikonformulas parameter for the
EikonGetData function.

## Usage

``` r
TR_Field(
  Field_name = NULL,
  Parameters = NULL,
  sort_dir = NULL,
  sort_priority = NULL
)
```

## Arguments

- Field_name:

  string Field name to request. You can find the list in Data Item
  Browser.

- Parameters:

  named List containing the parameters for the field passed in the
  argument field_name

- sort_dir:

  string Indicate the sort direction. Possible values are \\asc\\ or
  \\desc\\. The default value is \\asc\\

- sort_priority:

  integer Gives a priority to the field for the sorting. The highest
  priority is 0 (zero). The default value is NULL

## Value

a list of list which can be used as import for

## Examples

``` r
TR_Field(Field_name = 'tr.revenue')
#> $tr.revenue
#> list()
#> 
TR_Field(Field_name ='tr.open', sort_dir ='asc', sort_priority = 1)
#> $tr.open
#> $tr.open[[1]]
#> [1] "asc"
#> 
#> $tr.open[[2]]
#> [1] 1
#> 
#> 
TR_Field(Field_name ='TR.GrossProfit', Parameters = list('Scale' = 6, 'Curn'= 'EUR')
        , sort_dir = 'asc', sort_priority = 0)
#> $TR.GrossProfit
#> $TR.GrossProfit$params
#> $TR.GrossProfit$params$Scale
#> [1] 6
#> 
#> $TR.GrossProfit$params$Curn
#> [1] "EUR"
#> 
#> 
#> $TR.GrossProfit[[2]]
#> [1] "asc"
#> 
#> $TR.GrossProfit[[3]]
#> [1] 0
#> 
#> 
```
