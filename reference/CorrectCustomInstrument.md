# Function to check if custom symbol is in format "S)INSTRUMENTSYMBOL.UUID"

Function to check if custom symbol is in format
"S)INSTRUMENTSYMBOL.UUID"

## Usage

``` r
CorrectCustomInstrument(symbol, UUID = getOption(".RefinitivUUID"))
```

## Arguments

- symbol:

  character

- UUID:

  character Eikon UUID can be found here: Eikon Terminal –\> help –\>
  about –\> user details: UUID e.g. ABCDE-123456

## Value

character with corrected symbol

## See also

\[rd_ManageCustomInstruments()\]

## Examples

``` r
CorrectCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
#> [1] "S)test.ABCDE-123456"
CorrectCustomInstrument(symbol = "S)test" , UUID = "ABCDE-123456")
#> [1] "S)test.ABCDE-123456"
CorrectCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")
#> [1] "S)test.ABCDE-123456"
```
