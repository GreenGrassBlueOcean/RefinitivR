# Check if a symbol is really a custom symbol

Check if a symbol is really a custom symbol

## Usage

``` r
CheckifCustomInstrument(symbol, UUID = getOption(".RefinitivUUID"))
```

## Arguments

- symbol:

  character

- UUID:

  character Eikon UUID can be found here: Eikon Terminal –\> help –\>
  about –\> user details: UUID e.g. ABCDE-123456

## Value

boolean TRUE, FALSE or NA

## Examples

``` r
if (FALSE) { # \dontrun{
CheckifCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
CheckifCustomInstrument(symbol = c("test", 'test2') , UUID = "ABCDE-123456")
CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")
CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = NULL)
} # }
```
