# Check if Conda exists, if not instals miniconda, add the python eikon module to the python environment r-eikon

This function can also be used to update the required python packages so
that you can always use the latest version of the pyhton packages numpy
and eikon. For a pure reinstall of miniconda and the refinitiv python
libraries set set reset = TRUE and update = FALSE

## Usage

``` r
install_eikon(
  method = "conda",
  conda = "auto",
  envname = "r-eikon",
  update = TRUE,
  reset = FALSE,
  restart_session = TRUE
)
```

## Arguments

- method:

  Installation method. By default, "auto" automatically finds a method
  that will work in the local environment. Change the default to force a
  specific installation method. Note that the "virtualenv" method is not
  available on Windows.

- conda:

  The path to a conda executable. Use "auto" to allow reticulate to
  automatically find an appropriate conda binary. See Finding Conda in
  the reticulate package for more details

- envname:

  the name for the conda environment that will be used, default r-eikon.
  Don't Change!

- update:

  boolean, allow to rerun the command to update the miniconda
  environment and the packages required to update the python packages
  numpy,eikon, and refinitiv dataplatform defaults to TRUE

- reset:

  boolean, this will remove the miniconda r-eikon environment and
  reinstall miniconda, the conda environment and relevant packages.

- restart_session:

  boolean, Restart R session after installing (note this will only occur
  within RStudio).

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
install_eikon()
} # }

if (FALSE) { # \dontrun{
# when you get the error the refinitiv library cannot
# be found anymore or errors during installation:
install_eikon(update = FALSE, reset = TRUE)
} # }
```
