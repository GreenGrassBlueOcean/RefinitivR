.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  options(reticulate.long_as_bit64=TRUE)
  options(reticulate.ulong_as_bit64=TRUE)
}
