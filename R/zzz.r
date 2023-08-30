# Hidden package environment
.pkgglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  #reticulate::configure_environment(pkgname)
  options(reticulate.long_as_bit64=TRUE)
  options(reticulate.ulong_as_bit64=TRUE)
  options(refinitiv_base_url = 'http://localhost')
  options(eikon_port = 9060L)
  options(eikon_api = '/api/udf/') #old --> '/api/v1/data'
  options(rdp_api = '/api/rdp/')
  options(rdp_port=9000L)
}
