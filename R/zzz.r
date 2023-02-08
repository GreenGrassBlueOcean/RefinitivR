# Hidden package environment
.pkgglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  #reticulate::configure_environment(pkgname)
  options(reticulate.long_as_bit64=TRUE)
  options(reticulate.ulong_as_bit64=TRUE)


  .pkgglobalenv$ek <- list(
    base_url = 'http://127.0.0.1',
    eikon_port = 9000L,
    eikon_api = '/api/v1/data',
    rdp_api = '/api/rdp/discovery/',
    rdp_port = 9060L,
    api_key = NULL
  )


  .pkgglobalenv$rd <- list(
    searchViewParams = c(
      "Query",
      "Filter",
      "View",
      "OrderBy",
      "Boost",
      "Select",
      "Top",
      "Skip",
      "GroupBy",
      "GroupCount"
    )
  )
}


#
# _PythonKeywordArgumentNameToEndPointParameterName = {
#   "boost": "Boost",
#   "features": "Features",
#   "filter": "Filter",
#   "group_by": "GroupBy",
#   "group_count": "GroupCount",
#   "navigators": "Navigators",
#   "order_by": "OrderBy",
#   "query": "Query",
#   "scope": "Scope",
#   "select": "Select",
#   "skip": "Skip",
#   "terms": "Terms",
#   "top": "Top",
#   "view": "View",
# }
