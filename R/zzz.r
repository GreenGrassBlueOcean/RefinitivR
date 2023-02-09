# Hidden package environment
.pkgglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  #reticulate::configure_environment(pkgname)
  options(reticulate.long_as_bit64=TRUE)
  options(reticulate.ulong_as_bit64=TRUE)
  options(refinitiv_base_url = 'http://localhost')
  options(eikon_port = 9000L)
  options(eikon_api = '/api/v1/data')
  options(rdp_api = '/api/rdp/discovery/')
  options(rdp_port=9060L)

# #
# #
# #
# #   .pkgglobalenv$ek <- list(
# #     ,
# #     eikon_port = 9000L,
# #     ,
# #
# #     rdp_port = 9060L,
# #     api_key = NULL
# #   )
#
#
#   .pkgglobalenv$rd <- list(
#     searchViewParams = c(
#       "Query",
#       "Filter",
#       "View",
#       "OrderBy",
#       "Boost",
#       "Select",
#       "Top",
#       "Skip",
#       "GroupBy",
#       "GroupCount"
#     )
#   )
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
