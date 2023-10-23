#' Converts a Python object to an r object using python json as intermediate method.
#'
#' @param x Python object
#' @param py_json Python json module
#'
#' @return r list
#' @keywords internal
#' @noRd
#'
#' @examples
#' x_py <- reticulate::py_run_string('obj = [2435823760, 123, ["abc", 1234567890987654321]];', convert = FALSE)
#' PyJsonConvertor(x_py$obj)
PyJsonConvertor <- function(x, py_json = getOption("py_json")) {

  if(is.null(py_json)) {
    py_json <- reticulate::import("json")
  }


  x_json <- py_json[["dumps"]](x)
  jsonlite::fromJSON(x_json, simplifyVector = FALSE)
}
