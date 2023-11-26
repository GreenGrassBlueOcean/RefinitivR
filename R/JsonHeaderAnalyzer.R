#' Analyze Jsonheaders
#'
#' @param JsonReturn json list
#' @param Selectedheader character
#'
#' @return vector with headers
#' @noRd
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  internal function no example
#' }
JsonHeaderAnalyzer <- function(JsonReturn, Selectedheader){

  if("headers" %in% names(JsonReturn)){
    HeaderLoc <- JsonReturn$headers
  } else  if("headers" %in% names(JsonReturn[[1]])){
    HeaderLoc <- JsonReturn[[1]]$headers
  } else {
    stop("not supported")
  }

  CleanedHeaders <- replaceInList(HeaderLoc, function(x)if(is.null(x) || identical(x,"") )NA else x)
  headers <- data.table::rbindlist( CleanedHeaders, fill = TRUE, use.names = T)

  if(identical(names(headers), c("name", "type", "decimalChar") )){
    headers <- data.table::setDF(headers)
    headers <- headers[which(names(headers) == Selectedheader)]
    return(headers[[1]])
  } else if(identical(names(headers), c("name", "title", "type", "description", "decimalChar"))){

    ParameterName <- name <- title <- type <- NULL
    headers2 <- headers[, ParameterName := data.table::fifelse(test = (!(type %in% c("string", "number")) & name  != "date")
                                                               , no = name
                                                               , yes = paste0(name,".",tolower(title))
    )
    ][ParameterName == "instrument", ParameterName := "Instrument"
    ][ParameterName == "date", ParameterName := "Date"
    ]
    return(headers2$ParameterName)

  }









}
