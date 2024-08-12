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
JsonHeaderAnalyzer <- function(JsonReturn, Selectedheader, use_field_names_in_headers){


  if("headers" %in% names(JsonReturn)){
    HeaderLoc <- JsonReturn$headers
  } else  if("headers" %in% names(JsonReturn[[1]])){
    HeaderLoc <- JsonReturn[[1]]$headers
  } else {
    stop("not supported")
  }

  CleanedHeaders <- replaceInList(HeaderLoc, function(x)if(is.null(x) || identical(x,"") )NA else x)
  if(length(CleanedHeaders) == 1){CleanedHeaders <- CleanedHeaders[[1]]}


  if(Selectedheader == "title"){
    headernames <- unlist(CleanedHeaders)
    headernames <- headernames[which(names(headernames) == Selectedheader)]
    return(headernames)
  } else if(Selectedheader == "displayName"){

    JsonReturn$headers <- CleanedHeaders #headers[[1]]

    headernames <- lapply( X = JsonReturn$headers
                           , FUN =  function(x, use_field_names_in_headers){
                             if(use_field_names_in_headers){
                               if("field" %in% names(x)){return(x[["field"]])
                               } else {return(x[["displayName"]])}
                             } else {return(x[["displayName"]])}
                           }
                           , use_field_names_in_headers = use_field_names_in_headers
    ) |> unlist()
    return(headernames)
  } else if(Selectedheader == "name"){

    headers <- data.table::rbindlist( CleanedHeaders, fill = TRUE, use.names = TRUE)

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
  }  else {
    stop("Selectedheader not implemented")
  }
}


