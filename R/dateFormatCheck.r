#' Check date format
#'
#' Checks to see if format is YYYY-MM-DD. Also performs a few other date checks.
#' modified but based on the same function used in https://github.com/DOI-USGS/EGRET
#'
#'
#' @param date character
#' @return condition logical TRUE or FALSE if checks passed or failed
#' @noRd
#' @keywords internal
#'
#' @examples
#' date <- '1985-01-01'
#' dateFormatCheck(date)
#' dateWrong <- '1999/1/7'
#' dateFormatCheck(dateWrong)
dateFormatCheck <- function(date){  # checks for the format YYYY-MM-DD

  tryCatch({
    parts <- strsplit(date,"-",fixed=TRUE)
    condition <- FALSE
    if (length(parts[[1]])>1) {
      if (nchar(parts[[1]][1]) == 4 && nchar(parts[[1]][2]) == 2 && nchar(parts[[1]][3]) == 2){
        testYear <- as.numeric(parts[[1]][1])
        testMonth <- as.numeric(parts[[1]][2])
        testDay <- as.numeric(parts[[1]][3])
        if (!is.na(testYear) && !is.na(testMonth) && !is.na(testDay)){
          if (testMonth <= 12 && testDay <= 31){
            condition <- TRUE
          }
        }
      }
    }
    return(condition)
  }, error = function(e) {

    return(FALSE)
  })


}
