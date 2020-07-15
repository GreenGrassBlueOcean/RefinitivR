#' Remove <NA> NaN "" from dataframes and replace with NA while retaining column class
#'
#' the function only operates on character and numeric columns
#' @param df data.frame with/without  <NA> NaN ""
#'
#' @return data.frame with only NA entries in the right column class
#' @export
#'
#' @examples
#' dfr<-data.frame(A=c(1,2,NaN,3),B=c("a", "<NA>" ,"","d")
#' , C = c(as.Date("2012-01-01"), as.Date("2012-01-02"), as.Date("2012-01-03"), as.Date("2012-01-04")))
#' make.true.NA_df(dfr)
make.true.NA_df <- function(df){

make.true.NA <- function(x){
  if(is.character(x)||is.factor(x)){
    is.na(x) <- x %in% c("NA", "<NA>", ""); x
  } else if (is.numeric(x)){
    is.na(x) <- x %in% c(NaN); x
  } else {
    return(x)}
}

return(as.data.frame(lapply(df, make.true.NA), stringsAsFactors = FALSE ))
}
