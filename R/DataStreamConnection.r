#' Function to test if the DataStream Credentials are really working
#'
#' @param DatastreamUsername character datastream Username
#' @param DatastreamPassword character datastream password
#'
#' @return TRUE if the DataStream credentials are working and returns the JSON message when not.
#' @export
#' @importFrom httr GET
#'
#' @examples
#' TestDataStreamCredentials(DatastreamUsername = "wrongusername"
#' , DatastreamPassword = "wrongPassword")
TestDataStreamCredentials <- function(DatastreamUsername, DatastreamPassword){

# 1. check input ----
if (missing(DatastreamUsername)){
  stop("Please Supply Datastream Username")
}

if (missing(DatastreamPassword)){
    stop("Please Supply Datastream Password")
}

#2. perform GET operation----
token_url <- paste("http://product.datastream.com/DSWSClient/V1/DSService.svc/rest/Token?username=",
                   DatastreamUsername,
                   "&password=",
                   DatastreamPassword,
                   sep="")
test <- tryCatch({httr::GET(token_url, httr::timeout(5))},
                  error=function(cond) {
                        message(paste("URL does not seem to exist:", token_url, "Either the service is down or the credentials are false"))
                        message("Here's the original error message:")
                        message(cond)
                        # Choose a return value in case of error
                        return(list(val = NA, ErrorMessage = cond))
}, expectation_success=class)


#3. Interpret status ----
if(is.na(test$val) || test$status_code != 200  ){
    switch <- ifelse(("ErrorMessage" %in% names(test)), yes = test$ErrorMessage, no = test)
    warning(paste0("Most likely DataStream Credentials username \"", DatastreamUsername, "\" and password \"", DatastreamPassword, "\" are not ok.\n HTTP message:\n ", paste(switch, collapse = "\n" )))
    return(FALSE)
  } else if(test$status_code == 200){
    message("DataStreamCredentials are working ok")
    return(TRUE)
  }
}



