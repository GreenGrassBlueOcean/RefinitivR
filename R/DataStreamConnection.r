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
test <- httr::GET(token_url, httr::timeout(300))


#3. Interpret status ----
if(test$status_code == 200){
  message("DataStreamCredentials are working ok")
  return(TRUE)
} else
  warning(paste0("DataStream Credentials username \"", DatastreamUsername, "\" and password \"", DatastreamPassword, "\" are not ok.\n HTTP message:\n ", paste(test, collapse = "\n" )))
  return(test)
}
