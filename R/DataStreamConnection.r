#' Test if the DataStream Credentials are valid
#'
#' This function checks the validity of the provided DataStream username and
#' password by sending a request to the DataStream API.
#'
#' @param DatastreamUsername Character. The DataStream username.
#' @param DatastreamPassword Character. The DataStream password.
#'
#' @return Logical. Returns `TRUE` if the credentials are valid. If invalid,
#'   returns `FALSE` and issues a warning with the error message.
#' @export
#' @examples
#' TestDataStreamCredentials(DatastreamUsername = "wrongusername"
#' , DatastreamPassword = "wrongPassword")
TestDataStreamCredentials <- function(DatastreamUsername = NULL, DatastreamPassword = NULL) {


  # 1. Check input ----
  if (is.null(DatastreamUsername) || is.na(DatastreamUsername)) {
    stop("Please supply the DataStream username.")
  }

  if (is.null(DatastreamPassword) || is.na(DatastreamPassword)) {
    stop("Please supply the DataStream password.")
  }

  # 2. Perform GET operation ----
  token_url <- paste0(
    "http://product.datastream.com/DSWSClient/V1/DSService.svc/rest/Token?username=",
    DatastreamUsername,
    "&password=",
    DatastreamPassword
  )

  response <- tryCatch(
    {
      httr2::request(token_url) |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    },
    error = function(cond) {
      message(paste(
        "URL does not seem to exist:", token_url,
        "Either the service is down or the credentials are invalid."
      ))
      message("Original error message:")
      message(cond$message)
      # Return a meaningful error object
      return(NULL)
    }
  )

  # 3. Interpret status and parse response body ----
  if (is.null(response)) {
    warning(paste0(
      "The provided DataStream credentials (username: \"", DatastreamUsername,
      "\", password: \"", DatastreamPassword, "\") are invalid.\n"
    ))
    return(FALSE)
  } else if (httr2::resp_status(response) != 200) {
    warning(paste0(
      "The provided DataStream credentials (username: \"", DatastreamUsername,
      "\", password: \"", DatastreamPassword, "\") are invalid.\n",
      "HTTP status code: ", httr2::resp_status(response)
    ))
    return(FALSE)
  } else {
    # Extract and decode the response body
    response_body <- httr2::resp_body_string(response)
    response_json <- jsonlite::fromJSON(response_body)

    if (!is.null(response_json$TokenValue)) {
      message("DataStream credentials are valid.")
      return(TRUE)
    } else {
      warning("Failed to retrieve a valid token. Credentials may be incorrect.")
      return(FALSE)
    }
  }
}


#' Initialize DataStream Connection
#'
#' @param DatastreamUserName Refinitiv DataStream username
#' @param DatastreamPassword Refinitiv DataStream password
#'
#' @return a DataStream R5 object
#' @export
#'
#' @examples
#' \dontrun{
#' DatastreamUserName <- "Your datastream username"
#' DatastreamPassword <- "Your datastream password"
#' DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)
#' }
DataStreamConnect <- function(DatastreamUserName = NA, DatastreamPassword = NA){

  # 1. check input ----
  if (is.na(DatastreamUserName)){
    try(DatastreamUserName <- getOption("Datastream.Username") )
    if(is.null(DatastreamUserName)){stop("Please supply the DataStream username.")}
  }

  if (is.na(DatastreamPassword)){
    try(DatastreamPassword <- getOption("Datastream.Password") )
    if (is.null(DatastreamPassword)){stop("Please supply the DataStream password.")}
  }

  #2. Perform Main operation ----
  options(Datastream.Username = DatastreamUserName)
  options(Datastream.Password = DatastreamPassword)
  mydsws <- DatastreamDSWS2R::dsws$new()
  return(mydsws)

}
