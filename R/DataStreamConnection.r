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
#' \dontrun{
#' TestDataStreamCredentials(
#'   DatastreamUsername = "wrongusername",
#'   DatastreamPassword = "wrongPassword"
#' )
#' }
TestDataStreamCredentials <- function(DatastreamUsername = NULL, DatastreamPassword = NULL) {
  # 1. Check input ----
  if (is.null(DatastreamUsername) || is.na(DatastreamUsername)) {
    stop("Please supply the DataStream username.")
  }

  if (is.null(DatastreamPassword) || is.na(DatastreamPassword)) {
    stop("Please supply the DataStream password.")
  }

  # 2. Perform GET operation ----
  encoded_user <- utils::URLencode(as.character(DatastreamUsername), reserved = TRUE, repeated = TRUE)
  encoded_pass <- utils::URLencode(as.character(DatastreamPassword), reserved = TRUE, repeated = TRUE)
  token_url <- paste0(
    "https://product.datastream.com/DSWSClient/V1/DSService.svc/rest/Token?username=",
    encoded_user,
    "&password=",
    encoded_pass
  )

  response <- tryCatch(
    {
      httr2::request(token_url) |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    },
    error = function(cond) {
      message("DataStream token endpoint request failed. Either the service is down or the credentials are invalid.")
      message("Original error message:")
      safe_msg <- sub("\\?.*$", "", cond$message)
      message(safe_msg)
      # Return a meaningful error object
      return(NULL)
    }
  )

  # 3. Interpret status and parse response body ----
  if (is.null(response)) {
    warning(paste0(
      "The provided DataStream credentials for username \"", DatastreamUsername,
      "\" are invalid.\n"
    ))
    return(FALSE)
  } else if (httr2::resp_status(response) != 200) {
    warning(paste0(
      "The provided DataStream credentials for username \"", DatastreamUsername,
      "\" are invalid.\n",
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
DataStreamConnect <- function(DatastreamUserName = NA, DatastreamPassword = NA) {
  is_missing_cred <- function(x) {
    is.null(x) || is.na(x) || (is.character(x) && trimws(x) == "")
  }

  # 1. check input ----
  if (is_missing_cred(DatastreamUserName)) {
    DatastreamUserName <- refinitiv_vault_get("datastream_username")
    if (is_missing_cred(DatastreamUserName)) {
      env_user <- Sys.getenv("DatastreamUsername", unset = "")
      if (nzchar(env_user)) DatastreamUserName <- env_user
    }
    if (is_missing_cred(DatastreamUserName)) {
      DatastreamUserName <- getOption("Datastream.Username")
    }
    if (is_missing_cred(DatastreamUserName)) {
      stop("Please supply the DataStream username.")
    }
  }

  if (is_missing_cred(DatastreamPassword)) {
    DatastreamPassword <- refinitiv_vault_get("datastream_password")
    if (is_missing_cred(DatastreamPassword)) {
      env_pass <- Sys.getenv("DatastreamPassword", unset = "")
      if (nzchar(env_pass)) DatastreamPassword <- env_pass
    }
    if (is_missing_cred(DatastreamPassword)) {
      DatastreamPassword <- getOption("Datastream.Password")
    }
    if (is_missing_cred(DatastreamPassword)) {
      stop("Please supply the DataStream password.")
    }
  }

  # 2. Perform Main operation ----
  refinitiv_vault_set("datastream_username", DatastreamUserName)
  refinitiv_vault_set("datastream_password", DatastreamPassword)
  mydsws <- DatastreamDSWS2R::dsws$new(
    username = DatastreamUserName,
    password = DatastreamPassword
  )
  return(mydsws)
}
