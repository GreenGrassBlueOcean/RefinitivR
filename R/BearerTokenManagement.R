#' Show all available custom instruments that have been created
#'
#' @param RDObject Refinitiv Data connection object, defaults to RefinitivJsonConnect()
#' @param debug show api calls defaults to FALSE
#'
#' @return a list of custom Instruments created with all parameters
#' @export
#'
#' @examples
#' \dontrun{
#' test <- get_rdp_streaming_url()
#' }
get_rdp_streaming_url <- function(RDObject = RefinitivJsonConnect(), debug = TRUE){

  handshake <- rd_handshake()
  Request <- RDObject$get_rdp_streaming_url( debug = debug)
  EndPoint <- "streaming/pricing/v1/"
  payload <- NULL
  response <- send_json_request(payload, service = "rdp"
                                , EndPoint = EndPoint
                                , request_type = "GET"
                                , debug = debug
  )

  return(Request)

}

#' Get Bearer Key from Terminal
#'
#' The `rd_handshake` function manages bearer tokens by verifying the existing token's validity
#' using `rd_VerifyToken`. If the token is invalid or expired, or if `force = TRUE`, it performs a handshake
#' to request a new token from the Refinitiv API. The function stores the new token and its expiration time
#' for future use.
#'
#' @param debug Logical. If `TRUE`, prints URLs and additional debugging information for JSON requests. Defaults to `FALSE`.
#' @param force Logical. If `TRUE`, forces fetching a new token even if an existing valid token is present. Defaults to `TRUE`.
#'
#' @return A list with the following fields:
#' \describe{
#'  \item{access_token}{Bearer token (key).}
#'  \item{expires_in}{Number of seconds until the token expires.}
#'  \item{token_type}{Type of token (e.g., "bearer").}
#' }
#'
#' @details
#' The function first checks if an existing bearer token is present and valid. If so, and if `force = FALSE`,
#' it reuses the existing token. Otherwise, it initiates a handshake with the Refinitiv API to obtain a new token.
#' The new token and its expiration time are stored in global options for subsequent use.
#'
#' @examples
#' \dontrun{
#' # Fetch a new token regardless of existing tokens
#' response <- rd_handshake(force = TRUE, debug = TRUE)
#' print(response)
#' }
#'
#' @export
rd_handshake <- function(debug = FALSE, force = TRUE){

  # Check the terminal type (function assumed to be defined elsewhere)
  CheckTerminalType(verbose = debug, force = force)

  # Retrieve existing token from options
  existing_token <- getOption("refinitiv_access_token")
  token_exp <- getOption("refinitiv_token_expiration")
  token_type <- getOption("refinitiv_token_type")

  # Determine whether to use existing token
  use_existing <- FALSE
  if (!force && !is.null(existing_token) && !is.null(token_exp)) {
    is_valid <- rd_VerifyToken(existing_token)

    if (is_valid) {
      if (debug) {
        message("Using existing valid token.")
      }
      use_existing <- TRUE
    } else {
      if (debug) {
        message("Existing token is invalid or expired. Requesting a new token.")
      }
    }
  } else {
    if (force && !is.null(existing_token)) {
      if (debug) {
        message("Force is TRUE. Requesting a new token despite existing token.")
      }
    } else if (debug && (is.null(existing_token) || is.null(token_exp))) {
      message("No existing token found. Requesting a new token.")
    }
  }

  if (use_existing) {
    return(list(
      access_token = existing_token,
      expires_in = token_exp - as.numeric(Sys.time()),
      token_type = token_type
    ))
  }

  # Prepare the payload for handshake
  payload <- list(
    'AppKey' = 'DEFAULT_WORKSPACE_APP_KEY',
    'AppScope' = 'trapi',
    'ApiVersion'= '1',
    'LibraryName' = 'RDP Python Library',
    'LibraryVersion'= '1.3.1'
  )

  # Construct the handshake URL
  handshake_url <-  paste0(getOption("refinitiv_base_url"), ":",
                           getOption("rdp_port"), "/api/handshake")

  if (debug) {
    message("Sending handshake request to URL: ", handshake_url)
  }

  # Send the handshake request
  response <- send_json_request(
    json = payload,
    request_type = "POST",
    debug = debug,
    apikey = 'DEFAULT_WORKSPACE_APP_KEY',
    url = handshake_url
  )

  # Validate the response structure
  required_fields <- c("access_token", "expires_in", "token_type")
  if (!all(required_fields %in% names(response))) {
    warning("Handshake response is missing required fields.")
    return(NULL)
  }

  # Store the new token and its expiration time
  options(refinitiv_access_token = response$access_token)
  options(refinitiv_token_expiration = as.numeric(Sys.time()) + response$expires_in)
  options(refinitiv_token_type = response$token_type)

  if (debug) {
    message("New token acquired and stored successfully.")
  }

  return(response)
}




#' Verify the Validity of a JWT Access Token
#'
#' The `rd_VerifyToken` function checks whether a given JWT (JSON Web Token) is still valid by
#' inspecting its expiration time (`exp` claim). It decodes the token's payload without
#' verifying its signature.
#'
#' @param token A character string representing the JWT access token.
#'
#' @return A logical value:
#' \describe{
#'   \item{`TRUE`}{if the token is valid (i.e., not expired).}
#'   \item{`FALSE`}{if the token is expired or invalid.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example token (replace with a real token)
#' access_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
#' is_valid <- rd_VerifyToken(access_token)
#' print(is_valid)
#' }
#'
#' @export
rd_VerifyToken <- function(token) {
  # Ensure the token is a non-empty string
  if (!is.character(token) || length(token) != 1 || nchar(token) == 0) {
    warning("Token must be a non-empty character string.")
    return(FALSE)
  }

  # Split the token into its three parts
  parts <- strsplit(token, "\\.")[[1]]

  if (length(parts) != 3) {
    warning("Invalid token format. Expected three parts separated by '.'")
    return(FALSE)
  }

  # Function to decode Base64URL
  base64url_decode <- function(input) {
    # Replace URL-specific characters
    input <- gsub("-", "+", input)
    input <- gsub("_", "/", input)
    # Add padding if necessary
    padding_needed <- (4 - (nchar(input) %% 4)) %% 4
    if (padding_needed > 0) {
      input <- paste0(input, strrep("=", padding_needed))
    }
    # Decode using base64enc::base64decode
    raw <- tryCatch({
      base64enc::base64decode(input)
    }, error = function(e) {
      warning("Base64 decoding failed: ", e$message)
      return(NULL)
    })
    return(raw)
  }

  # Decode the payload
  payload_raw <- base64url_decode(parts[2])

  if (is.null(payload_raw)) {
    return(FALSE)
  }

  # Convert raw bytes to character string
  payload_json <- tryCatch({
    rawToChar(payload_raw)
  }, error = function(e) {
    warning("Failed to convert payload to character string: ", e$message)
    return(NULL)
  })

  if (is.null(payload_json)) {
    return(FALSE)
  }

  # Parse JSON using jsonlite::fromJSON
  payload <- tryCatch({
    jsonlite::fromJSON(payload_json)
  }, error = function(e) {
    warning("JSON parsing failed: ", e$message)
    return(NULL)
  })

  if (is.null(payload)) {
    return(FALSE)
  }

  # Check for 'exp' claim
  if (!"exp" %in% names(payload)) {
    warning("'exp' field not found in token payload.")
    return(FALSE)
  }

  # Validate 'exp' is numeric
  if (!is.numeric(payload$exp) || length(payload$exp) != 1) {
    warning("'exp' field is not a valid numeric timestamp.")
    return(FALSE)
  }

  # Current time in Unix epoch
  current_time <- as.numeric(Sys.time())

  # Compare current time with expiration time
  if (current_time < payload$exp) {
    return(TRUE)
  } else {
    warning("Token has expired.")
    return(FALSE)
  }
}



