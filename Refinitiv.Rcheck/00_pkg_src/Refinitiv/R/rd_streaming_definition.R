#' StreamDefinition - Base class for stream definitions
#'
#' @description
#' Base class for defining streaming requests. Subclasses implement
#' specific stream types (pricing, analytics, etc.)
#'
#' @import R6
#' @noRd
StreamDefinition <- R6::R6Class("StreamDefinition",
  public = list(
    #' @description Initialize StreamDefinition
    #' @param universe Character vector of instrument RICs
    #' @param fields Character vector of field names
    #' @param parameters Optional parameters list
    #' @param domain Domain for OMM streams (default "MarketPrice")
    initialize = function(universe, fields, parameters = NULL, domain = "MarketPrice") {
      # Validate inputs
      validate_streaming_params(universe, fields)
      
      private$.universe <- universe
      private$.fields <- fields
      private$.parameters <- parameters
      private$.domain <- domain
    },
    
    #' @description Get Stream object from this definition
    #' @param manager Optional StreamManager (creates new if not provided)
    #' @return Stream object
    get_stream = function(manager = NULL) {
      if (is.null(manager)) {
        manager <- StreamManager$new(stream_type = private$.get_stream_type())
      }
      
      # Stream class is defined in rd_streaming_stream.R
      # Both files are part of the same package, so Stream will be available
      # when the package loads (R loads all .R files in the R/ directory)
      stream <- Stream$new(
        definition = self,
        manager = manager
      )
      
      return(stream)
    },
    
    #' @description Validate configuration
    #' @return TRUE if valid, list of errors if not
    validate = function() {
      errors <- list()
      
      if (length(private$.universe) == 0) {
        errors <- c(errors, "universe must contain at least one instrument")
      }
      
      if (length(private$.fields) == 0) {
        errors <- c(errors, "fields must contain at least one field")
      }
      
      if (length(errors) > 0) {
        return(errors)
      }
      
      return(TRUE)
    },
    
    #' @description Convert to WebSocket request format
    #' @param streaming Logical, TRUE for streaming, FALSE for snapshot
    #' @return JSON string
    to_request = function(streaming = TRUE, stream_id = NULL) {
      # Default implementation for OMM/tr_json2 protocol
      return(create_omm_stream_request(
        domain = private$.domain,
        universe = private$.universe,
        fields = private$.fields,
        streaming = streaming,
        stream_id = stream_id
      ))
    },
    
    #' @description Get universe
    #' @return Character vector of instruments
    get_universe = function() {
      return(private$.universe)
    },
    
    #' @description Get fields
    #' @return Character vector of fields
    get_fields = function() {
      return(private$.fields)
    },
    
    #' @description Get parameters
    #' @return Parameters list
    get_parameters = function() {
      return(private$.parameters)
    },
    
    #' @description Get domain
    #' @return Domain string
    get_domain = function() {
      return(private$.domain)
    }
  ),
  
  private = list(
    .universe = NULL,
    .fields = NULL,
    .parameters = NULL,
    .domain = "MarketPrice",
    
    .get_stream_type = function() {
      # Default to pricing
      return("pricing")
    }
  )
)

#' Pricing Stream Definition
#'
#' @description
#' Definition class for pricing streams (market data).
#' Use \code{rd_streaming_pricing$Definition$new()} to create a pricing stream definition.
#'
#' @format
#' A list containing the \code{Definition} R6 class constructor.
#'
#' @details
#' To create a pricing stream definition, call:
#' \code{rd_streaming_pricing$Definition$new(universe, fields, parameters = NULL, domain = "MarketPrice")}
#' 
#' Arguments:
#' \itemize{
#'   \item \code{universe}: Character vector of instrument RICs
#'   \item \code{fields}: Character vector of field names
#'   \item \code{parameters}: Optional parameters list
#'   \item \code{domain}: Domain for OMM streams (default "MarketPrice")
#' }
#'
#' @return PricingStreamDefinition object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stream_def <- rd_streaming_pricing$Definition$new(
#'   universe = c("EUR=", "GBP="),
#'   fields = c("BID", "ASK", "DSPLY_NAME")
#' )
#' stream <- stream_def$get_stream()
#' }
rd_streaming_pricing <- list(
  Definition = R6::R6Class("PricingStreamDefinition",
    inherit = StreamDefinition,
    public = list(
      initialize = function(universe, fields, parameters = NULL, domain = "MarketPrice") {
        super$initialize(universe = universe, fields = fields, 
                        parameters = parameters, domain = domain)
      }
    ),
    private = list(
      .get_stream_type = function() {
        return("pricing")
      }
    )
  )
)

