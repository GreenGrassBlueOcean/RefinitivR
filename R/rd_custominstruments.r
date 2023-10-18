#' Build a holiday object for a custom instrument
#'
#' @param dates vector of character dates in format YYYY-MM-DD
#' @param reasons character vector of reaons for this holida e.g. Christmas
#'
#' @return object with holidays
#' @export
#' @seealso [rd_ManageCustomInstruments()]
#'
#'
#' @examples
#' CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
#' , reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))
CustomInstrumentHolidayBuilder <- function(dates = NULL, reasons = NULL){
  #   "holidays": [
  #     {
  #       "date": "2022-12-18",
  #       "reason": "Hanukkah"
  #     }
  #   ],

  if(is.null(dates)){
    stop("dates can not be NULL, but should be string in formt YYYY-MM-DD")
  }

  #Check Dates
  if(!all(stringi::stri_detect(str = dates
                              , regex = "[1-2][0-9]{3}-[0-1][0-9]-[0-3][0-9]" ))){
    stop("dates should be string and should be all in correct format YYYY-MM-DD")
  }

  holidays <- lapply( X = 1:length(dates)
                    , FUN = function(x, dates, reasons){
                              if(!is.null(reasons[x])){
                                  return(list( "date" = dates[x]
                                             , "reason" = reasons[x]))
                              } else {
                                return(list( "date" = dates[x]))
                              }
                      }
                    , dates = dates
                    , reasons = reasons
                    )


  holidays <- holidays |>
              structure(class = "Refinitiv_holidays")
  return(holidays)
}




#' Build a user defined continuation object for a custom instrument
#'
#' @param root CC or CL
#' @param rollover volume based or day based
#' @param spreadAdjustment arithmetic or percentage
#'
#' @return object with user defined continuation
#' @export
#' @seealso [rd_ManageCustomInstruments()]
#'
#' @examples
#' \dontrun{
#'  # no example cause function not implemented yet
#' }
CustomInstrumentUDCBuilder <- function( root
                                      , rollover
                                      , spreadAdjustment
                                      ){
  UDC <- NULL
  UDC <- UDC |> structure(class = "Refinitiv_udc")

  return("This function is not implemented yet")

# user defined continuation
#volume based

# {
#   "udc": {
#     "root": "CC",
#     "months": {
#       "numberOfYears": 3,
#       "includeAllMonths": true
#     },
#     "rollover": {
#       "volumeBased": {
#         "method": "volume",
#         "numberOfDays": 1,
#         "joinAtDay": 1,
#         "rollOccursWithinMonths": 4,
#         "rollOnExpiry": true
#       }
#     },
#     "spreadAdjustment": {
#       "adjustment": "arithmetic",
#       "method": "close-to-close",
#       "backwards": true
#     }
#   }
# }

# day based
# {
#   "udc": {
#     "root": "CL",
#     "months": {
#       "numberOfYears": 3,
#       "includeAllMonths": true
#     },
#     "rollover": {
#       "dayBased": {
#         "method": "daysBeforeExpiry",
#         "numberOfDays": 3,
#         "monthsPrior": 1
#       }
#     },
#     "spreadAdjustment": {
#       "adjustment": "percentage",
#       "method": "close-to-close",
#       "backwards": false
#     }
#   }
# }

# manual
# {
#   "type": "udc",
#   "udc": {
#     "root": "CC",
#     "rollover": {
#       "manual": [
#         {
#           "month": 7,
#           "year": 2021,
#           "startDate": "2021-02-01"
#         }
#       ]
#     },
#     "spreadAdjustment": {
#       "adjustment": "arithmetic",
#       "method": "close-to-close",
#       "backwards": true
#     }
#   }
# }
}



#' Function to check if custom symbol is in format "S)INSTRUMENTSYMBOL.UUID"
#'
#' @param symbol character
#' @param UUID character  Eikon UUID can be found here: Eikon Terminal --> help --> about --> user details: UUID e.g. ABCDE-123456
#'
#' @return character with corrected symbol
#' @seealso [rd_ManageCustomInstruments()]
#' @export
#'
#' @examples
#' CorrectCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
#' CorrectCustomInstrument(symbol = "S)test" , UUID = "ABCDE-123456")
#' CorrectCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")
CorrectCustomInstrument <- function(symbol, UUID = getOption(".RefinitivUUID")){

  #Check if symbol end with .UUID
  if(!stringi::stri_endswith(symbol, fixed = paste0(".",UUID))){
    #Check if dot is missing
    if(stringi::stri_endswith(symbol, fixed = paste0(UUID))){
      stop(paste0("Custom Symbol '",symbol,"' can not be corrected by CorrectCustomInstrument Check manually"))
    }
    symbol <- paste0(symbol, ".", UUID)
  }

  #Check if symbol starts with "S)"
  if(!stringi::stri_startswith(symbol, fixed = "S)")){
    symbol <- paste0("S)", symbol)
  }


  return(symbol)
}



#' Check if a symbol is really a custom symbol
#'
#' @param symbol character
#' @param UUID character  Eikon UUID can be found here: Eikon Terminal --> help --> about --> user details: UUID e.g. ABCDE-123456
#'
#' @return boolean TRUE, FALSE or NA
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' CheckifCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
#' CheckifCustomInstrument(symbol = c("test", 'test2') , UUID = "ABCDE-123456")
#' CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")
#' CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = NULL)
#' }
CheckifCustomInstrument <- function(symbol, UUID = getOption(".RefinitivUUID")){

  #Check if symbol end with .UUID
  if(!is.null(UUID)){
    IsCustomInstrument <- lapply(symbol, function(x){
      if(!(stringi::stri_endswith(x, fixed = paste0(".",UUID)) &
          stringi::stri_startswith(x, fixed = "S)"))){
      return(FALSE)
    } else{
      return(TRUE)
    }
    })
  } else{
      IsCustomInstrument <- rep(NA, length(symbol))
  }

  return(unlist(IsCustomInstrument))
}




#' Build a basket element for a custom instrument
#'
#' @param RICs character vector of RICs in the basket
#' @param Weights numeric vector of weights in the basket
#' @param Normalize boolean Normalize all weight to a maximum of 1?
#'
#' @seealso [rd_ManageCustomInstruments()]
#'
#' @return a basket object that can be used
#' @export
#'
#' @examples
#' \dontrun{
#' CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))
#' }
CustomInstrumentBasketBuilder <- function(RICs =NULL, Weights =NULL, Normalize = TRUE){

  #   "basket": {
  #     "constituents": [
  #       {
  #         "ric": "LSEG.L",
  #         "weight": 50
  #       },
  #       {
  #         "ric": "EPAM.N",
  #         "weight": 50
  #       }
  #     ],
  #     "normalizeByWeight": true
  #   }

  if(is.null(RICs)){
    stop("Parameter RICs can not be NULL")
  }
  if(is.null(Weights)){
    stop("Parameter Weights can not be NULL")
  }

  if(length(RICs) != length(Weights)){
    stop("length of Rics and Weights should be the same")
  }

  if(!all(Weights > 0) & !(all(Weights < 1))){
    warning("weights should normally be between 0-1 for non leveraged long only, currently this basket contains leveraged or short elements")
  }

  constituents <- lapply(X = 1:length(Weights)
                        , FUN = function(x, RICs, Weights){list( "ric" = RICs[x]
                                                               , "weight" = Weights[x] * 100)}
                        , RICs = RICs
                        , Weights = Weights
                        )


  Basket <- list( "constituents" = constituents
                , "normalizeByWeight" = Normalize
                ) |> structure(class = "Refinitiv_basket")
  return(Basket)
}


# {
#   "symbol": "S)lseg_epam.PAXTRA12345",
#   "type": "basket",
#   "basket": {
#     "constituents": [
#       {
#         "ric": "LSEG.L",
#         "weight": 50
#       },
#       {
#         "ric": "EPAM.N",
#         "weight": 50
#       }
#     ],
#     "normalizeByWeight": true
#   },
#   "currency": "USD"
# }
#



#' CREATE, GET, UPDATE or DELETE a custom instrument
#'
#' @param operation character one of "CREATE", "GET", "UPDATE", "DELETE"
#' @param symbol character instrument symbol in the format "S)someSymbol.YOURUUID"
#' @param formula formula consisting of rics (fields can be specified by comma)
#' @param basket of rics and weights use CustomInstrumentBasketBuilder()
#' @param udc optional a user defined continuation object as returned by CustomInstrumentUDCBuilder()
#' @param currency 3-letter code of the currency of the instrument, e.g. GBP
#' @param instrumentName Human-readable name of the instrument. Maximum of 16 characters.
#' @param exchangeName 4-letter code of the listing exchange.
#' @param holidays List of custom calendar definitions. use CustomInstrumentHolidayBuilder() if required
#' @param timeZone character Time Series uses an odd custom 3-letter value for time zone IDs, e.g. "LON" for London.
#' @param description character Free text field from the user to put any notes or text. Up to 1000 characters.
#' @param UUID character  Eikon UUID can be found here: Eikon Terminal --> help --> about --> user details: UUID e.g. ABCDE-123456
#' @param RDObject Refinitiv Data connection object, defaults to RefinitivJsonConnect()
#' @param debug boolean prints intermediate api calls to console
#'
#' @return list with data of the result of the request.
#' @export
#'
#' @importFrom methods is
#'
#' @seealso CustomInstrumentBasketBuilder()
#' @seealso CustomInstrumentHolidayBuilder()
#' @seealso CustomInstrumentUDCBuilder()
#'
#' @examples
#' \dontrun{
#'  # Create Simple Instrument
#'  rd_ManageCustomInstruments(operation = "CREATE", symbol = "testAAPLandAMZN",
#'                               formula = "AAPL.O + AMZN.O")
#'  #'  #get instrument details
#'  rd_ManageCustomInstruments(operation = "GET", symbol = "testAAPLandAMZN")
#'
#'  # Update Instrument formula
#'  rd_ManageCustomInstruments(operation = "UPDATE", symbol = "testAAPLandAMZN",
#'                               formula = "AAPL.O + 2 * AMZN.O")
#'  #Delete
#'  rd_ManageCustomInstruments(operation = "DELETE", symbol = "testAAPLandAMZN")
#'
#' #build a custom instrument with a basket
#' basket <- CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"), Weights = c(0.5, 0.5))
#'
#' rd_ManageCustomInstruments(operation = "CREATE", symbol = "InterestingBasket",
#'                               basket = basket, currency = "USD")
#'
#' #update the basket with some holidays
#' holidays <-  CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
#'        , reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))
#'
#'
#' rd_ManageCustomInstruments(operation = "UPDATE", symbol = "InterestingBasket",
#'                              holidays = holidays )
#'  #Delete instrument
#'  rd_ManageCustomInstruments(operation = "DELETE", symbol = "InterestingBasket")
#'
#' RealInstrumentName <- CorrectCustomInstrument("InterestingBasket")
#' }
rd_ManageCustomInstruments <- function( RDObject = RefinitivJsonConnect()
                                      , symbol = NULL
                                      , formula = NULL
                                      , basket = NULL
                                      , udc = NULL
                                      , currency = NULL
                                      , instrumentName = NULL
                                      , exchangeName = NULL
                                      , holidays = NULL
                                      , timeZone = NULL
                                      , description = NULL
                                      , operation = c("CREATE","GET", "UPDATE", "DELETE")
                                      , UUID = getOption(".RefinitivUUID")
                                      , debug = FALSE
                                      ){
  #Check operation
  if(is.null(operation) || !is.character(operation) || length(operation) > 1 ){
    stop("Parameter operation can not be null but should be one of 'CREATE','GET', 'UPDATE', 'DELETE'")
  }

  if(!(toupper(operation) %in% c("CREATE","GET", "UPDATE", "DELETE"))){
    stop("operation should be one of 'CREATE','GET', 'UPDATE', 'DELETE' but is ")
  }

  #check UUID
  if(is.null(UUID)){
    stop("Parameter UUID has to be set can be found here: Eikon Terminal --> help --> about --> user details: UUID e.g. ABCDE-123456")
  }

  #Inspect
  if(!is.null(holidays) && !is(holidays,"Refinitiv_holidays")){
    stop("Parameter holidays is not defined correctly use function CustomInstrumentHolidayBuilder() to build a correct holidays element")
  } else if(!is.null(holidays)){
    holidays <- structure(holidays, class = "list")
  }

  if(!is.null(basket) && !is(basket,"Refinitiv_basket")){
    stop("Parameter basket is not defined correctly use function CustomInstrumentBasketBuilder() to build a correct basket")
  } else if(!is.null(basket)){
    basket <- structure(basket, class = "list")
  }

  if(!is.null(udc) && !is(udc,"Refinitiv_udc")){
    stop("Parameter udc is not defined correctly use function CustomInstrumentUDCBuilder() to build a correct udc")
  } else if(!is.null(udc)){
    udc <- structure(udc, class = "list")
  }


  # "CREATE", "GET", "UPDATE", "DELETE"
  #for GET, DELETE
  type <- NULL
  if(operation %in% c("GET", "DELETE")){
    type <- formula <- basket <- udc <- NULL
  }

  if(!(sum((c(!is.null(formula), !is.null(basket), !is.null(udc)))) <  2)){
      stop("Only ONE single type of formula, basket, or user definined continuation (udc) has to be supplied")
    } else if(!is.null(formula)){
      type <- "formula"
    } else if(!is.null(basket)){
      type <- "basket"
      if(is.null(currency)){
        stop("Parameter currency is required for baskets")
      }
    } else if(!is.null(udc)){
      type <- "udc"
    }


  #check symbol and if not correct it
  symbol <- CorrectCustomInstrument(symbol = symbol, UUID = UUID)

  if(toupper(operation) %in% c("CREATE")){
    message(paste0("Creating Custom Instrument: ", symbol))

    #1 first check if instrument is not defined already

    AlreadyExists <- RDObject$manage_custom_instrument(operation = "GET"
                                                      , symbol = symbol)


    if(!is.null(AlreadyExists$state$code)){
      Request <- RDObject$create_custom_instrument( symbol = symbol
                                           , formula = formula
                                           , type = type
                                           , basket = basket
                                           , udc = udc
                                           , currency = currency
                                           , instrumentName = instrumentName
                                           , exchangeName = exchangeName
                                           , holidays = holidays
                                           , timeZone = timeZone
                                           , description = description
                                           , debug = debug
                                           )
    } else {
      stop("Instrument is already be created and should be deleted or updated first if you want to make changes")
    }




  } else if(toupper(operation) %in% c('GET', "DELETE", "UPDATE")){

    AlreadyExists <- RDObject$manage_custom_instrument(operation = "GET"
                                                       , symbol = symbol)


    if(is.null(AlreadyExists$state$code)){
      if(toupper(operation) == "DELETE"){
        Request <- RDObject$manage_custom_instrument( operation = operation
                                                    , symbol = symbol)
        message(paste("Custom Instrument",symbol,"deleted"))
        Request <- NULL
      } else if(toupper(operation) == "UPDATE"){
        Request <- RDObject$manage_custom_instrument( operation = operation
                                                      , symbol = symbol
                                                      , formula = formula
                                                      , type = type
                                                      , basket = basket
                                                      , udc = udc
                                                      , currency = currency
                                                      , instrumentName = instrumentName
                                                      , exchangeName = exchangeName
                                                      , holidays = holidays
                                                      , timeZone = timeZone
                                                      , description = description
                                                      , debug = debug
                                                      )
      } else {
        Request <- AlreadyExists
      }
    } else {
      if(toupper(operation) == "GET"){
        stop("Instrument did not exist and can therefore not be obtained")
      } else if(toupper(operation) == "UPDATE"){
        stop("Instrument did not exist and can therefore not be updated")
      } else {
        stop("Instrument did not exist and can therefore not be deleteted")
  }}
}

  return(Request)





}


#' Show all available custom instruments that have been created
#'
#' @param RDObject Refinitiv Data connection object, defaults to RefinitivJsonConnect()
#' @param debug show api calls defaults to false
#'
#' @return a list of custom Instruments created with all parameters
#' @export
#'
#' @examples
#' \dontrun{
#' test <- rd_SearchCustomInstruments()
#' }
rd_SearchCustomInstruments <- function(RDObject = RefinitivJsonConnect(), debug = TRUE){

  Request <- RDObject$search_custom_instrument( debug = debug)
  return(Request)

}

# Streaming custom instruments

# streaming get default websocket
#https://api.refinitiv.com/streaming/custom-instruments/v1/resource
# access-control-allow-origin	*
# access-control-expose-headers	*
#   content-length	189
# content-type	application/json
# date	Fri, 08 Sep 2023 14:52:44 GMT
# x-amzn-trace-id   #	Root=1-64fb353c-0b645213093cdb82506fd4a8
# x-served-by	      # region=eu-west-1; cid=a98ca41813154e3a99688ecf3661a965
# x-tr-requestid	  #  c648a370-e475-43a8-a5dd-b70f6458ccf0
#https://github.com/rstudio/websocket

# {
#   "services": [
#     {
#       "provider": "AWS",
#       "endpoint": "emea-custom-instruments.extranet.refinitiv.biz/websocket",
#       "transport": "websocket",
#       "port": 443,
#       "dataFormat": [
#         "tr_json2"
#       ],
#       "location": [
#         "eu-west-1"
#       ]
#     }
#   ]
# }
#
#
# ws$send("hello")















































