#' GetHistoricalPricing
#'
#' get historical timeseries from the Refinitiv API
#'
#' @param RDObject connection object, defaults to RefinitivJsonConnect()
#' @param universe The entity universe e.g. RIC name
#' @param interval The consolidation interval in ISO8601. defaults to P1D, see also details
#' @param start The start date and timestamp of the query. see also details
#' @param end The end date and timestamp of the query. see also details
#' @param adjustments character vector: The list of adjustment types (comma delimiter) that tells the system whether to apply or not apply CORAX (Corporate Actions) events or exchange/manual corrections to historical time series data. see also details
#' @param count integer The maximum number of data returned
#' @param fields The comma separated list of fields that are to be returned in the response (only interday)
#' @param sessions The list of market session classification (comma delimiter) that tells the system to return historical time series data based on the market session definition (market open/market close)
#' @param debug boolean, if TRUE prints url of get requests
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is NULL
#'
#' @return data.frame with result
#' @export
#'
#' @seealso [translate_to_iso8601_duration()] for translation from eikon legacy interval like e.g. 'daily' to 'P1D'
#' @seealso [mapEikonTimefieldsToRd()] for translation of eikon timeseries field names to RD field names.
#'
#'
#' @details
#'
#' Additional details on parameters:
#'
#'## \strong{RDObject}:
#' The support connection objects are:
#' \describe{
#'  \item{JSON:}{ RefinitivJsonConnect}
#'  \item{refinitiv.data:}{ RDConnect()}
#' }
#'
#' ## \strong{Interval}:
#' The support intervals are:
#' \describe{
#'  \item{Intraday Summaries Intervals}{ PT1M, PT5M, PT10M, PT30M, PT60M, and PT1H}
#'  \item{Interday Summaries Tntervals}{ P1D, P7D, P1W, P1M, P3M, P12M, and P1Y.}
#' }
#' When interval is not specified, back-end will return the lowest supported interday interval.
#'
#' ## \strong{start} & \strong{end}:
#'\describe{
#'  \item{\strong{Intraday Summaries Interval}}{ }
#'    \itemize{
#'    \item Local time is not supported
#'    \item This parameter support time up to nanoseconds granularity.
#'          For more details on minute summaries boundary
#'          , see "Minute Summaries Boundary" in Readme.
#'    \item See more details on "Start / End / Count Behavior" in Readme.
#' }
#'
#'  \item{\strong{Interday Summaries Tntervals}}{ }
#'    \describe{
#'    \item{The start/end of the query is in ISO8601 with local date only e.g 2018-01-01. If the time is supplied, it will be ignored.}{ }
#'    \item{See more details on "Start / End / Count Behavior" in Readme.}{ }
#' }}
#'
#'## \strong{adjustments}:
#' The list of adjustment types (comma delimiter) that tells the system whether to apply or not apply CORAX (Corporate Actions)
#' events or exchange/manual corrections to historical time series data.
#' \describe{
#' \item{\strong{If unspecified: }}{the response will be controlled by each back-end service with the proper adjustments  in the response
#'                                so that the clients know which adjustment types are applied by default.
#'                                In this case, the returned data will be applied with exchange
#'                                and manual corrections as well as being applied with CORAX adjustments.}
#' \item{\strong{If specified: }}{it means that the clients want to get some specific adjustment types applied or even unadjusted.}
#' }
#'
#' Normally, the back-end should strictly serve what clients need. However, if the back-end cannot support them,
#' back-end can still return the form that the back-end supports with the proper adjustments
#' in the response together with status block (if applicable) instead of an error message.
#' The supported values of adjustments :
#' \describe{
#'  \item{unadjusted }{Not apply both exchange/manual corrections and CORAX}
#'  \item{exchangeCorrection }{Apply exchange correction adjustment to historical pricing}
#'  \item{manualCorrection }{Apply manual correction adjustment to historical pricing i.e. annotations made by content analysts}
#'  \item{CCH }{Apply Capital Change adjustment to historical Pricing due to Corporate Actions e.g. stock split}
#'  \item{CRE }{Apply Currency Redenomination adjustment when there is redenomination of currency}
#'  \item{RPO }{Apply Reuters Price Only adjustment to adjust historical price only not volume}
#'  \item{RTS }{Apply Reuters TimeSeries adjustment to adjust both historical price and volume}
#' }
#'
#'
#' ### Notes:
#' \describe{
#'  \item{1 }{Summaries data will always have exchangeCorrection and manualCorrection applied. If the request is explicitly asked for
#'           uncorrected data, a status block will be returned along with the corrected data saying "Uncorrected summaries are currently not supported".}
#'  \item{2 }{unadjusted will be ignored when other values are specified.}
#' }
#'### Limitations:
#'Adjustment behaviors listed in the limitation section may be changed or improved in the future.
#'\describe{
#' \item{1 }{In case of any combination of correction types is specified (i.e. exchangeCorrection or manualCorrection), all correction types will be applied to data in applicable event types.}
#' \item{2 }{In case of any combination of CORAX is specified (i.e. CCH, CRE, RPO, and RTS), all CORAX will be applied to data in applicable event types.}
#'}
#'
#' ## \strong{count}:
#' The maximum number of data returned. If count is smaller than the total amount of data of the time range specified, some data (the oldest)
#' will not be delivered. To retrieve all available data with in the time range specified, this parameter should not be specified.
#' The returned data could be less than the number requested if there are not enough data with in the time range specified. If not specified,
#' count will default to 20 unless both the start and end parameters are also specified.
#' This parameter has no maximum limit for Interday summaries interval. The minimum value for this parameter is 1.
#' Negative value is not supported. See more details on "Start / End / Count Behavior" in Readme.
#' \describe{
#' ## \strong{fields}:
#' The comma separated list of fields that are to be returned in the response. The fields value is case-sensitive, can be specified only with alphanumeric or underscore characters, and cannot be empty.
#' If the requested fields are not valid or not availale for the given RIC universe, the back-end still returns the response of the valid fields (if available)
#' together with the status block to indicate the unsupported fields. Currently, the fields attribute is only effective with interday summaries (e.g. interval=P1D/P1M).
#'
#' ## \strong{sessions}:The list of market session classification (comma delimiter) that tells the system to return historical time series data
#'                      based on the market session definition (market open/market close). This parameter is applicable to intraday summary intervals only.
#' \describe{
#'   \item{\strong{If unspecified: }}{all data within the query range will be returned without taking market session definition into consideration}
#'   \item{\strong{If specified: }}{only data from specific market session classification within the query range will be returned.}
#' }
#'}
#'
#' @examples
#' \dontrun{
#' # run with python refinitiv data
#' Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D"
#' , count = 20L, RDObject = RDConnect(PythonModule = "RD"))
#'
#' # run with r json
#' Vodafone2 <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D"
#' , count = 20L, RDObject = RDConnect(PythonModule = "JSON"))
#'
#' identical(Vodafone, Vodafone2)
#'
#' # run wit a subset of fields
#' Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D", count = 20L
#' , fields =c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS") )
#'
#'
#' # test for interday
#'
#' Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "PT1M", count = 20L
#' , RDObject = RefinitivJsonConnect())
#'
#'  # 1 minute - Count - All Sessions
#'  Vodafone <- rd_GetHistoricalPricing( universe = c("VOD.L", "AAPL.O")
#'                                     , interval = "PT1M", count = 500L
#'                                     , sessions= c("pre","normal","post")
#'                                     , RDObject = RefinitivJsonConnect())
#'
#'
#'  # test with custom instrument you need to construct a custom instrument first
#'  # intraday
#'  Vodafone <- rd_GetHistoricalPricing( universe = "S)lseg_epam4.ABCDE-123456"
#'  , interval = "P1D", count = 20)
#'
#'  # interday
#'  Vodafone <- rd_GetHistoricalPricing( universe = "S)lseg_epam4.ABCDE-123456"
#'  , interval = "PT1M", count = 500L)
#'
#'
#' }
rd_GetHistoricalPricing <- function( RDObject = RefinitivJsonConnect()
                                   , universe = NULL
                                   , interval = "P1D"
                                   , start = NULL
                                   , end = NULL
                                   , adjustments = NULL
                                   , count = 20L
                                   , fields = NULL
                                   , sessions = NULL
                                   , debug = FALSE
                                   , SpaceConvertor = "."
                                   ){

  #In case no rics are supplied return nothing
  if(is.null(universe)){
    warning("no rics are supplied to GetHistoricalPricing")
    return(data.frame())
  }

  AllowedIntervals <- c("PT1M", "PT5M", "PT10M", "PT30M", "PT60M", "PT1H"
                        ,"P1D", "P7D", "P1W", "P1M", "P3M", "P12M", "P1Y")

  if(!(interval %in% AllowedIntervals)){
    stop(paste("Interval is", interval, "but can only be one of", paste0("'", AllowedIntervals, "'" , collapse = ", " )))
  }

  # start api call
  force(RDObject)

  if(!(getOption(".RefinitivPyModuleName")  %in% c("refinitiv.data", "JSON"))){
    stop("historical pricing is only available when JSON --> RefinitivJsonConnect() or Python Refinitiv data --> RDConnect() is used as RDObject")
  }

  # Make sure that Python object has api key and change timeout
  try(RDObject$set_app_key(app_key = .Options$.EikonApiKey), silent = TRUE)

  ChunckedRics <- universe
  TimeSeriesList <- as.list(rep(NA, times = length(ChunckedRics)))
  DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
                                   , succes =  rep(FALSE, length(ChunckedRics))
                                   , retries = rep( 0L, length(ChunckedRics)
                                                  , stringsAsFactors = FALSE
                                                  )
                                   )

  while (!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)  ) {

    ChunckedRicsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

     for (j in ChunckedRicsTryList) {
        TimeSeriesList[[j]] <- try({

          retry(
          if(getOption(".RefinitivPyModuleName") =="JSON"){
            if(CheckifCustomInstrument(ChunckedRics[[j]]) %in% c(FALSE, NA)){
               Request <- RDObject$get_historical_pricing( universe = ChunckedRics[[j]]
                                                             , interval = interval
                                                             , start = start
                                                             , end = end
                                                             , adjustments = adjustments
                                                             , count = count
                                                             , fields = fields
                                                             , sessions = sessions)

            } else {
               if(interval %in% c("PT1M", "PT5M", "PT10M", "PT30M", "PT60M", "PT1H")){
                 #intraday
                 Request <- RDObject$get_intraday_custominstrument_pricing( universe = ChunckedRics[[j]]
                                                                , interval = interval
                                                                , start = start
                                                                , end = end
                                                                , adjustments = adjustments
                                                                , count = count
                                                                , fields = fields
                                                                , sessions = sessions)

               } else if(interval %in% c("P1D", "P7D", "P1W", "P1M", "P3M", "P12M", "P1Y")){
                 #interday
                 Request <- RDObject$get_interday_custominstrument_pricing( universe = ChunckedRics[[j]]
                                                                               , interval = interval
                                                                               , start = start
                                                                               , end = end
                                                                               , adjustments = adjustments
                                                                               , count = count
                                                                               , fields = fields
                                                                               , sessions = sessions)

               } else {
                stop("the supplied interval cannot be used for this custom instrument")
              }
            }
            Request[[1]]
          } else {
            Request <- {RDObject$content$historical_pricing$summaries$Definition(universe = ChunckedRics[[j]]
                                                         , interval = interval
                                                         , start = start
                                                         , end = end
                                                         , adjustments = adjustments
                                                         , count = count
                                                         , fields = fields
                                                         , sessions = sessions)}


          Request <- Request$get_data()
          PyJsonConvertor(Request$data$raw)
          # reticulate::py_to_r(Request$data$raw)
          }
           )

      })
       Sys.sleep(time = 0.5)
       if (!identical(TimeSeriesList[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE }
       # if(verbose){
         message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
       # }
     }

     DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1
   }
   if(any(DownloadCoordinator$retries > 4L)){
     warning("EikonGetTimeseries downloading data failed for one or more Rics")
   }
  # Process request and build return data.frame using data.table ----
   return_DT <- data.table::rbindlist(lapply( X =  TimeSeriesList
                                            , FUN =  rd_OutputProcesser
                                            , use_field_names_in_headers = FALSE, NA_cleaning = FALSE)
                                      , use.names = TRUE, fill = TRUE )

  data.table::setnames(return_DT, new = EikonNameCleaner( names = names(return_DT)
                                                        , SpaceConvertor = SpaceConvertor))
  return(data.table::setDF(return_DT))
}


#' print list for warnings
#'
#' @param list the list to be printed
#' @param hn print only first elements of error list, defaults to 6
#'
#' @return message on the terminal
#' @keywords internal
#' @noRd
#'
#' @examples
#' ListForPrint <- list( list( code = 218L, col = 1L
#' , message = "The formula must contain at least one field or function.", row = 0L)
#'     , list( code = 218L, col = 1L
#' , message = "The formula must contain at least one field or function.", row = 1L)
#'     )
#' printList(list= ListForPrint)
printList <- function(list, hn = 6) {

  for (item in 1:length(list)) {
    message("\n", names(list[item]), ":\n")
    message(head(list[[item]], hn), digits = 3)

  }
}



#' Process output from refintiv data to r data.frame output
#'
#' @param x refinitiv data platform output
#' @param use_field_names_in_headers boolean wheater or not to return titles of field (formulas) as headers
#' @param NA_cleaning clean NA in return data
#'
#' @importFrom data.table `.SD`
#'
#' @return data.frame
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  EndPoint = "data/datagrid/beta1/"
#'  payload <- list( 'universe'= as.list(c("GOOG.O", "NVDA.O"))
#'                , 'fields'= as.list(c('TR.CLOSE', 'TR.OPEN'))
#'                , 'parameters'=list('SDate'= '2022-10-05', 'EDate'= '2022-11-05')
#'                , 'output'= 'Col,T|Va,Row,In,date|'
#'                )
#'
#' response <- send_json_request(json = payload, service = "rdp"
#' , EndPoint = EndPoint, request_type = "POST")
#' Output <- rd_OutputProcesser(response)
#' }
rd_OutputProcesser <- function(x, use_field_names_in_headers = TRUE, NA_cleaning = TRUE, SpaceConvertor = NULL){


  #check errors
  if("error" %in% names(x)){
    warning("The following errors where returned from the LSEG API:")
    try(warning(printList(x$error)))
  }

  #check input
  if(!"data" %in% names(x)){
    message(x)
    return(data.table::data.table())
  } else if(identical(x$data,list())){
    message(paste(x$universe, "returned empty list"))
    return(data.table::data.table())
  }

  # bind rows
  CleanedData <- replaceInList(x$data, function(x)if(is.null(x) || identical(x,"") )NA else x)
  return_DT <- data.table::rbindlist(CleanedData)

  # Select the proper column names
  if(!is.null(use_field_names_in_headers) && !use_field_names_in_headers && "title" %in% names(x$headers[[1]])){
    Selectedheader <- "title"
  }  else if(!is.null(use_field_names_in_headers) && ("displayName" %in%  names(unlist(x$headers)))){ #[[1]]
    Selectedheader <- "displayName"
  }   else {
    Selectedheader <- "name"
  }

  headernames <- JsonHeaderAnalyzer(x, Selectedheader = Selectedheader, use_field_names_in_headers = use_field_names_in_headers)

  # set column names
  data.table::setnames(x = return_DT, new = headernames)


  # add universe
  if(!("universe" %in% tolower(names(return_DT)) | "instrument" %in% tolower(names(return_DT))) && ("universe" %in% names(x)) ){
    universe <- NULL
    return_DT <- return_DT[, universe := x$universe]
    data.table::setcolorder(return_DT,c("universe"))
  }

  # check for casus with unnamed columns
  if(all(c("V1","V2") %in% names(return_DT))){
     DateValueCol <- names(return_DT)[sapply(return_DT, function(col) "date Value" %in% col)]

    # Set the values in the identified column to NA
    return_DT[, (DateValueCol) := NA]
    data.table::setnames(return_DT, old = DateValueCol, new = "Date")


    # Find the names of columns that start with "V"
    if(!("Instrument" %in% names(return_DT))){
      remaining_v_columns <- grep("^V", names(return_DT), value = TRUE)
      data.table::setnames(x = return_DT, old = remaining_v_columns, new = "Instrument" )
    }
  }


  #Check if there are other fields that should be dates
  OtherDateColumns <- grep(pattern = ".date$|\\bdate\\b", x = tolower(names(return_DT)))
  if(!identical(OtherDateColumns, integer(0) )){

    Columnclasses <- lapply(return_DT, class)
    DateType <-  Columnclasses[OtherDateColumns] |> unlist() |> as.vector() |> unique()
    if(identical(DateType, "numeric")){
       return_DT[, (OtherDateColumns) := lapply(.SD, function(x){as.POSIXct(x/1000, origin="1970-01-01", tz = "UTC")}), .SDcols = OtherDateColumns]
    } else {
      return_DT[, (OtherDateColumns) := lapply(.SD, lubridate::as_date), .SDcols = OtherDateColumns]
    }
  }
  data.table::setnames(return_DT, old = c("date", "DATE"), new = c("Date", "Date"), skip_absent = TRUE)

  #Na cleaning
  if(NA_cleaning){
    for (i in seq_along(return_DT)){
      data.table::set( return_DT
                     , i = which(is.na(return_DT[[i]]))
                     , j = i, value=FALSE)
    }
  }


  if(!is.null(SpaceConvertor)){
  data.table::setnames( return_DT
                        , new = EikonNameCleaner( names = names(return_DT)
                                                  , SpaceConvertor = SpaceConvertor))
  }

  return(return_DT)

}
