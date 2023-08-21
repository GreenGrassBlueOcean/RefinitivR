#' GetHistoricalPricing
#'
#' @param EikonObject connection object, defaults to RefinitivJsonConnect()
#' @param universe The entity universe e.g. RIC name
#' @param interval The consolidation interval in ISO8601. see also details
#' @param start The start date and timestamp of the query. see also details
#' @param end The end date and timestamp of the query. see also details
#' @param adjustments character vector: The list of adjustment types (comma delimiter) that tells the system whether to apply or not apply CORAX (Corporate Actions) events or exchange/manual corrections to historical time series data. see also details
#' @param count integer The maximum number of data returned
#' @param fields The comma separated list of fields that are to be returned in the response (only interday)
#' @param sessions The list of market session classification (comma delimiter) that tells the system to return historical time series data based on the market session definition (market open/market close)
#'
#' @return data.frame with result
#' @export
#'
#'
#' @details
#'
#' Additional details on parameters:
#'
#' ## \strong{Interval}:
#' The support intervals are:
#' \itemize{
#'  \item{Intraday Summaries Intervals}{ PT1M, PT5M, PT10M, PT30M, PT60M, and PT1H}
#'  \item{Interday Summaries Tntervals}{ P1D, P7D, P1W, P1M, P3M, P12M, and P1Y.}
#' }
#' When interval is not specified, backend will return the lowest supported interday interval.
#'
#' ## \strong{start} & \strong{end}:
#'\itemize{
#'  \item{\strong{Intraday Summaries Interval}}{
#'    \itemize{
#'    \item{Local time is not supported}{}
#'    \item{This parameter support time up to nanoseconds granularity.
#'          For more details on minute summaries boundary
#'          , see "Minute Summaries Boundary" in Readme.}{}
#'    \item{See more details on "Start / End / Count Behavior" in Readme.}{}
#' }
#'  }
#'  \item{\strong{Interday Summaries Tntervals}{
#'    \itemize{
#'    \item{The start/end of the query is in ISO8601 with local date only e.g 2018-01-01. If the time is supplied, it will be ignored.}{}
#'    \item{See more details on "Start / End / Count Behavior" in Readme.}{}
#' }}}}
#'
#'## \strong{adjustments}:
#' The list of adjustment types (comma delimiter) that tells the system whether to apply or not apply CORAX (Corporate Actions)
#' events or exchange/manual corrections to historical time series data.
#' \itemize{
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
#' \itemize{
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
#' \itemize{
#'  \item{1 }{Summaries data will always have exchangeCorrection and manualCorrection applied. If the request is explicitly asked for
#'           uncorrected data, a status block will be returned along with the corrected data saying "Uncorrected summaries are currently not supported".}
#'  \item{2 }{unadjusted will be ignored when other values are specified.}
#' }
#'### Limitations:
#'Adjustment behaviors listed in the limitation section may be changed or improved in the future.
#'\itemize{
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
#' \itemize{}
#' ## \strong{fields}:
#' The comma separated list of fields that are to be returned in the response. The fields value is case-sensitive, can be specified only with alphanumeric or underscore characters, and cannot be empty.
#' If the requested fields are not valid or not availale for the given RIC universe, the back-end still returns the response of the valid fields (if available)
#' together with the status block to indicate the unsupported fields. Currently, the fields attribute is only effective with interday summaries (e.g. interval=P1D/P1M).
#'
#' ## \strong{sessions}:The list of market session classification (comma delimiter) that tells the system to return historical time series data
#'                      based on the market session definition (market open/market close). This parameter is applicable to intraday summary intervals only.
#' \itemize{
#'   \item{\strong{If unspecified: }}{all data within the query range will be returned without taking market session definition into consideration}
#'   \item{\strong{If specified: }}{only data from specific market session classification within the query range will be returned.}
#' }
#'
#'
#' @examples
#' \dontrun{
#' Vodafone <- rd_GetHistoricalPricing(universe = "VOD.L", interval = "P1D", count = 20L
#'                                    , fields =c("BID","ASK","OPEN_PRC","HIGH_1","LOW_1","TRDPRC_1","NUM_MOVES","TRNOVR_UNS") )
#' }
rd_GetHistoricalPricing <- function( EikonObject = RefinitivJsonConnect()
                                , universe
                                , interval = "daily"
                                , start = NULL
                                , end = NULL
                                , adjustments = NULL
                                , count = 20L
                                , fields = NULL
                                , sessions = NULL
                                ){

  # Make sure that Python object has api key and change timeout
  if(!("get_historical_pricing"  %in% names(EikonObject))){
    stop("historical pricing is only available when RefinitivJsonConnect is used as EikonObject")
  }

  try(EikonObject$set_app_key(app_key = .Options$.EikonApiKey), silent = TRUE)

  #In case no rics are supplied return nothing
  if(is.null(universe)){
    warning("no rics are supplied to GetHistoricalPricing")
    return(data.frame())
  }

  ChunckedRics <- universe

  TimeSeriesList <- as.list(rep(NA, times = length(ChunckedRics)))

  DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
                                    , succes =  rep(FALSE, length(ChunckedRics))
                                      , retries = rep(0L, length(ChunckedRics), stringsAsFactors = FALSE)
  )

  while (!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)  ) {

    ChunckedRicsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

     for (j in ChunckedRicsTryList) {
        TimeSeriesList[[j]] <- try({

          retry(
          EikonObject$get_historical_pricing( universe = ChunckedRics[[j]]
                                                  , interval = interval
                                                  , start = start
                                                  , end = end
                                                  , adjustments = adjustments
                                                  , count = count
                                                  , fields = fields
                                                  , sessions = sessions)


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

   return(data.table::setDF(data.table::rbindlist(TimeSeriesList, use.names = T, fill = T)))
}
