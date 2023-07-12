#' The get_history function allows you to retrieve pricing history, as well as Fundamental and Reference data history through a single function call.
#'
#' @param RD Refinitiv data object (currently only RDconnect())
#' @param universe Instruments to request	str or vector
#' @param fields Fields to request	str or vector
#' @param parameters a named key value list for setting parameters, Default: NULL
#' @param interval Date interval. Supported intervals are: ["minute", "1min", "5min", "10min", "30min", "60min", "hourly", "1h", "daily", "1d", "1D", "7D", "7d", "weekly", "1W", "monthly", "1M", "quarterly", "3M", "6M", "yearly", "12M", "1Y"]
#' @param start The start date and timestamp of the requested history	str, date
#' @param end The end date and timestamp of the requested history	str, date
#' @param adjustments Tells the system whether to apply or not apply CORAX (Corporate Actions) events or exchange/manual corrections or price and volume adjustment according to trade/quote qualifier summarization actions to historical time series data. Possible values are ["exchangeCorrection", "manualCorrection", "CCH", "CRE", "RTS", "RPO", "unadjusted", "qualifiers"]
#' @param count The maximum number of data points returned. Values range: 1 - 10000
#' @param use_field_names_in_headers boolean 	If True - returns field name as column headers for data instead of title
#'
#'
#' @return data.frame
#' @export
#'
#' @details
#'
#'  This function is currently work in progress and only works with RDConnect (python), directJson is not available for this function.
#'
#'  #section regarding adjustments parameters:
#'
#'  The vector of adjustment types (comma delimiter) that tells the system whether
#'  to apply or not apply CORAX (Corporate Actions) events or
#'  exchange/manual corrections to historical time series data.
#'
#'  The supported values of adjustments :
#'\itemize{
#'  \item{"unadjusted"}   {Not apply both exchange/manual corrections and CORAX}
#'  \item{"exchangeCorrection"}   {Apply exchange correction adjustment to historical pricing}
#'  \item{"manualCorrection"}   {Apply manual correction adjustment to historical pricing i.e. annotations made by content analysts}
#'  \item{"CCH"}  {Apply Capital Change adjustment to historical Pricing due to Corporate Actions e.g. stock split}
#'  \item{"CRE"}  {Apply Currency Redenomination adjustment when there is redenomination of currency}
#'  \item{"RPO"}  {Apply Reuters Price Only adjustment to adjust historical price only not volume}
#'  \item{"RTS"}  {Apply Reuters TimeSeries adjustment to adjust both historical price and volume}
#'  \item{"qualifiers"}   {Apply price or volume adjustment to historical pricing according to trade/quote qualifier summarization actions}
#' }
#'
#'
#' @examples
#' \dontrun{
#' RDConnect("your api key here")
#' timeseries1 <-  rd_GetHistory(universe=c("AAPL.O", "NVDA.O"))
#' timeseries2 <- rd_GetHistory(universe="GOOG.O"
#'                             ,fields = c("BID", "ASK"),interval="tick",count=5)
#'
#' test <- rd_GetHistory(universe= "AAPL.O"
#'                      , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)"
#'                        ,"TR.FreeFloatPct()/100/*FreefloatWeight*/"
#'                        ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/"
#'                        ,"TR.CLOSEPRICE(Adjusted=0)/*close*/")
#'                      , parameters = list("Curn" = "USD"
#'                      , "SDate" = "2020-10-27", "EDate" = "2020-12-01"))
#'
#' test <- rd_GetHistory(universe = c("GOOG.O","AAPL.O")
#'                        , fields = c("TR.Revenue","TR.GrossProfit")
#'                        , parameters = list("SDate" = "0CY", "Curn" = "CAD"))
#' test <-  rd_GetHistory(universe = c("GOOG.O","AAPL.O")
#'                       , fields = c("TR.PriceTargetMean(SDate:0CY)","TR.LOWPRICE(SDate:0d)"))
#'
#'
#' test <- rd_GetHistory( universe = c("GOOG.O","MSFT.O","FB.O","AMZN.O")
#'                      ,fields = c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
#'                      ,parameters = list("Scale" = 6,"SDate" = 0
#'                      ,"EDate" = -3,"FRQ" = "FY", "Curn" = "EUR"))
#' }
rd_GetHistory <- function(RD = RDConnect() #RefinitivJsonConnect() #
                         , universe = NULL
                         , fields = NULL
                         , parameters = NULL
                         , interval = NULL
                         , start = NULL
                         , end = NULL
                         , adjustments = NULL
                         , count = NULL
                         , use_field_names_in_headers = NULL
                         ){

  #Check if universe is supplied
  if(is.null(universe)){
    stop("Parameter universe should be supplied and is not")
  }

  #
  #
  # # Divide RICS in chunks to satisfy api limits
  # if(!is.null(fields)){
  #    ChunckFields <- fields
  # } else {
  #   ChunckFields <- rep(x = "dummy", times =14)
  # }
  #
  # ChunckedRics <- EikonChunker( RICS = universe, Eikonfields = ChunckFields
  #                             , MaxCallsPerChunk = 10000)
  #
  #   #
  # # }
  # #
  # # EikonDataList <- as.list(rep(NA, times = length(ChunckedRics)))
  # #
  # DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
  #                                  , succes =  rep(FALSE, length(ChunckedRics))
  #                                  , retries = rep(0L, length(ChunckedRics), stringsAsFactors = FALSE)
  #                                  )

  #Build Argument list
  if(!exists("Arglist") || identical(list(),Arglist)){
    Arglist <- as.list(match.call(expand.dots=FALSE))
    Arglist[[1]] <- NULL
    # Arglist[["universe"]] <- NULL
  }

  # PyCallList <- vector(mode = "list", length = length(ChunckedRics))

  # PyCallList <- lapply(X = 1:length(ChunckedRics)
                      # , FUN = function(x, Arglist, ChunckedRics){
                              # Arglist[["universe"]] <- ChunckedRics[[x]]
                              # return(Arglist)}
        # , Arglist = Arglist
        # , ChunckedRics = ChunckedRics
        # )

  #Make sure all arguments are evaluated before passing to the gethistory api
  Arglist <- lapply(X = Arglist, FUN = function(x){eval(x, envir=sys.frame(-3))})


  if("count" %in% names(Arglist)){
    if (count <= 0){
      stop("count should be integer > 0")
    } else {
      Arglist$count <- as.integer(Arglist$count)
    }
  }

  # remove RDP from arglist if this is in it.
  if("RD" %in% names(Arglist)){
    Arglist$RD <- NULL
  }

  #Execute get_history
  PyCall <- do.call(what = RD[["get_history"]], args = Arglist)
  ColumnLevels <- reticulate::py_to_r(PyCall$columns$nlevels)
  if(ColumnLevels == 1){
    VariableName <- PyCall$columns$name
    if(identical(as.character(VariableName) ,universe)){
      PyCall$insert(0L, "Instrument", VariableName)
      id_vars <- c("Date", "Instrument")
      var_name = "variable"
    } else {
      PyCall$insert(0L, "variable", VariableName)
      id_vars <- c("Date", "variable")
      var_name <- "Instrument"
    }
    DroppedIndex <- PyCall$reset_index()
    DroppedIndex <- DroppedIndex$melt(ignore_index = TRUE
                                     , id_vars = id_vars
                                     , var_name = var_name)

  } else if(ColumnLevels == 2){
    Melted <- PyCall$melt(ignore_index = FALSE
                         , var_name = c("Instrument", "variable"))
    DroppedIndex <- Melted$reset_index()

  }
  JsonString <- DroppedIndex$to_json(date_format= "iso")
  Converted <- Process_RDP_output(JsonString, RemoveNA = TRUE)
  return(Converted)
}
