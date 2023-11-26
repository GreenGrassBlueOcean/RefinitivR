#' A postprocessor to process Eikon (python) get_Data into r data.frames
#'
#' As the Python eikon get_data structure contains various list that are null these should be replaced
#' with NA to prevent disasters when later handling the data.frame. For example when using the unlist function Null elements
#' are removed from the lists causing shorter vectors than expected.
#'
#' @param Eikon_get_dataOuput a single Eikon$get_data result or a list of Eikon$get_data results.
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is "."
#'
#' @return a list of a data.frame with cleaned output and the resulting error data.frame
#' @keywords internal
#'
#' @seealso EikonNameCleaner
#'
#' @examples
#' \dontrun{"internal function no examples"}
EikonPostProcessor <- function(Eikon_get_dataOuput, SpaceConvertor = "."){

  #0. helper functions ----

  getData <- function(data, requestnumber, SpaceConvertor) {
    #1. Remove NULL values and replace with NA in nested list

    data[[requestnumber]][["data"]] <- replaceInList(data[[requestnumber]][["data"]], function(x)if(is.null(x) || identical(x,""))NA else x)

    #2. put list format in uniform way (don't mix up lists and vectors in one nested list)

    data[[requestnumber]][["data"]] <- flattenNestedlist(data[[requestnumber]][["data"]])

    if (length(data[[requestnumber]][["data"]]) > 1 ) {
      RequestData <- data.table::rbindlist(data[[requestnumber]][["data"]])
    } else {
      RequestData <- data[[requestnumber]][["data"]][[1]]
    }

    Requestheaders <- EikonNameCleaner(getheaders(data, requestnumber), SpaceConvertor = SpaceConvertor)
    data.table::setnames(RequestData, Requestheaders)

    return(RequestData)
  }

  getheaders <- function(data, requestnumber){
    #replace null headers with NA headers
    data[[requestnumber]][["headers"]] <- replaceInList(data[[requestnumber]][["headers"]], function(x)if(is.null(x) || identical(x,"") )NA else x)

    unlist(lapply( X = 1:length(data[[requestnumber]][["headers"]][[1]])
                   , FUN = function(x,data,requestnumber){data[[requestnumber]][["headers"]][[1]][[x]][["displayName"]] }
                   , data = data
                   , requestnumber = requestnumber))

  }


    #1. Check Input

  if(identical(Eikon_get_dataOuput,list(NULL))) {
    return(list( "PostProcessedEikonGetData" = data.frame()
                 , "Eikon_Error_Data" = data.frame()))
  }

  #1. main program ----
  RequestData <- RequestError <- vector(mode = "list", length = length(Eikon_get_dataOuput))
  RequestData <- lapply( X = 1:length(Eikon_get_dataOuput)
                       , FUN = function(x, data){getData(data, requestnumber=x, SpaceConvertor = SpaceConvertor)}
                       , data = Eikon_get_dataOuput
                       )

  RequestError <- lapply( X = 1:length(Eikon_get_dataOuput)
                         , FUN = function(x, data){rbindlist(data[[x]][["error"]])}
                         , data = Eikon_get_dataOuput
  )

  Eikon_Error_Data <- data.table::rbindlist(RequestError, use.names = TRUE, fill = TRUE)
  PostProcessedEikonGetData <- data.table::rbindlist(RequestData, use.names = TRUE, fill = TRUE)

  #2. Clean output ----
  .SD <- NULL
  PostProcessedEikonGetData2 <- PostProcessedEikonGetData[, lapply(.SD, function(x) replace(x, which( x == ""), NA))
                                                        ][, lapply(.SD, function(x) {if("NaN" %in% x){as.numeric(x)} else{x} })
                                                        ][, suppressWarnings(lapply(.SD, function(x){
                                                                if (is.logical(x)){as.logical(x)}
                                                                else if(all(is.na(as.numeric(x))) != TRUE){as.numeric(x)}
                                                                else {x}
                                                          }))
                                                        ]


    # return human readable names

  return(list( "PostProcessedEikonGetData" = data.table::setDF(PostProcessedEikonGetData2)
             , "Eikon_Error_Data" = data.table::setDF(Eikon_Error_Data)
  )
  )

}








#' Often operating Mics are missing from the Eikon api, this function does repair these missing operating Mics based upon an internal list of codes.
#'
#' @param Fundamentals_Data a data.frame containing at leasts the columns "RDN_EXCHD2" and "Operating MIC"
#'
#' @return the corrected data.frame in which the column "Operating MIC" empty string and NA elements are replaced with an operating MIC based on RDN_EXCHD2
#' @export
#'
#' @examples
#' \dontrun{
#' DataStream <- Refinitiv::DataStreamConnect(DatastreamUserName = DatastreamUserName,
#'                        DatastreamPassword = DatastreamPassword)
#' Stoxx1800Constits <- DataStream$listRequest(instrument = "LDJS180E",
#'                        datatype = c("RIC", "NAME"), requestDate = "0D")
#' Eikon <- Refinitiv::EikonConnect()
#' EikonDataWithFailingOPeratingMics <- EikonGetData(EikonObject = Eikon,
#'       rics = Stoxx1800Constits$RIC,
#'      Eikonformulas = c( "RDN_EXCHD2", "TR.OperatingMIC", "TR.CompanyName"))
#' EikonDataWithRepairedOPeratingMics <- EikonRepairMic(EikonDataWithFailingOPeratingMics)
#' }
EikonRepairMic <- function(Fundamentals_Data){

  if (!is.data.frame(Fundamentals_Data)) {
    stop("the input should be a data.frame containing the columns RDN_EXCHD2 and Operating MIC and it is not!")
  } else if (all(!(c("RDN_EXCHD2", "Operating MIC") %in% names(Fundamentals_Data)))) {
    args <- methods::formalArgs(EikonRepairMic)
    stop("The data.frame ",args , " presented to EikonRepairMic does not contain the columns RDN_EXCHD2 and Operating MIC and can therefore not be processed!")
  }

  #Get operating mics from package data
  MicLookup <- Refinitiv::OperatingMicLookup

  MergedwithLookup <- merge( x = Fundamentals_Data
                           , y = MicLookup
                           , by = "RDN_EXCHD2"
                           , all.x = TRUE
                           , sort = FALSE
                           )
  #Repair only if really required
  if ( any(c("", NA) %in% MergedwithLookup$`Operating.MIC`)) {
    MergedwithLookup[((MergedwithLookup$`Operating.MIC` %in% "") | is.na(MergedwithLookup$`Operating.MIC`)  ),]$`Operating.MIC` <- MergedwithLookup[((MergedwithLookup$`Operating.MIC` %in% "") | is.na(MergedwithLookup$`Operating.MIC`) ),]$repairedMIC
  }

  # Remove repaired field again before delivering back
  MergedwithLookup$repairedMIC <- NULL

  return(MergedwithLookup)
}


#' Helper function to build the Eikonformulas parameter for the EikonGetData function.
#'
#' @param Field_name string Field name to request. You can find the list in Data Item Browser.
#' @param Parameters named List containing the parameters for the field passed in the argument field_name
#' @param sort_dir string Indicate the sort direction. Possible values are \'asc\' or \'desc\'. The default value is \'asc\'
#' @param sort_priority integer Gives a priority to the field for the sorting. The highest priority is 0 (zero). The default value is NULL
#'
#' @return a list of list which can be used as import for
#' @export
#'
#' @examples
#' TR_Field(Field_name = 'tr.revenue')
#' TR_Field(Field_name ='tr.open', sort_dir ='asc', sort_priority = 1)
#' TR_Field(Field_name ='TR.GrossProfit', Parameters = list('Scale' = 6, 'Curn'= 'EUR')
#'         , sort_dir = 'asc', sort_priority = 0)
TR_Field <- function(Field_name = NULL, Parameters = NULL, sort_dir = NULL, sort_priority = NULL){

  # input checks ----------------
  #check Field_name
  if(is.null(Field_name)){
    stop("Field_name should be provided")
  }

  # check Parameters
  if(!is.null(Parameters) && (!is.list(Parameters) | is.null(names(Parameters)))){
    stop("Parameters should be a named list")
  }

  # check sort_dir
  if(!is.null(sort_dir) && ( !is.character(sort_dir) | (!((sort_dir) %in% c("asc", "desc"))))){
    stop("sort_dir parameter should be character \"asc\" or \"desc\"")
  }

  # Check sort_priority
  if(!is.null(sort_priority) && ( !is.numeric(sort_priority))){
    stop("sort_priority parameter should be integer")
  }

  # Build list -----------
  if(!is.null(Parameters)){
    FieldList <- list(list('params' = Parameters))
  } else{
    FieldList <- list(list())
  }

  if(!is.null(sort_dir))
    FieldList[[1]]  <- append(FieldList[[1]], sort_dir)

  if(!is.null(sort_priority))
    FieldList[[1]]  <- append(FieldList[[1]], sort_priority)

  names(FieldList)[1] <- Field_name

return(FieldList)
}


#' Function to process raw output of python get_symbology to better in r readable format
#'
#' @param EikonSymbologyResult nested list: output from EikonGetSymbology with option raw_output set to TRUE
#' @param from_symbol_type character use her same input as in EikonGetSymbology
#' @param to_symbol_type character use her same input as in EikonGetSymbology
#'
#' @return data.frame containing 4 columns to_symbol_type, from_symbol_type, BestMatch (as defined by Eikon), error
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' Raw_output_No_BestMatch <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
#' , to_symbol_type = "RIC" , raw_output = TRUE, bestMatch = FALSE  )
#' ProcessSymbology(EikonSymbologyResult = Raw_output_No_BestMatch
#' , from_symbol_type = "ISIN", to_symbol_type = "RIC")
#'
#' Raw_output_BestMatch <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
#' , to_symbol_type = "RIC" , raw_output = TRUE, bestMatch = TRUE  )
#' ProcessSymbology(EikonSymbologyResult = Raw_output_BestMatch
#' , from_symbol_type = "ISIN", to_symbol_type = "RIC")
#' }
ProcessSymbology <- function(EikonSymbologyResult, from_symbol_type, to_symbol_type){

  EikonSymbologyResult <- EikonSymbologyResult[[1]]$mappedSymbols
  EikonSymbologyNames <- unlist(unique(lapply(X=EikonSymbologyResult, names)))

  #replace NUll Lists with NA list when required
  EikonSymbologyResult <- lapply(EikonSymbologyResult, function(x){replaceInList(x, function(x)if(is.null(x))NA else x)})

  #1. Check Input

  if ("RICs" %in% EikonSymbologyNames){
    BestMatch <- FALSE
  } else if ( "bestMatch" %in% EikonSymbologyNames){
    BestMatch <- TRUE
    BestMatchName <- names(EikonSymbologyResult[[1]]$bestMatch)
  } else{
    stop("ProcessSymbology retrieved input in wrong format")
  }

  #2. Run main function
  EikonSymbologyResult2 <- data.table::rbindlist(EikonSymbologyResult, fill = TRUE)
  if("bestMatch" %in% names(EikonSymbologyResult2)){
    EikonSymbologyResult2$bestMatch <- as.character(EikonSymbologyResult2$bestMatch)
  }
   if(BestMatch){
     data.table::setnames(EikonSymbologyResult2, old = c("bestMatch","symbol") , new = c(BestMatchName, from_symbol_type) )

   }
   else{
     data.table::setnames(EikonSymbologyResult2, old = c("symbol") , new = c(from_symbol_type) )
   }

  #3. return output and remove lists in output
  EikonSymbologyResult3 <- data.table::as.data.table(lapply(EikonSymbologyResult2, as.character))
  return(data.table::setDF(EikonSymbologyResult3))
}



#' Replace items in nested list
#'
#' @param x list
#' @param FUN function to operate on list
#' @param ... pass through parameters
#'
#' @return list
#' @keywords internal
#'
#' @examples
#'  x <- list(list(NA, NULL, NULL), list("a", "b", "c"))
#' # test <- Refinitiv:::replaceInList(x, function(x)if(is.null(x))NA else x)
replaceInList <- function (x, FUN, ...)
{
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(replaceInList(x[[i]], FUN, ...))
    }
    x
  }
  else FUN(x, ...)
}




#' Postprocessor for raw Timeseries Requests
#'
#' @param RawTimeSeriesRequest raw return from eikon EikonGetTimeseries
#'
#' @return data.frame
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' RawTimeSeries <- try(EikonGetTimeseries( EikonObject = Eikon,
#' rics = c("MMM"),
#' start_date = "2020-01-01T01:00:00",
#' end_date = "2020-01-10T01:00:00",
#' fields = "CLOSE", raw = TRUE))1
#' PostProcessTimeSeriesRequest(RawTimeSeries)
#' }
PostProcessTimeSeriesRequest <- function(RawTimeSeriesRequest){


  GetSingleRicTimeSeries <- function(TS, ListPos){

    if( identical(TS[["timeseriesData"]][[ListPos]][["statusCode"]],  "Error")){
      message(paste0("Warning Instrument ", TS[["timeseriesData"]][[ListPos]][["ric"]],  ", error Code: "
                     , TS[["timeseriesData"]][[ListPos]][["errorCode"]],"\n"
                     , TS[["timeseriesData"]][[ListPos]][["errorMessage"]] ))


      return(NULL)
    }

    if(identical(TS[["timeseriesData"]][[ListPos]][["dataPoints"]], list()) ){
      return(NULL)
    }

    data <- data.table::rbindlist(TS[["timeseriesData"]][[ListPos]][["dataPoints"]])

    headers <-  unlist(lapply( X = 1:length(TS[["timeseriesData"]][[ListPos]][["fields"]])
                               , FUN = function(x,data){data[["timeseriesData"]][[ListPos]][["fields"]][[x]][["name"]] }
                               , data = TS))

    data.table::setnames(x = data, headers)
    rics <- TS[["timeseriesData"]][[ListPos]][["ric"]]

    Security <- Date <- TIMESTAMP <- NULL
    data <- data[ , Security := rics
    ][ , Date := as.POSIXct(TIMESTAMP, origin = "1970-01-01", tz = "GMT", format = "%FT%H:%M:%SZ")
    ][ , TIMESTAMP := NULL]

    data.table::setcolorder(data, intersect(c( "Date", "Security", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME"), names(data)))

    return(data)
  }

  TimeSeriesList <- replaceInList(RawTimeSeriesRequest, function(x)if(is.null(x) || identical(x,"") )NA else x)

  TimeSeriesRequest <- function(RequestNumber, TS){
    if(!is.null(names(TS[[RequestNumber]]))){

      Return_DT <- lapply(X  = 1:length(TS[[RequestNumber]][["timeseriesData"]])
                         , FUN = GetSingleRicTimeSeries
                         , TS = TS[[RequestNumber]]
                         )
      Return_DT <- data.table::rbindlist(Return_DT, use.names = TRUE, fill = TRUE)
    } else {return(data.table::data.table(NULL))}
  }



  ReturnTimeSeries <- lapply(X = 1:length(TimeSeriesList), FUN = TimeSeriesRequest, TS = TimeSeriesList)
  if (identical(ReturnTimeSeries[[1]], data.table::data.table(NULL))) {return(data.frame())}
  ReturnTimeSeries <- data.table::rbindlist(ReturnTimeSeries, use.names = TRUE, fill = TRUE)

  # if("VOLUME" %in% names(ReturnTimeSeries) && !is.integer(ReturnTimeSeries$VOLUME)){
  #   ReturnTimeSeries$VOLUME <- as.integer(ReturnTimeSeries$VOLUME)
  # }

  Security <- Date <- NULL
  ReturnTimeSeries <- ReturnTimeSeries[order(Security,Date)]

  ReturnTimeSeries <- data.table::setDF(ReturnTimeSeries)

  return(ReturnTimeSeries)
}



#' Preprocessor for Eikon Get timeseries to automatically chunk pieces in the required length
#'
#' This is a subfunction of EikonGetTimeseries
#'
#' @param interval character choose from  c('tick', 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly')
#' @param rics character vector containing the Reuters unique stock indentifier
#' @param start_date character start date in format "YYYY-MM-DD"
#' @param end_date character end date in format "YYYY-MM-DD"
#'
#' @return list with chunked rics
#' @seealso EikonGetTimeseries
#'
#' @keywords internal
#'
#' @examples
#' test <- Refinitiv:::EikonTimeSeriesPreprocessor(interval = "daily"
#' , rics = rep(letters, 1000), start_date = "2015-01-01", end_date = "2018-01-01")
EikonTimeSeriesPreprocessor <- function(interval, rics, start_date, end_date){

  AllowedIntervals <-  c('tick', 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly')

  if(!(interval %in% AllowedIntervals)){
    stop(paste("Parameter Interval is", interval, "but should be one of ", paste(AllowedIntervals, collapse = ", ") ))
  }


  # Build dataframe for internal lookup of names and datapoints limits
  difftimeConversionTable <- data.frame( EikonTimeName = AllowedIntervals
                                         , difftimeName = c(NA,  "mins", "hours","days", "weeks", NA, NA, NA)
                                         , limit = c(50000,50000,50000,3000,3000,3000,3000,3000)
                                         , stringsAsFactors = FALSE
  )


  #check if chunking is required
  # CalculateDuration based on weekends
  if (interval %in% c('tick')) {
    stop("Intraday tick data chunking currently not supported, maximum 50.000 data points per request")
  } else if ( interval %in% c('minute', 'hour', 'daily', 'weekly')) {
    Duration <- difftime(end_date, start_date
                         , units = difftimeConversionTable[difftimeConversionTable$EikonTimeName == interval,]$difftimeName
    )[[1]]
    # remove weekends as these need not be to downloaded, public holidays ignored
    Duration <- Duration/7*5
  } else if (interval == "monthly") {
    Duration <- (zoo::as.yearmon(end_date) - zoo::as.yearmon(start_date))*12
  } else if (interval == "quarterly") {
    Duration <- (zoo::as.yearqtr(end_date) - zoo::as.yearqtr(start_date))*4
  } else if (interval == "yearly") {
    Duration <- difftime(end_date, start_date, units = "days")[[1]]
    Duration <- as.double(Duration)/365 # absolute years
  }

  # Now calculate amount of data points, these are calculated as used rows

  if (!is.null(Duration)) {
    Datapoints <- ceiling(Duration) * length(rics)
    Limit <- difftimeConversionTable[difftimeConversionTable$EikonTimeName == interval,]$limit
  }

  if ( !is.null(Duration) && (Limit < Datapoints)) {
    message("The operation is too large for one api request and will be chunked in multiple requests")
    ChunckedRics <- EikonChunker(RICS = rics, MaxCallsPerChunk = Limit, Duration =  ceiling(Duration), MaxRicsperChunk = 300 )
  } else{
    ChunckedRics <- list(rics)
  }

  return(ChunckedRics)
}




#' Flatten a nested list put list format in data.table format way (don't mix up lists and vectors in one nested list)
#'
#' @param data a nested list
#'
#' @return a list
#' @keywords internal
#'
#' @examples
#' # Refinitiv:::flattenNestedlist(list(list("a", "b"), c(1,2)))
flattenNestedlist <- function(data){
  NestedListPos <- which(lapply(data, class) == "list")
  .SD <- NULL
  data2 <- vector(mode = "list", length = length(data))
  data2 <- lapply( X = seq_along(data)
                   , FUN = function(x, data, NestedListPos){
                     if (x %in% NestedListPos){
                       return(data.table::as.data.table(data[[x]])
                       )
                     } else{
                       return(data.table::transpose(data.table::as.data.table(data[[x]])))
                     }}
                   , data = data
                   , NestedListPos = NestedListPos
  )

}

