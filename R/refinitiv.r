#' Check if Conda exists, if not instals miniconda, add the python eikon module to the python environment r-reticulate
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment. Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
#' @param conda  The path to a conda executable. Use "auto" to allow reticulate to automatically find an appropriate conda binary. See Finding Conda in the reticulate package for more details
#' @param envname the name for the conda environment that will be used, default  r-reticulate. Don't Change!
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' install_eikon()
#' }
install_eikon <- function(method = "auto", conda = "auto", envname= "r-reticulate") {

  # create helper function to check if conda is installed
  CondaExists <- function(){

    out <- tryCatch(
      {
        reticulate::conda_binary()
        return(TRUE)
      },
      error = function(cond) {
        message(paste("Conda does not seem to be installed"))
        message(cond)
        return(FALSE)
      }
    )
    return(out)
  }

# Check if a conda environment exists and install if not available
  if (CondaExists() == FALSE ) {
      message("installing MiniConda, if this fails retry by running r/rstudio with windows administrative powers/linux elevated permissions")
      reticulate::install_miniconda(update = TRUE, force = TRUE)
      message('Miniconda installed')

  }
 if (!(envname %in% reticulate::conda_list()$name)) {
     reticulate::conda_create(envname = envname )
    }

  # reticulate::use_condaenv(condaenv = envname, conda = conda)

  if (!reticulate::py_module_available("eikon")) {
      reticulate::py_install(packages = c("numpy", "eikon") , envname = envname,  method = method, conda = conda)
  }


  if(!("DatastreamDSWS2R" %in% rownames(installed.packages()))){
    print("installing DatastreamDSWS2R from https://github.com/CharlesCara/DatastreamDSWS2R")
    try(devtools::install_github("CharlesCara/DatastreamDSWS2R"))
  }

}



#Data stream r api------------------

#' Initialize DataStream Connection
#'
#' @param DatastreamUserName Refinitiv DataStream username
#' @param DatastreamPassword Refinitiv DataStream password
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' DatastreamUserName <- "Your datastream username"
#' DatastreamPassword <- "Your datastream password"
#' DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)
#' }
DataStreamConnect <- function(DatastreamUserName, DatastreamPassword){

  options(Datastream.Username = DatastreamUserName)
  options(Datastream.Password = DatastreamPassword)
  mydsws <- DatastreamDSWS2R::dsws$new()
  return(mydsws)

  }

# Initialize eikon Python api using reticulate -------------------------------

#' Initialize Eikon Python api
#'
#' @param Eikonapplication_port proxy port id
#' @param Eikonapplication_id eikon api key
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L)
#' }
EikonConnect <- function(Eikonapplication_id = .EikonApiKey , Eikonapplication_port = 9000L) {

  options(.EikonApiKey = Eikonapplication_id)
  options(.EikonApplicationPort = Eikonapplication_port)

  reticulate::use_condaenv(condaenv = "r-reticulate", conda = "auto") # set virtual environment right
  PythonEK <- reticulate::import(module = "eikon") # import python eikon module

  PythonEK$set_port_number(.Options$.EikonApplicationPort)
  PythonEK$set_app_key(app_key = .Options$.EikonApiKey)

  return(PythonEK)
}


#' Show the attributes of the Python Eikon
#'
#' Function that the returns the names of the python Eikon api attributes that can be used as commands in R.
#'
#' @param EikonObject Python Object generated by EikonConnect
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L)
#' EikonShowAttributes(EikonObject = Eikon)
#' }
EikonShowAttributes <- function(EikonObject){
  PossibleAttributes <- reticulate::py_list_attributes(EikonObject)
  return(PossibleAttributes)
  }




#' Convert eikon formula's in human readable names
#'
#' @param names vector of data.frame column names
#'
#' @return
#' @export
#'
#' @examples
EikonNameCleaner <- function(names){

  returnNames <-  lapply( names
                        , FUN = function(x){TryCleanName <- unlist(qdapRegex::rm_between(x, '/*', '*/', extract = TRUE));
                                            return(ifelse(test = !is.na(TryCleanName), yes = TryCleanName, no = x ))
                                           }

                        )
  return(returnNames)
}



#' Returns a list of chunked Rics so that api limits can be satisfied
#'
#' @param RICS a vector containing RICS to be requested
#' @param Eikonfields a list of the eikonfields to be requested default NULL, if eikonfields are supplied duration may not be supplied
#' @param MaxCallsPerChunk the maximum amount of apicalls that can be made
#' @param Duration a natural number denoting the amoount of row asked for in a TimeSeries default NULL, if Duration is supplied duration may not be supplied
#'
#' @return a list of splitted RICS that can be returned to guarantee compliance with api limits.
#' @export
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
EikonChunker <- function(RICS, Eikonfields = NULL, MaxCallsPerChunk = 12000, Duration = NULL) {

  if (!is.null(Eikonfields) & is.null(Duration)) {
    totalDataPoints <- length(RICS) * length(Eikonfields)
  } else if (!is.null(Duration) & is.null(Eikonfields)) {
    totalDataPoints <- length(RICS) * Duration
    if (Duration > MaxCallsPerChunk) {
        stop("Duration is too long for even one ric, Reduce Duration by changing start_date or end_date!")
    }
  } else{
      stop("supply either Duration or Eikonfields")
  }


  # # Make sure that api limits are respected

  message(paste0("the operation you intend to perform will cost ", totalDataPoints, " data points"))
  Chunks <- ceiling(totalDataPoints/MaxCallsPerChunk)
  ChunkLength <- length(RICS)/Chunks

  # ChosenSimulataneousRics <- length(Stoxx1800Constits$RIC)/chosenchunks
  SplittedRics <-  split(RICS, ceiling(seq_along(RICS)/ChunkLength))
  return(SplittedRics)
}


#' Function to retry failed functions after a time out of 5 seconds. Especially useful for failed api calls.
#'
#' @param max maximum number of retries, default = 0
#' @param init initial state of retries should always be zero
#' @param retryfun function to retry
#'
#' @return
#' @export
#'
#' @examples  retry(sum(1,"a"), max = 2)
retry <- function(retryfun, max = 10, init = 0){
  suppressWarnings( tryCatch({
    if (init < max) retryfun
  }, error = function(e){print(paste("retrying time",init + 1)) ;Sys.sleep(time = 5); retry(retryfun, max, init = init + 1)}))
}


#' Function to obtain timeseries from Eikon. Based on the Eikon python function get_timeseries
#'
#' Automatically chunks the timeseries in seperate apicalls and binds them together again in order to comply with api regulations.
#'
#' @param EikonObject Python eikon module result from EikonConnect
#' @param rics a vector containing Reuters rics
#' @param interval Data interval. Possible values: 'tick', 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly'  Default: 'daily'
#' @param calender Possible values: 'native', 'tradingdays', 'calendardays'., Default: 'tradingdays'
#' @param fields a vector containing  any combination ('TIMESTAMP', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE')
#' @param start_date Starting date and time of the historical range. string format is: '\%Y-\%m-\%dT\%H:\%M:\%S'.
#' @param end_date  End date and time of the historical range.  string format is: '\%Y-\%m-\%dT\%H:\%M:\%S'.
#' @param cast  castedata from wide to long format using the reshape::cast function, Default: TRUE
#'
#' @return
#' @export
#'
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#' @examples
#' \dontrun{
#' Eikon <- Refinitiv::EikonConnect()
#' EikonGetTimeseries(EikonObject = Eikon, rics = c("MMM", "III.L"),
#'                    start_date = "2020-01-01T01:00:00",
#'                    end_date = paste0(Sys.Date(), "T01:00:00"))
#' }
EikonGetTimeseries <- function(EikonObject, rics, interval = "daily", calender = "tradingdays", fields = c('TIMESTAMP', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE')
                              , start_date = "2020-01-01T01:00:00", end_date = paste0(Sys.Date(), "T01:00:00"), cast = TRUE){

  # Make sure that Python object has api key
  EikonObject$set_app_key(app_key = .Options$.EikonApiKey)

  # Convert as posix
  start_date <- as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S")
  end_date <- as.POSIXlt(end_date, format = "%Y-%m-%dT%H:%M:%S")

  # Build dataframe for internal lookup of names and datapoints limits
  difftimeConversionTable <- data.frame( EikonTimeName = c('tick', 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly')
                                       , difftimeName = c(NA,  "mins", "hours","days", "weeks", NA, NA, NA)
                                       , limit = c(50000,50000,50000,3000,3000,3000,3000,3000)
                                       )


  #check if chunking is required
  # CalculateDuration based on weekends
  if (interval %in% c('tick')) {
    warning("Intraday tick data chunking currently not supported, maximum 50.000 data points per request")
  } else if ( interval %in% c('minute', 'hour', 'daily', 'weekly')) {
    Duration <- difftime(end_date, start_date, units = difftimeConversionTable[difftimeConversionTable$EikonTimeName == interval,]$difftimeName)[[1]]
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

 # Now calculate amount of datapoints, these are calculated as used rows

  if (!is.null(Duration)) {
    Datapoints <- ceiling(Duration) * length(rics)
    Limit <- difftimeConversionTable[difftimeConversionTable$EikonTimeName == interval,]$limit
  }

  if ( !is.null(Duration) && (Limit < Datapoints)) {
    print("chunking required!")
    ChunckedRics <- EikonChunker(RICS = rics, MaxCallsPerChunk = Limit, Duration =  ceiling(Duration) )
  } else{
    ChunckedRics <- list(rics)
  }

  TimeSeriesList <- vector(mode = 'list', length = length(ChunckedRics))
  for (j in 1:length(ChunckedRics)) {
    TimeSeriesList[[j]] <- try(retry(EikonObject$get_timeseries( rics = ChunckedRics[[j]]
                                                               , interval = interval
                                                               , calendar = calender
                                                               , fields = fields
                                                               , start_date = as.character(start_date,  "%Y-%m-%dT%H:%M:%S")
                                                               , end_date = as.character(end_date,  "%Y-%m-%dT%H:%M:%S")
                                                               , normalize = TRUE
                                                               )

    ))
    Sys.sleep(time = 0.5)
  }

  ReturnTimeSeries <- do.call("rbind", TimeSeriesList)


  if (isTRUE(cast)) {
    ReturnTimeSeries <- suppressWarnings(reshape::cast(ReturnTimeSeries,  Date +  Security ~ Field))
    ReturnTimeSeries <- ReturnTimeSeries[order(ReturnTimeSeries$Security),]
   }

  return(ReturnTimeSeries)
}





#' Function to obtain data from Eikon. Based on the Eikon python function get_data
#'
#' The function automatically chunks the list of rics into chunks that comply with the api limitations and in the end rebuilds the chunks again into a single data.frame.
#'
#' @param EikonObject Eikon object created using EikonConnect function
#' @param rics a vector containing the instrument RICS
#' @param Eikonformulas a vector containing character string of Eikon Formulas
#' @param raw_output to return the raw list by chunk for debugging purposes, default = FALSE
#'
#' @return
#' @export
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
#' \dontrun{
#' Eikon <- Refinitiv::EikonConnect()
#' EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
#'              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/", "TR.CompanyName"))
#' }
EikonGetData <- function(EikonObject, rics, Eikonformulas, raw_output = FALSE){

#Make sure that Python object has api key
EikonObject$set_app_key(app_key = .Options$.EikonApiKey)
# Divide RICS in chunks to satisfy api limits
ChunckedRics <- Refinitiv::EikonChunker(RICS = rics, Eikonfields = Eikonformulas)

EikonDataList <- vector(mode = 'list', length = length(ChunckedRics))
for (j in 1:length(ChunckedRics)) {
  EikonDataList[[j]] <- try(retry(EikonObject$get_data( instruments = ChunckedRics[[j]]
                                           , fields = as.list(Eikonformulas)
                                           , debug = FALSE, raw_output = FALSE
  )))
  Sys.sleep(time = 0.5)
}


if (!raw_output) {
  ReturnElement <- EikonPostProcessor(EikonDataList)
} else {
  ReturnElement <- EikonDataList
}

return(ReturnElement)
}




