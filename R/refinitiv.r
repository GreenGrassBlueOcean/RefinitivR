# create helper function to check if conda is installed
CondaExists <- function(){

  out <- tryCatch(
    {
      suppressWarnings(suppressMessages(reticulate::conda_binary()))
    },
    error = function(cond) {
      message(paste("Conda does not seem to be installed"))
      message(cond)
      return(FALSE)
    }
  )
  out <- ifelse(isFALSE(out), yes = FALSE, no = TRUE)

  return(out)
}


#' Check if Conda exists, if not instals miniconda, add the python eikon module to the python environment r-reticulate
#'
#' This function can also be used to update the required python packages so that you can always use the latest version of the pyhton packages numpy and eikon.
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment. Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
#' @param conda  The path to a conda executable. Use "auto" to allow reticulate to automatically find an appropriate conda binary. See Finding Conda in the reticulate package for more details
#' @param envname the name for the conda environment that will be used, default  r-reticulate. Don't Change!
#' @param update boolean, allow to rerun the command to update the packages required to update the python packages numpy and eikon defaults to true
#'
#' @return None
#' @importFrom utils installed.packages
#' @export
#'
#' @examples
#' \dontrun{
#' install_eikon()
#' }
install_eikon <- function(method = "auto", conda = "auto", envname= "r-reticulate", update = TRUE) {

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

  if (!reticulate::py_module_available("eikon") || update ) {
    # try(reticulate::conda_remove(packages = c("numpy", "pandas", "nest-asyncio","eikon") , envname = envname,  conda = conda ))
    # reticulate::py_install(packages = c("httpx==0.14.2", "numpy", "eikon==1.1.6post2") , envname = envname,  method = method, conda = conda, pip = TRUE )
    reticulate::py_install(packages = c("numpy==1.19.3", "pandas", "nest-asyncio==1.3.0","eikon") , envname = envname,  method = method, conda = conda, pip = TRUE, update = TRUE )
    reticulate::py_install(packages = "refinitiv.dataplatform", envname = envname, method = method, conda = conda, update = TRUE, pip = TRUE)
  }

  return("Eikon Python interface successfully installed or updated")
}



#Data stream r api------------------

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
    if(is.null(DatastreamUserName)){stop("Please supply Datastream Username")}
  }

  if (is.na(DatastreamPassword)){
    try(DatastreamPassword <- getOption("Datastream.Password") )
    if (is.null(DatastreamPassword)){stop("Please supply Datastream Password")}
  }

  #2. Perform Main operation ----
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
#' @param PythonModule character choose between Eikon or RDP for Refinitiv Data Platform
#'
#' @return a Python module that is an EikonObject
#' @export
#'
#' @examples
#' \dontrun{
#' Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L
#' , PythonModule = "Eikon")
#' Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L
#' , PythonModule = "RDP")
#' }
EikonConnect <- function(Eikonapplication_id = NA , Eikonapplication_port = 9000L, PythonModule = "Eikon") {

  # 1. check input ----
  if (is.na(Eikonapplication_id)){
    try(Eikonapplication_id <- getOption(".EikonApiKey") )
    if(is.null(Eikonapplication_id)){stop("Please supply Eikonapplication_id")}
  }

  if (is.na(PythonModule)){
    try(PythonModule <- getOption(".RefinitivAPI") )
    if(is.null(PythonModule)){stop("Please supply name of PythonModule: Eikon or RDP")}
  }

  if(!CondaExists()){stop("Conda/reticulate does not seem to be available please run install_eikon")}

  #2. Run main programme ----
  options(.EikonApiKey = Eikonapplication_id)
  options(.EikonApplicationPort = Eikonapplication_port)
  options(.RefinitivAPI = PythonModule)

  reticulate::use_condaenv(condaenv = "r-reticulate", conda = "auto") # set virtual environment right
  # PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon") # import python eikon module

  if (identical(.Options$.RefinitivAPI, "Eikon")){
    PythonEK <- reticulate::import(module = "eikon") # import python eikon module
    PythonEK$set_port_number(.Options$.EikonApplicationPort)
    PythonEK$set_app_key(app_key = .Options$.EikonApiKey)
  } else if (identical(.Options$.RefinitivAPI, "RDP")){
    PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon") # import python eikon module
    PythonEK$set_app_key(app_key = .Options$.EikonApiKey)
  } else {
    stop(paste("variable PythonModule is now", PythonModule, "but should only be 'Eikon' or 'RDP'." ))
  }

  return(PythonEK)
}


#' Show the attributes of the Python Eikon
#'
#' Function that the returns the names of the python Eikon api attributes that can be used as commands in R.
#'
#' @param EikonObject Python Object generated by EikonConnect
#'
#' @return a list of attributes
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




#' Convert Eikon formula's in human readable names
#'
#' @param names vector of data.frame column names
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is "."
#'
#' @return a data.frame in which the Eikon formula's are replaced with the Eikon display Name which is the last part of the formula.
#' @export
#'
#' @details
#' The variables returned by the Eikon API are not always easily guessable upfront.
#' There is no fixed way how variables are returned. EikonNameCleaner improves this by returning UpperCamel case variable names.
#' It does however not turn capitalized words back into lowercase. So e.g. RDN_EXCHD2 will stay RDN_EXCHD2 and "Dividend yield"
#' will return as "Dividend.Yield" (if SpaceConvertor is set as ".")
#'
#' The parameter SpaceConvertor converts spaces into any of the following characters ".", "," , "-", "_".
#' Setting SpaceConvertor to another value will cause no conversion of spaces in the variable names.
#' Space Conversion is highly advisable due to the fact that otherwise issues could occur with accessing columnnames in the returned data.frame
#' as an example "Dividend yield" will turn into "Dividend.Yield" with SpaceConvertor being set as "."
#'
#' @examples
#' EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC"))
EikonNameCleaner <- function(names, SpaceConvertor = "."){

  #0. Helper Functions ----
  firstup <- function(x) {
              substr(x, 1, 1) <- toupper(substr(x, 1, 1))
              x
  }

  #1. Main Function ----

  returnNames <- unlist(qdapRegex::rm_between(names, '/*', '*/', extract = TRUE))

  returnNames[is.na(returnNames)] <- stringi::stri_split(str = names[is.na(returnNames)], fixed = " ") %>%
                                     lapply(FUN =  firstup) %>%
                                     lapply(FUN = paste, collapse = " ")


  if(SpaceConvertor %in% c(".", "," , "-", "_") ){
    returnNames <- gsub(x = returnNames, pattern = " ", replacement = SpaceConvertor, perl = TRUE)
  }

  returnNames <- stringi::stri_trans_general(returnNames, "latin-ascii")
  return(returnNames)
}



#' Returns a list of chunked Rics so that api limits can be satisfied
#'
#' @param RICS a vector containing RICS to be requested
#' @param Eikonfields a list of the eikonfields to be requested default NULL, if eikonfields are supplied duration may not be supplied
#' @param MaxCallsPerChunk the maximum amount of apicalls that can be made
#' @param Duration a natural number denoting the amoount of rows asked for in a TimeSeries default NULL, if Duration is supplied Eikonfields may not be supplied
#' @param MaxRicsperChunk  a natural number denoting the maximum amount of Rics that should be available in one call, default is 300.
#'
#' @return a list of splitted RICS that can be returned to guarantee compliance with api limits.
#' @export
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
EikonChunker <- function(RICS, Eikonfields = NULL, MaxCallsPerChunk = 12000, Duration = NULL, MaxRicsperChunk = 300) {

  if (!is.null(Eikonfields) & is.null(Duration)) {
    totalDataPoints <- length(RICS) * length(Eikonfields)
  } else if (!is.null(Duration) & is.null(Eikonfields)) {
    totalDataPoints <- length(RICS) * Duration
    if (Duration > MaxCallsPerChunk) {
        stop("Duration is too long for even one RIC, Reduce Duration by changing start_date or end_date!")
    }
  } else{
      stop("supply either Duration or Eikonfields")
  }


  # # Make sure that api limits are respected

  message(paste0("the operation you intend to perform will cost ", totalDataPoints, " data points"))
  Chunks <- ceiling(totalDataPoints/MaxCallsPerChunk)
  ChunkLength <- length(RICS)/Chunks

  # ChosenSimulataneousRics <- length(Stoxx1800Constits$RIC)/chosenchunks
  if(!is.null(MaxRicsperChunk)){
    if(ChunkLength > MaxRicsperChunk){
      ChunkLength <- MaxRicsperChunk - 1
    }
  }

  SplittedRics <-  split(RICS, ceiling(seq_along(RICS)/floor(ChunkLength)))
  return(SplittedRics)
}

#' Function to retry failed functions after a time out of 5 seconds. Especially useful for failed api calls.
#'
#' @param max maximum number of retries, default = 2
#' @param init initial state of retries should always be left zero, default = zero
#' @param retryfun function to retry.
#'
#' @return None
#' @export
#'
#' @examples  retry(sum(1,"a"), max = 2)
retry <- function(retryfun, max = 2, init = 0){
  suppressWarnings( tryCatch({
    if (init < max) retryfun
  }, error = function(e){message(paste0("api request failed, automatically retrying time ",init + 1, "/", max, " error received: ", e))
                        ; Sys.sleep(time = 0.5); retry(retryfun, max, init = init + 1);return(NA)}))
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
#' @param cast  cast data from wide to long format using the data.table::dcast function, Default: TRUE
#' @param time_out set the maximum timeout to the Eikon server, default = 60
#' @param verbose boolean if TRUE prints out the python call to the console
#' @param raw_output provide only the raw downloaded info from Eikon
#' @param corax possible values 'adjusted', 'unadjusted'. Default: 'adjusted'
#'
#' @importFrom utils capture.output head
#' @importFrom data.table `:=`
#'
#' @return A data.frame containing time series from Eikon
#' @export
#'
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#' @importFrom  data.table rbindlist dcast
#' @examples
#' \dontrun{
#' Eikon <- Refinitiv::EikonConnect()
#' ex1 <- EikonGetTimeseries(EikonObject = Eikon, rics = c("MMM", "III.L"),
#'                    start_date = "2020-01-01T01:00:00",
#'                    end_date = paste0(Sys.Date(), "T01:00:00"), verbose = TRUE)
#' }
EikonGetTimeseries <- function(EikonObject, rics, interval = "daily", calender = "tradingdays", corax = "adjusted", fields = c('TIMESTAMP', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE')
                              , start_date = "2020-01-01T01:00:00", end_date = paste0(Sys.Date(), "T01:00:00"), cast = TRUE, time_out = 60, verbose = FALSE, raw_output = FALSE){

  # Make sure that Python object has api key and change timeout
  # EikonObject$set_timeout(timeout = time_out)
  EikonObject$set_app_key(app_key = .Options$.EikonApiKey)





  # Convert as posix
  start_date <- as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S")
  end_date <- as.POSIXlt(end_date, format = "%Y-%m-%dT%H:%M:%S")


  ChunckedRics <- EikonTimeSeriesPreprocessor(interval = interval, rics = rics, start_date = start_date, end_date = end_date)

  TimeSeriesList <- as.list(rep(NA, times = length(ChunckedRics)))

  DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
                                     , succes =  rep(FALSE, length(ChunckedRics))
                                     , retries = rep(0L, length(ChunckedRics), stringsAsFactors = FALSE)
  )

  while (!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)  ) {

    ChunckedRicsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

  for (j in ChunckedRicsTryList) {
    TimeSeriesList[[j]] <- try({ if (verbose){  message(paste0(Sys.time(), "\n"
                                                               , " get_timeseries( rics = [\"", paste(ChunckedRics[[j]], collapse = "\",\""), "\"]\n"
                                                               , "\t, interval= \"", interval, "\"\n"
                                                               , "\t, interval= \"", calender, "\"\n"
                                                               , "\t, fields = [\"", paste(fields, collapse = "\",\""),  "\"]\n"
                                                               , "\t, start_date =  \"", as.character(start_date,  "%Y-%m-%dT%H:%M:%S"), "\"\n"
                                                               , "\t, end_date =  \"", as.character(end_date,  "%Y-%m-%dT%H:%M:%S"), "\"\n"
                                                               , "\t, normalize = True\n\t)"
    )
    )}


      retry(EikonObject$get_timeseries( rics = ChunckedRics[[j]]
                                                               , interval = interval
                                                               , calendar = calender
                                                               , fields = fields
                                                               , start_date = as.character(start_date,  "%Y-%m-%dT%H:%M:%S")
                                                               , end_date = as.character(end_date,  "%Y-%m-%dT%H:%M:%S")
                                                               , normalize = TRUE
                                                               , raw_output = TRUE
                                                               , corax = corax
                                                               )

    )})
    InspectRequest(df = TimeSeriesList[[j]], functionname = "EikonGetTimeseries", verbose = verbose)
    Sys.sleep(time = 0.5)


    if (!identical(TimeSeriesList[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE }
    if(verbose){
      message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
    }
  }

  DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1
}
  if(any(DownloadCoordinator$retries > 4L)){
    warning("EikonGetTimeseries downloading data failed for one or more Rics")
  }

  if(raw_output){
    return(TimeSeriesList)
  } else {
    return(PostProcessTimeSeriesRequest(TimeSeriesList))
  }
}





#' Function to obtain data from Eikon. Based on the Eikon python function get_data
#'
#' The function automatically chunks the list of rics into chunks that comply with the api limitations and in the end rebuilds the chunks again into a single data.frame.
#'
#' Currently there is a known bug in the reticulate package with large integers.
#' If a request is made that returns large integers the reticulate package will return -1 for these integers.
#' See also this issue \url{https://github.com/rstudio/reticulate/issues/323}
#' or try for yourself as indicated here \url{https://community.rstudio.com/t/large-integer-conversion-from-python-to-r/82568}
#'
#'
#' @param EikonObject Eikon object created using EikonConnect function
#' @param rics a vector containing the instrument RICS
#' @param Eikonformulas a vector containing character string of Eikon Formulas
#' @param Parameters a named key value list for setting parameters, Default: NULL
#' @param raw_output to return the raw list by chunk for debugging purposes, default = FALSE
#' @param time_out set the maximum timeout to the Eikon server, default = 60
#' @param verbose boolean, set to true to print out the actual python call with time stamp for debugging.
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is "."
#'
#' @return a data.frame containing data.from Eikon
#' @importFrom utils capture.output
#'
#' @export
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
#' \dontrun{
#' Eikon <- Refinitiv::EikonConnect()
#' ex1 <- EikonGetData(EikonObject = Eikon, rics = c("MMM", "III.L"),
#'              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
#'              , "TR.CompanyName"), verbose = TRUE)
#'
#' ex2 <- EikonGetData( EikonObject = Eikon, rics = "AAPL.O"
#'                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
#'                    )
#'
#' # ex2 will return -1 which is most likely not the current market cap of apple")
#' # a workaround is to scale back the output to millions
#'
#' ex2a <- EikonGetData( EikonObject = Eikon, rics = "AAPL.O"
#'                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
#'                    , Parameters = list("scale" = 6)
#'                    )
#' # or for more complex formula's
#' # scale back in the formula itself
#' ex2b <- EikonGetData( EikonObject = Eikon, rics = "AAPL.O"
#'                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D, scale=6)/*Market Cap*/"
#'                    )
#' }
EikonGetData <- function(EikonObject, rics, Eikonformulas, Parameters = NULL, raw_output = FALSE, time_out = 60, verbose = FALSE, SpaceConvertor = "."){

#Make sure that Python object has api key
EikonObject$set_app_key(app_key = .Options$.EikonApiKey)
# EikonObject$set_timeout(timeout = time_out) #add timeout to reduce chance on timeout error chance.


# Divide RICS in chunks to satisfy api limits
ChunckedRics <- Refinitiv::EikonChunker(RICS = rics, Eikonfields = Eikonformulas)


EikonDataList <- as.list(rep(NA, times = length(ChunckedRics)))

DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
                                 , succes =  rep(FALSE, length(ChunckedRics))
                                 , retries = rep(0L, length(ChunckedRics), stringsAsFactors = FALSE)
                                 )

while (!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)  ) {

  ChunckedRicsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

  for (j in ChunckedRicsTryList){
    #for (j in 1:length(ChunckedRics)) {
      EikonDataList[[j]] <- try({ if (verbose){  message(paste0(Sys.time(), "\n"
                                                                , " get_data( instruments = [\"", paste(ChunckedRics[[j]], collapse = "\",\""), "\"]\n"
                                                                , "\t, fields = [\"", paste(Eikonformulas, collapse = "\",\""),  "\"]\n"
                                                                , "\t, debug = False, raw_output = True\n\t)"
      )
      )}
        retry(EikonObject$get_data( instruments = ChunckedRics[[j]]
                                    , fields = as.list(Eikonformulas)
                                    , parameters = Parameters
                                    , debug = FALSE, raw_output = TRUE
        ), max = 3)})



      InspectRequest(df = EikonDataList[[j]], functionname = "EikonGetData", verbose = verbose)
      Sys.sleep(time = 0.5)



  if (!identical(EikonDataList[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE }

  if(verbose){
      message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
  }
  }

  DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1

}

if(any(DownloadCoordinator$retries > 4L)){
  stop("EikonGetData downloading data failed")
}


if (!raw_output) {
  EikonDataList <- lapply(EikonDataList, FUN = function(x){if(all(is.na(x))){return(NULL)} else{return(x)}})
  ReturnElement <- EikonPostProcessor(EikonDataList, SpaceConvertor)
} else {
  ReturnElement <- EikonDataList
}

return(ReturnElement)
}







# #' @param debug boolean When set to TRUE, the json request and response are printed.



#' Returns a list of instrument names converted into another instrument code.
#' For example: convert SEDOL instrument names to RIC names
#'
#' original python parameters raw_output and debug cannot be used due to int64 python to R conversion problem.
#' \url{https://github.com/rstudio/reticulate/issues/729}
#'
#' @param EikonObject Eikon object created using EikonConnect function
#' @param symbol character or list of characters 	Single instrument or list of instruments to convert.
#' @param from_symbol_type character Instrument code to convert from. Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker', 'lipperID', 'IMO' Default: 'RIC'
#' @param to_symbol_type character  string or list 	Instrument code to convert to. Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker', 'lipperID', 'IMO', 'OAPermID' Default: None (means all symbol types are requested)
#' @param raw_output boolean 	Set this parameter to True to get the data in json format if set to FALSE, the function will return a data frame Default: FALSE
#' @param bestMatch boolean 	When set to TRUE, only primary symbol is requested. When set to FALSE, all symbols are requested
#' @param time_out numeric set the maximum timeout to the Eikon server, default = 60
#' @param verbose boolean, set to true to print out the actual python call with time stamp for debugging.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' Eikon <- Refinitiv::EikonConnect()
#' ex1 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "AAPL.O"
#'  , to_symbol_type = "ISIN" )
#' ex2 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE)
#' ex3 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
#' ex4 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.AS"
#' , to_symbol_type = "ISIN"  , verbose = TRUE)
#' ex5 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.L"
#' , to_symbol_type = "ISIN"  , verbose = TRUE)
#' ex6 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  c("GB00B03MLX29", "NL0015476987"), from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
#' ex7 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
#' }
EikonGetSymbology <- function( EikonObject, symbol, from_symbol_type = "RIC", to_symbol_type = c('CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker', 'lipperID', 'IMO', 'OAPermID')
                               , bestMatch = TRUE, time_out = 60, verbose = FALSE, raw_output = FALSE){

  #Make sure that Python object has api key
  EikonObject$set_app_key(app_key = .Options$.EikonApiKey)
  # EikonObject$set_timeout(timeout = time_out) #add timeout to reduce chance on timeout error chance.


  # Divide symbols in chunks to satisfy api limits
  ChunckedSymbols <- Refinitiv::EikonChunker(RICS = symbol, Eikonfields = to_symbol_type)

  EikonSymbologyList <- as.list(rep(NA, times = length(ChunckedSymbols)))
  for (j in 1:length(ChunckedSymbols)) {
    EikonSymbologyList[[j]] <- try({ if (verbose){  message(paste0(Sys.time(), "\n"
                                                              , "get_symbology( symbol = [\"", paste(ChunckedSymbols[[j]], collapse = "\",\""), "\"]\n"
                                                              , "\t, from_symbol_type = [\"", paste(from_symbol_type, collapse = "\",\""),  "\"]\n"
                                                              , "\t, to_symbol_type = [\"", paste(to_symbol_type, collapse = "\",\""),  "\"]\n"
                                                              , "\t, bestMatch = ", ifelse(test = isTRUE(bestMatch), yes = "True", no = "False")  ,"\n"
                                                              , "\t, debug = False, raw_output = True\n\t)"
    )
    )}
      retry(EikonObject$get_symbology( symbol = ChunckedSymbols[[j]]
                                     , from_symbol_type = from_symbol_type
                                     , to_symbol_type = list(to_symbol_type)
                                     , raw_output = TRUE
                                     , debug = FALSE
                                     , bestMatch = bestMatch
                                  ))
    })
    InspectRequest(df = EikonSymbologyList[[j]], functionname = "EikonGetSymbology")
    Sys.sleep(time = 0.5)
  }


  if (!raw_output) {
     EikonSymbologyList <- lapply(EikonSymbologyList, FUN = function(x){if(all(is.na(x))){return(NULL)} else{return(x)}})
     ReturnElement <- ProcessSymbology(EikonSymbologyList, from_symbol_type = from_symbol_type, to_symbol_type = to_symbol_type)
   } else {
     ReturnElement <- EikonSymbologyList
   }

  return(ReturnElement)
}




#' function to check if a downloaded dataframe is empty
#'
#' @param df data.frame
#' @param functionname function name for error reporting
#' @param verbose Boolean print or not variable
#'
#' @importFrom utils capture.output
#' @importFrom utils head
#' @return boolean
#' @export
#'
#' @examples
#' InspectRequest(data.frame(), functionname = "test")
#' InspectRequest(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test")
InspectRequest <- function(df, functionname, verbose = TRUE){
  if(!verbose){
    return(NULL)
  }

  try(message(str(df)))

  if(class(df) == "logical" && is.na(df) ){
    try(message(paste0(functionname, " request returned NA")))
    # stop("Wrong output retrieved from Refinitiv")
  }

 if(("error" %in% names(df))){
    try(message(paste0(functionname, " request returned the following (and other) erors: ",paste(capture.output(head(data.table::rbindlist(df$error),10)), collapse = "\n" ))))
  }

  if("totalRowsCount" %in% names(df) &&  "totalColumnsCount"  %in% names(df)  ){
    try(message(paste0(functionname, " request returned the following data: ",df$totalColumnsCount, " columns and ",  df$totalRowsCount, " rows")))}

  if(length(df)!=0){
    try(message(paste0(functionname, " request returned with length ", length(df))))
  } else{
    try(message(paste0(functionname, " request returned with length 0")))
  }

}




