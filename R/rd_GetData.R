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
#' @param rics a vector containing the instrument RICS
#' @param Eikonformulas a vector containing character string of Eikon Formulas
#' @param Parameters a named key value list for setting parameters, Default: NULL
#' @param raw_output to return the raw list by chunk for debugging purposes, default = FALSE
#' @param time_out set the maximum timeout to the Eikon server, default = 60
#' @param verbose boolean, set to true to print out the actual python call with time stamp for debugging.
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is NULL
#' @param RDObject Refinitiv Data connection object
#' @param use_field_names_in_headers boolean return request fieldnames in stead of titles
#' @param SyncFields boolean, synchronize fields over same time axis (only JSON!, because not supported in Python (use GetHistory))
#'
#' @return a data.frame containing data.from Eikon
#' @importFrom utils capture.output
#'
#' @export
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
#' \dontrun{
#' Refinitiv <- RDConnect(PythonModule = "RD")
#' ex1 <- rd_GetData(RDObject = Refinitiv, rics = c("MMM", "III.L"),
#'              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
#'              , "TR.CompanyName"), verbose = TRUE)
#'
#' ex2 <- rd_GetData( RDObject = Refinitiv, rics = "AAPL.O"
#'                    , Eikonformulas = "TR.CompanyMarketCap(Sdate=0D)/*Market Cap*/"
#'                    )
#' }
#' \dontrun{
#' Refinitiv <- RDConnect(PythonModule = "JSON")
#' ex1 <- rd_GetData(RDObject = Refinitiv, rics = c("MMM", "III.L"),
#'              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
#'              , "TR.CompanyName"), verbose = TRUE)
#'
#' }
#'
rd_GetData <- function(RDObject = RefinitivJsonConnect(), rics, Eikonformulas, Parameters = NULL, raw_output = FALSE
                       , time_out = 60, verbose = FALSE, SpaceConvertor = NULL, use_field_names_in_headers = F, SyncFields = FALSE){


  #Make sure that Python object has api key
  try(RDObject$set_app_key(app_key = .Options$.EikonApiKey), silent = T)
  # EikonObject$set_timeout(timeout = time_out) #add timeout to reduce chance on timeout error chance.


  # Divide RICS in chunks to satisfy api limits
  ChunckedRics <- EikonChunker(RICS = rics, Eikonfields = Eikonformulas)


  EikonDataList <- as.list(rep(NA, times = length(ChunckedRics)))

  DownloadCoordinator <- data.frame( index = 1:length(ChunckedRics)
                                   , succes =  rep(FALSE, length(ChunckedRics))
                                   , retries = rep(0L, length(ChunckedRics), stringsAsFactors = FALSE)
                                   )

  while (!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)  ) {

    ChunckedRicsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

    for (j in ChunckedRicsTryList){
      if(getOption(".RefinitivPyModuleName") =="JSON"){

      #for (j in 1:length(ChunckedRics)) {
      EikonDataList[[j]] <- try({
        retry(RDObject$get_data( instruments = ChunckedRics[[j]]
                                   , fields = as.list(Eikonformulas)
                                   , parameters = Parameters
                                   , SyncFields = SyncFields
                                   , debug = FALSE, raw_output = TRUE
        ), max = 3)})

      }else{

        if(!isFALSE(SyncFields)){
          warning("Parameter SyncFields can only be used in JSON and is therefore ignored")
        }

        EikonDataList[[j]] <- try({
          retry({#Pycall <- RDObject$content$fundamental_and_reference$Definition(universe = ChunckedRics[[j]]
            Pycall <- RDObject$get_data( universe = ChunckedRics[[j]]
                                       , fields = as.list(Eikonformulas)
                                       , parameters = Parameters
                                       , use_field_names_in_headers = use_field_names_in_headers
                                       #, extended_params = NULL #not implemented yet
                                       )

                #request <- Pycall$get_data()
                request <- Pycall$to_json(date_format = "ms")
                #PyJsonConvertor(request$data$raw)
                }, max = 3)})
      }

      #InspectRequest(df = EikonDataList[[j]], functionname = "EikonGetData", verbose = verbose)
      Sys.sleep(time = 0.01)



      if (!identical(EikonDataList[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE }

      if(verbose){
        message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
      }
    }

    DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1

  }

  if(any(DownloadCoordinator$retries > 4L)){
    stop("rd_GetData downloading data failed")
  }


  if (!raw_output) {

    # Process request and build return data.frame using data.table ----
    if(getOption(".RefinitivPyModuleName") =="refinitiv.data"){
      ReturnList <-  lapply( X =  EikonDataList
                           , FUN =  Process_RDP_output
                           , RemoveNA = FALSE
                           , SpaceConvertor = SpaceConvertor
                           )
    }
    if(getOption(".RefinitivPyModuleName") =="JSON"){
      ReturnList <-  lapply( X =  EikonDataList
                           , FUN =  rd_OutputProcesser
                           , NA_cleaning = FALSE
                           , use_field_names_in_headers = use_field_names_in_headers
                           , SpaceConvertor = SpaceConvertor
                           )
    }
    ReturnElement <- data.table::rbindlist(ReturnList, use.names = T, fill = T) |> data.table::setDF()

  } else {
    ReturnElement <- EikonDataList
  }

  return(ReturnElement)
}

