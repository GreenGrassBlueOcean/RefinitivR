#' ErrorProcessor for get_data python output
#'
#' This function processes the error output of the python function in r readable output.
#' This function is used by
#'
#' @param Eikon_get_data_Error List intermediate output of EikonPostProcessor
#' @param Chunked Boolean indicating if the get_data python output is chunked or not
#' @param ChunkRowStart Vector what the cumulative position is in the final data output
#'
#' @return a data.frame
#'
#' @seealso \link{EikonGetData},
#'          \link{EikonGetTimeseries}
#' @examples
EikonErrorProcessor <- function(Eikon_get_data_Error, Chunked, ChunkRowStart){

  # if Error is not chunked make it chunked with list length one to get uniform processing
  if ( isFALSE(Chunked)) {
    Eikon_get_data_Error <- list(Eikon_get_data_Error)
  }

  # Error is a list of list of list of errors
  # 1. turn it in a list of list of data.frames
  Step1 <- lapply( X = Eikon_get_data_Error
                 , FUN = function(x){ lapply( X = x
                                            , FUN = function(y, ncol){as.data.frame(matrix(y, ncol = ncol), stringsAsFactors = FALSE)}
                                            , ncol = 4
                 )
                 }
    )
  #2. turn it into a list of data.frames
  Step2 <- lapply( X = Step1
                 , FUN = function(x){as.data.frame(do.call("rbind", x), stringsAsFactors = FALSE)}
                 )

  #2.A set row number correct for one dataframe in stead a list of data.frames using ChunkRowStart
  Step2A <- Step2
  for (i in 1:length(Step2)) {
    if (length(Step2[[i]]) > 0 ) {
        Step2A[[i]][[4]] <- as.list(unlist(Step2[[i]][[4]]) + ChunkRowStart[i])
    }
  }

  #3. turn it into one single big data.frane
  Step3 <- do.call("rbind", Step2A)
  #4. turn columns into vectors
  Step4 <- as.data.frame(lapply(Step3, unlist), stringsAsFactors = FALSE)
  if (length(Eikon_get_data_Error[[1]]) > 0) {
      names(Step4) <-  names(Eikon_get_data_Error[[1]][[1]])
  }
  Step4 <- make.true.NA_df(Step4)
  return(Step4)
  }


#' A postprocessor to process Eikon (python) get_Data into r data.frames
#'
#' As the Python eikon get_data structure contains various list that are null these should be replaced
#' with NA to prevent disasters when later handling the data.frame. For example when using the unlist function Null elements
#' are removed from the lists causing shorter vectors than expected.
#'
#' @param Eikon_get_dataOuput a single Eikon$get_data result or a list of Eikon$get_data results.
#'
#' @return a list of a data.frame with cleaned output and the resulting error data.frame
#' @export
#'
#' @examples
EikonPostProcessor <- function(Eikon_get_dataOuput){

  #0. helper functions

  #1. main program
  if(identical(Eikon_get_dataOuput,list(NULL))) {
    return(list( "PostProcessedEikonGetData" = data.frame()
                 , "Eikon_Error_Data" = data.frame()))
  }

  getheaders <- function(data, requestnumber){
    #replace null headers with NA headers
    data[[requestnumber]][["headers"]] <- replaceInList(data[[requestnumber]][["headers"]], function(x)if(is.null(x))NA else x)

    unlist(lapply( X = 1:length(data[[requestnumber]][["headers"]][[1]])
                          , FUN = function(x,data){data[[1]][["headers"]][[1]][[x]][["displayName"]] }
                          , data = data))

  }

  getData <- function(data, requestnumber) {

    #1. Remove NULL values and replace with NA in nested list

    data[[requestnumber]][["data"]] <- replaceInList(data[[requestnumber]][["data"]], function(x)if(is.null(x))NA else x)

    #2. put list format in uniform way (don't mix up lists and vectors in one nested list)
    flattenNestedlist <- function(data){
      NestedListPos <- which(lapply(data, class) == "list")
      data <- lapply(X = seq_along(data), FUN = function(x, data, NestedListPos){if(x %in% NestedListPos){unlist(data[[x]])}else{data[[x]]}}, data = data, NestedListPos=NestedListPos)
      data <- lapply(X = seq_along(data), FUN = function(x, data){data.table::transpose(data.table::as.data.table(data[[x]]))}, data = data)
    }

    data[[requestnumber]][["data"]] <- flattenNestedlist(data[[requestnumber]][["data"]])

    if (length(data[[requestnumber]][["data"]]) > 1 ) {
      RequestData <- data.table::rbindlist(data[[requestnumber]][["data"]])
    } else {
      RequestData <- data[[requestnumber]][["data"]][[1]]
    }

     Requestheaders <- EikonNameCleaner(getheaders(data, requestnumber))
     data.table::setnames(RequestData, Requestheaders)
    return(RequestData)
  }


  RequestData <- lapply( X = 1:length(Eikon_get_dataOuput)
                       , FUN = function(x, data){getData(data, requestnumber=x)}
                       , data = Eikon_get_dataOuput
                       )

  RequestError <- lapply( X = 1:length(Eikon_get_dataOuput)
                         , FUN = function(x, data){rbindlist(data[[x]][["error"]])}
                         , data = Eikon_get_dataOuput
  )

  Eikon_Error_Data <- data.table::setDF(data.table::rbindlist(RequestError, use.names = TRUE, fill = TRUE))
  PostProcessedEikonGetData <- data.table::setDF(data.table::rbindlist(RequestData, use.names = TRUE, fill = TRUE))

  # return human readable names

  return(list( "PostProcessedEikonGetData" = PostProcessedEikonGetData
             , "Eikon_Error_Data" = Eikon_Error_Data
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



#' Download Operating mic definitions from the isowebsite
#'
#' @return a data.frame with details of the operating mics from www.iso20022.org
#' @export
#'
#' @examples GetISO103883_MIC()
#' @references \url{"https://www.iso20022.org/sites/default/files/ISO10383_MIC/ISO10383_MIC.csv"}
GetISO103883_MIC <- function(){
   OperatingMics_df <- utils::read.csv(file = "https://www.iso20022.org/sites/default/files/ISO10383_MIC/ISO10383_MIC.csv")

   colnames(OperatingMics_df)[which(names(OperatingMics_df) == "OPERATING.MIC")] <- "OPERATINGMIC"
   #Change countrycode to 3 digit format
   OperatingMics_df$iso3c <- suppressWarnings(countrycode::countrycode(sourcevar = OperatingMics_df$COUNTRY, origin = 'country.name', destination = "iso3c"))
   OperatingMics_df <- stats::na.omit(OperatingMics_df)
   return(OperatingMics_df)
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
  if(!is.null(Parameters) && (class(Parameters) != "list" | is.null(names(Parameters)))){
    stop("Parameters should be a named list")
  }

  # check sort_dir
  if(!is.null(sort_dir) && ((class(sort_dir) != "character") | (!((sort_dir) %in% c("asc", "desc"))))){
    stop("sort_dir parameter should be character \"asc\" or \"desc\"")
  }

  # Check sort_priority
  if(!is.null(sort_priority) && (class(sort_priority) != "numeric" )){
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
#' @export
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

   if(BestMatch){
     data.table::setnames(EikonSymbologyResult2, old = c("bestMatch","symbol") , new = c(BestMatchName, from_symbol_type) )

   }
   else{
     data.table::setnames(EikonSymbologyResult2, old = c("symbol") , new = c(from_symbol_type) )
   }

  #3. return output
  return(data.table::setDF(EikonSymbologyResult2))
}



#' Replace items in nested list
#'
#' @param x list
#' @param FUN function to operate on list
#' @param ... pass through parameters
#'
#' @return list
#'
#' @examples
#'  x <- list(list(NA, NULL, NULL), list("a", "b", "c"))
#' test <- Refinitiv:::replaceInList(x, function(x)if(is.null(x))NA else x)
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


