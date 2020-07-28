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

    # check if chuncked list is returned
    # Non Chunked list satisfies 2 conditions:
    # 1. Result is list that consits out of two lists of whuich the first is a data.frame
    Chunked <- !(length(Eikon_get_dataOuput) == 2 && is.data.frame(Eikon_get_dataOuput[[1]]) )

    if (Chunked) {
      Eikon_get_data_pre <- lapply(Eikon_get_dataOuput, "[[", 1)
      Eikon_get_data_pre <- lapply(Eikon_get_data_pre, as.data.frame, stringsAsFactors = FALSE)
      Eikon_get_data <- do.call("rbind",Eikon_get_data_pre)
      Eikon_get_data_Error <- lapply(Eikon_get_dataOuput, "[[", 2)
      ChunkRowStart <- cumsum(lapply(Eikon_get_data_pre, nrow))
      ChunkRowStart <- ChunkRowStart - ChunkRowStart[1]

      # Eikon_Error_Data <- do.call("rbind",Eikon_get_data_Error_pre, stringsAsFactors = FALSE)
    } else{
      Eikon_get_data <- Eikon_get_dataOuput[[1]]
      Eikon_get_data_Error <- Eikon_get_dataOuput[[2]]
    }

   # As the Python eikon get_data structure contains various list that are null these should be replaced
   #  with NA to prevent disasters when later handling the data.frame when using unlist Null elements
   #  are removed from the lists causing shorter vectors than expected.

     # Eikon_get_datawithoutNULL <- as.data.frame(sapply(Eikon_get_data, function(x) ifelse(x == "NULL", NA, x)), stringsAsFactors = FALSE )
     #
     # if(ncol(Eikon_get_datawithoutNULL) > 1){
     #   # sapply returns a data.frame of lists go back to a data.frame of vectors using unlist
     #   Eikon_get_dataFinal <- as.data.frame(lapply(Eikon_get_datawithoutNULL, unlist), stringsAsFactors = FALSE )
     # } else {
     #   Eikon_get_dataFinal <- as.data.frame(t(Eikon_get_datawithoutNULL), stringsAsFactors = FALSE)
     #   rownames(Eikon_get_dataFinal) <- NULL
     # }

   # return human readable names
   Eikon_get_dataFinal <- Eikon_get_data
   names(Eikon_get_dataFinal) <- EikonNameCleaner(names(Eikon_get_dataOuput[[1]][[1]]))
   Eikon_get_dataFinal <- make.true.NA_df(Eikon_get_dataFinal)

   EikonReturnError <-  EikonErrorProcessor(Eikon_get_data_Error, Chunked, ChunkRowStart)

   return(list( "PostProcessedEikonGetData" = Eikon_get_dataFinal
              , "Eikon_Error_Data" = EikonReturnError
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
#' @return data.frame containing 4 columns to_symbol_type, from_symbol_type, BestMatch (as defined by EIkon), error
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

  EikonSymbologyResult <- EikonSymbologyResult[[1]]

  #1. Check Input
  if ( to_symbol_type %in% names(EikonSymbologyResult)){
    BestMatch <- TRUE
  } else if( paste0(to_symbol_type, "s") %in% names(EikonSymbologyResult)){
    BestMatch <- FALSE
  } else{
    stop("ProcessSymbology retrieved input in wrong format")
  }

  #2. Run main function
  if(BestMatch){
    EikonSymbologyResult[[to_symbol_type]] <- rownames(EikonSymbologyResult)
    rownames(EikonSymbologyResult) <- NULL
    ReturnVar <- EikonSymbologyResult
  }
  else{
    returnList <- vector(mode = "list", length = nrow(EikonSymbologyResult))

    for (i in 1:nrow(EikonSymbologyResult)){
      returnList[[i]] <- if(!is.null(unlist(EikonSymbologyResult[i,4]))){

        data.frame( to_symbol_type = unlist(EikonSymbologyResult[[1]][[i]])
                    , from_symbol_type = unlist(EikonSymbologyResult[i,3])
                    , BestMatch = as.character(unlist(EikonSymbologyResult[i,2]))
                    , error =  unlist(EikonSymbologyResult[i,4])
                    , stringsAsFactors = FALSE
        )
      } else {
        data.frame( to_symbol_type = unlist(EikonSymbologyResult[[1]][[i]])
                    , from_symbol_type = unlist(EikonSymbologyResult[i,3])
                    , BestMatch = as.character(unlist(EikonSymbologyResult[i,2]))
                    , stringsAsFactors = FALSE
        )
      }
    }
    ReturnVar <- do.call("rbind", returnList)
    ReturnVar <- make.true.NA_df(ReturnVar)
    names(ReturnVar)[names(ReturnVar) == 'to_symbol_type'] <- to_symbol_type
    names(ReturnVar)[names(ReturnVar) == 'from_symbol_type'] <- from_symbol_type
  }

  #3. return output
  return(ReturnVar)
}





#' Check if an element of list exists or is NULL
#'
#' @param list a list
#' @param index integer, indicating proposed list location
#'
#' @return boolean
#' @export
#'
#' @examples
#' Testlist <- list(NULL, 1)
#' indexExists(Testlist, 1)
indexExists <- function(list, index) {
  tryCatch({
    list[[index]]  # Efficiency if element does exist and is large??
    TRUE
  }, error = function(e) {
    FALSE
  })
}

