# Build a test object that can

# 1. receive dynamically receive information and check if this is correct if this is correct
# 2. Send back a result based on this input


#' Checks which api can be used for testing, live or replay.
#'
#' @param testMode character write or replay
#'
#' @return EikonObject either a RealEikon object or unittesting EikonObject that mimics Eikon
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_Eikonapi("replay")
#' check_Eikonapi("write")
#' }
check_Eikonapi <- function(testMode = "replay", ExecutionMode = "Eikon") {

  if (is.null(getOption(".EikonApiKey")) && testMode != "write"  ) {
    warning("API not available, using offline database for testing")
    ReturnObject <- EikonTestObject (testMode = "replay")
    options(.RefinitivPyModuleName = "testing object")
    options(.RefinitivPyModuleVersion = "NA")
    options(.RefinitivPyModuleType = "mimic connection using previously downlaoded data using archivist package")
  } else if (!is.null(getOption(".EikonApiKey")) && testMode == "write"  ) {
    warning("overwriting internal test database")
    ReturnObject <- EikonTestObject (testMode = "write")
  } else {
    print("Eikon API available performing test")
    if(ExecutionMode == "Eikon" ){
      ReturnObject <- EikonConnect(PythonModule = "Eikon")
    } else if(ExecutionMode == "RD"){
      ReturnObject <- RDConnect(PythonModule = "RD")
    } else if(ExecutionMode == "RDP"){
      ReturnObject <- RDPConnect(PythonModule = "RDP")
    }
  }
  return(ReturnObject)
}

#' Dummy Eikon Object used for testing
#'
#' This Object can record and replay previously recorded python api responses using archivist package
#'
#' @param mode character replay (replay previously downloaded) or
#'
#' @return Eikon Unit testing Object
#' @noRd
#'
#' @examples
#' EikonTestObject("write")
#' EikonTestObject("replay")
EikonTestObject  <- function(testMode  = "replay"){

  #0. Helper functions -----
  StoreOrRetrievefromDB <- function(FunctionName, RealEikonObject, repodir, Input){
    Input[[1]] <- NULL
    #Make sure all arguments are evaluated before passing to the search api
    Arglist <- lapply(X = Input, FUN = function(x, env = parent.frame(4)){eval(x, envir=env)})
    InputHash <- digest::digest(Arglist, algo="md5", serialize=T)

    if(testMode  == "write"){
      #store input in archivist
      response <- do.call(RealEikonObject[[FunctionName]], args = Arglist)

      AlreadyinDBmd5 <- archivist::searchInLocalRepo(InputHash, repoDir = repodir)
      if(!identical(AlreadyinDBmd5, character(0))){
        print("cleaning up repo by deleting old data")
        archivist::rmFromLocalRepo(md5hash = AlreadyinDBmd5, repoDir = repodir)
      }

      OutputHash <- archivist::saveToLocalRepo(response, repoDir = repodir)
      archivist::addTagsRepo(md5hashes = OutputHash, repoDir = repodir, tags = InputHash)
    } else if(testMode  == "replay"){
      Repohashes <- data.table::as.data.table(archivist::showLocalRepo(repoDir = repodir, method = "tags"))
      Outputhash <-Repohashes[tag == InputHash,]
      response <- archivist::areadLocal(md5hash = Outputhash$artifact, repo = repodir)
    }

    return(response)}

  #1. Check parameters ----
  stopifnot(testMode  %in% c("write", "replay"))
  if(testMode  == "write"){
    RealEikon = Refinitiv::EikonConnect()
    RealRD = Refinitiv::RDConnect()
  }

  repodir = paste0(testthat::test_path(),"/RefTestData")

  #2. Main function ----
  TEST_EK <- rlang::env( set_app_key =  function(app_key = Eikonapplication_id){options(.EikonApiKey = app_key)}
                         , get_app_key = function(){return(getOption(".EikonApiKey"))}

                         , get_timeseries = function(rics, interval, calendar, fields
                                                     , start_date, end_date, corax, normalize, raw_output){
                          response <- StoreOrRetrievefromDB( FunctionName = "get_timeseries"
                                                              , RealEikonObject = RealEikon
                                                              , repodir = repodir
                                                              , Input=as.list(match.call(expand.dots=FALSE))
                           )

                         }
                         , get_data = function( instruments, fields, parameters = NULL
                                                , debug, raw_output){
                           response <- StoreOrRetrievefromDB( FunctionName = "get_data"
                                                              , RealEikonObject = RealEikon
                                                              , repodir = repodir
                                                              , Input=as.list(match.call(expand.dots=FALSE))
                           )

                         }
                         , get_symbology = function( symbol, from_symbol_type
                                                     , to_symbol_type, raw_output
                                                     , debug, best_match){

                           response <- StoreOrRetrievefromDB( FunctionName = "get_symbology"
                                                              , RealEikonObject = RealEikon
                                                              , repodir = repodir
                                                              , Input=as.list(match.call(expand.dots=FALSE))
                           )
                         }
                         , search = function( query =  NULL, view = "SearchAll"
                                              ,  select = NULL, top = NULL, filter = NULL
                                              ,  boost= NULL, order_by = NULL, group_by = NULL
                                              ,  group_count = NULL, navigators = NULL, features = NULL){

                           response <- StoreOrRetrievefromDB( FunctionName = "search"
                                                              , RealEikonObject = RealEikon
                                                              , repodir = repodir
                                                              , Input=as.list(match.call(expand.dots=FALSE))
                           )
                         }
                         , get_search_metadata = function(RDP, searchView){
                           response <- StoreOrRetrievefromDB( FunctionName = "get_search_metadata"
                                                              , RealEikonObject = RealEikon
                                                              , repodir = repodir
                                                              , Input=as.list(match.call(expand.dots=FALSE))
                           )
                         }
                          , get_history = function(RD = RealRD
                                                     , universe = NULL, fields = NULL
                                                     , parameters = NULL
                                                     , interval = NULL
                                                     , start = NULL
                                                     , end = NULL
                                                     , adjustments = NULL
                                                     , count = NULL
                                                     , use_field_names_in_headers = NULL){
                            response <- StoreOrRetrievefromDB( FunctionName = "get_history"
                                                                , RealEikonObject = RealRD
                                                                , repodir = repodir
                                                                , Input=as.list(match.call(expand.dots=FALSE))
                             )

                         },  get_news_headlines = function(query = NULL
                                                           , count = NULL
                                                           , repository = NULL
                                                           , date_from = NULL
                                                           , date_to = NULL
                                                           , raw_output = TRUE
                                                           , debug = NULL){
                           response <- StoreOrRetrievefromDB( FunctionName = "get_news_headlines"
                                                            , RealEikonObject = RealEikon
                                                            , repodir = repodir
                                                            , Input=as.list(match.call(expand.dots=FALSE))
                           )
                         }, get_news_story = function( story_id = NULL
                                                     , debug = NULL
                                                     ) {
                           response <- StoreOrRetrievefromDB( FunctionName = "get_news_story"
                                                            , RealEikonObject = RealEikon
                                                            , repodir = repodir
                                                            , Input=as.list(match.call(expand.dots=FALSE))
                           )
                         }

  )
  return(TEST_EK)
}
