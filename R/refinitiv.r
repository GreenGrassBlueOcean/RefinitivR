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



#' check to see if installation of python module is succesfull
#'
#' @param PyhtonModuleName name of pythonModule
#' @param InstallationStat status of other installations defaults to NA
#' @param envname defaults to r-eikon
#' @param python_path
#'
#' @return data.frame with installation status
#' @noRd
#'
#' @examples
#' \dontrun{
#' CheckInstallationResult(PyhtonModuleName = "wrongmodule")
#' }
CheckInstallationResult <- function(PyhtonModuleName, InstallationStat = NULL,envname = "r-eikon", python_path = NULL){

  if(is.null(InstallationStat)){
    InstallationStat <- data.frame(Python_Package = c("eikon", "refinitiv-dataplatform", "refinitiv-data" )
                                   ,status = c(NA,NA,NA))
  }

  if(is.null(python_path)){
    CondaEnvironments <- reticulate::conda_list()
    python_path <- CondaEnvironments[CondaEnvironments$name == envname, ]$python
  }

  try(reticulate::use_condaenv(condaenv = envname))

  PyhtonModuleNameCheck <- gsub(PyhtonModuleName, pattern = "-", replacement = ".")
  if(!(reticulate::py_module_available(PyhtonModuleNameCheck))){
    warning(paste0("Installation of python module ", PyhtonModuleName, " failed"))
    Status <- "failed"
  } else {
    message(paste0("Installation of python module ", PyhtonModuleName, " successful"))
    Status <- "available"
  }
  try(InstallationStat[which(InstallationStat$Python_Package==PyhtonModuleName),]$status <- Status, silent = TRUE)
  return(InstallationStat)
}






#' Check if Conda exists, if not instals miniconda, add the python eikon module to the python environment r-eikon
#'
#' This function can also be used to update the required python packages so that you can always use the latest version of the pyhton packages numpy and eikon.
#' For a pure reinstall of miniconda and the refinitiv python libraries set set reset = TRUE and update = FALSE
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment. Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
#' @param conda  The path to a conda executable. Use "auto" to allow reticulate to automatically find an appropriate conda binary. See Finding Conda in the reticulate package for more details
#' @param envname the name for the conda environment that will be used, default  r-eikon. Don't Change!
#' @param update boolean, allow to rerun the command to update the miniconda environment and the packages required to update the python packages numpy,eikon, and refinitiv dataplatform defaults to TRUE
#' @param reset boolean, this will remove the miniconda r-eikon environment and reinstall miniconda, the conda environment and relevant packages.
#' @param restart_session boolean, Restart R session after installing (note this will only occur within RStudio).
#'
#' @return None
#' @importFrom utils installed.packages
#' @importFrom here here
#' @importFrom rstudioapi restartSession
#' @importFrom rstudioapi hasFun
#' @export
#'
#' @examples
#' \dontrun{
#' install_eikon()
#' }
#'
#' \dontrun{
#' # when you get the error the refinitiv library cannot
#' # be found anymore or errors during installation:
#' install_eikon(update = FALSE, reset = TRUE)
#' }
install_eikon <- function(method = "conda", conda = "auto", envname= "r-eikon", update = TRUE, reset = FALSE,restart_session = TRUE) {
#"r-eikon"

  #helper functions ====
  InstallPythonModule <- function( PyModuleName, AuxilliaryPackages, envname
                                   , method, conda, update){

    # reticulate::virtualenv_create(python = reticulate::conda_python(), envname = PyModuleName, packages = c(PyModuleName, AuxilliaryPackages))
    print(reticulate::conda_binary())
    #reticulate::use_condaenv(condaenv = "r-reticulate", conda = "C:\\Users\\LaurensVdb\\AppData\\Local\\miniconda3\\Scripts\\conda.exe")
    # options(reticulate.conda_binary = "C:\\Users\\LaurensVdb\\AppData\\Local\\miniconda3\\Scripts\\conda.exe")
    if(!reticulate::py_module_available(gsub(PyModuleName, pattern = "-", replacement = ".")) || update ) {
      message(paste("installing ", PyModuleName))
      try(reticulate::py_install(packages = c(PyModuleName, AuxilliaryPackages)
                                    , envname = envname,  method = method
                                    , conda = conda, update = update,pip=TRUE
                                    #, pip_options = ("--user")
      ))
    # for now also install in r-reticulate as long
      # as https://github.com/rstudio/reticulate/issues/1147 is not resolved
      # try(reticulate::conda_install( packages = c(PyModuleName, AuxilliaryPackages)
      #                                , envname = "r-reticulate",  method = method
      #                                , conda = conda, update = update, pip = TRUE
      #                                #, pip_options = ("--user")
      # ))
      # reticulate::use_condaenv(condaenv = "r-reticulate", conda = "C:\\Users\\LaurensVdb\\AppData\\Local\\miniconda3\\Scripts\\conda.exe")
     }
  }

  #check input ====
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }


  # some special handling for windows
  if (Sys.info()["sysname"] == "Windows") {

    # avoid DLL in use errors
    if (reticulate::py_available()) {
      stop("You should call install_eikon() only in a fresh ",
           "R session that has not yet initialized Refinitiv (this is ",
           "to avoid DLL in use errors during installation)")
    }
  }


  if(!is.logical(reset)){
    stop(paste("reset variable should be TRUE or FALSE but has currently value", reset))
  }
  if(!is.logical(update)){
    stop(paste("update variable should be TRUE or FALSE but has currently value", update))
  }

  if(CondaExists() && (envname %in% reticulate::conda_list()$name) && reset){
    message(paste0("Removing conda environment ", envname))
    reticulate::conda_remove(envname = envname, conda = conda)
  }

  InstallationStatus <- data.frame(Python_Package = c( "refinitiv-data" ) #"eikon", "refinitiv-dataplatform",
                                   ,status = c(NA))


  #setup reticulate/conda ====




  # Check if a conda environment exists and install if not available
  if (CondaExists() == FALSE || reset ) {
    if(reset){
      message("uninstalling MiniConda")
      Sys.unsetenv("RETICULATE_PYTHON")
      try(reticulate::miniconda_uninstall(), silent = TRUE)
    }

    tryCatch({ message("installing MiniConda")
               reticulate::install_miniconda(update = update, force = TRUE)},
        error=function(cond) {
          message(cond)
          message("if this fails try running studio with elevated permissions/as administrator")
          stop("Miniconda installation failed, stopping install_eikon")},
        warning=function(cond) {
          message("Miniconda installation gave the folowing original warning message:")
          message(cond)
    })

  } else if(update){
    message("updating conda environment")
    # resolve conda
    reticulate::miniconda_update()
  }

  if(!CondaExists()){
    stop("MiniConda does not seems to be installed, install_eikon cannot continu because miniconda is requred")

    }


  #conda update -n base -c defaults conda
  CondaEnvironments <- reticulate::conda_list()
  print(CondaEnvironments)

  if (!(envname %in% reticulate::conda_list()$name)) {
     py_version <- "3.10"
     reticulate::conda_create(envname = envname, python_version = py_version )
     CondaEnvironments <- reticulate::conda_list()
     print(CondaEnvironments)
  }

  try(reticulate::use_miniconda(condaenv = envname), silent = TRUE)

  ## section installing refinitiv-data ----

  InstallPythonModule(PyModuleName = "refinitiv-data"
                      , AuxilliaryPackages = c("httpx", "numpy", "pandas", "nest-asyncio", "scipy", "tabulate")
                      , envname = envname, method = method, conda = conda, update = update)


  ## section installing Eikon ----

  # InstallPythonModule(PyModuleName = "eikon"
  #                    , AuxilliaryPackages = c()
  #                    , envname = envname, method = method, conda = conda, update = update)




  ## section installing refinitiv-dataplatform ----

  # InstallPythonModule(PyModuleName = "refinitiv-dataplatform"
  #                      , AuxilliaryPackages = c()
  #                      , envname = envname, method = method, conda = conda, update = update)

  # show installation results ----
  print(envname)
  python_path <- CondaEnvironments[CondaEnvironments$name == envname, ]$python

  #Verify that eikon is really available
  # InstallationStatus <- CheckInstallationResult("eikon", InstallationStatus,envname, python_path)
  #
  # #Verify that rdp is really available
  # InstallationStatus <- CheckInstallationResult("refinitiv-dataplatform", InstallationStatus,envname, python_path)

  #Verify that rdp is really available
  InstallationStatus <- CheckInstallationResult("refinitiv-data", InstallationStatus,envname, python_path)


  Sys.setenv(RETICULATE_PYTHON = here::here(python_path))

  print(reticulate::py_config())
  print(InstallationStatus, quote = TRUE, row.names = FALSE)
  message("Eikon/RD Python installation is finished, check log which modules were succesful")

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible()
}


#
# options(reticulate.conda_binary = "C:\\Users\\LaurensVdb\\AppData\\Local\\miniconda3\\_conda.exe")



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


#' Get and Set pythonmodule name and version as an R option
#'
#' @param PythonModule python module
#'
#' @return nothing options are set within global environment
#' @noRd
#' @keywords internal
#'
#' @examples
#'  test <- reticulate::import(module = "numpy")
#' GetandSetPyModuleNameandVersion(test)
#'
GetandSetPyModuleNameandVersion <- function(PythonModule){
    try(options(.RefinitivPyModuleName = as.character(reticulate::py_get_attr(x = PythonModule, name =  "__name__"))))
    try(options(.RefinitivPyModuleVersion = as.character(reticulate::py_get_attr(x = PythonModule, name =  "__version__"))), silent = TRUE)
    try(options(.RefinitivPyModuleType = class(PythonModule)))
    invisible()
}



#' Function to check through which Refinitiv Connection object requests are made
#'
#' @param verbose boolean defaults to TRUE
#'
#' @return named list
#' @export
#'
#' @examples
#' test <- PropertiesActiveRefinitivObject()
PropertiesActiveRefinitivObject <- function(verbose = TRUE){

  #0. helper functions ---
  NullChecker <- function(x){
    ifelse(is.null(x), "not loaded", x)
  }

  #1. Main function ----
  if(!is.logical(verbose)){
    stop(paste("parameter verbose should be logical, but is", verbose))
  }

  ModuleName <- getOption(".RefinitivPyModuleName")
  Version <- getOption(".RefinitivPyModuleVersion")
  Type <-  getOption(".RefinitivPyModuleType")

  if(verbose){
    message(paste("Refinitiv Connection method =",NullChecker(ModuleName) ,"\n",
                  "Version =", Version , "\n",
                  "Type =", Type
                  ))
  }

  return(list("name" = ModuleName, "version" = Version, "Type" = Type))
}


# Initialize Eikon Python api using reticulate -------------------------------

#' Initialize Eikon Python api
#'
#' @param Eikonapplication_port proxy port id
#' @param Eikonapplication_id Eikon api key
#' @param PythonModule character choose between Eikon (python),RD (python),JSON (direct JSON message without python)
#' @param TestConnection Boolean, TRUE or FALSE test connection after initiating contact with Eikon terminal
#' @param UUID optional character parameter for custom instruments, not necessary for regular requests
#'
#' @return a Python module that is an EikonObject
#' @export
#'
#'
#' @examples
#' \dontrun{
#' Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L
#' , PythonModule = "Eikon")
#' Eikon <- EikonConnect(Eikonapplication_id = "your key", Eikonapplication_port = 9000L
#' , PythonModule = "RDP")
#' }
EikonConnect <- function( Eikonapplication_id = NA , Eikonapplication_port = 9000L
                        , UUID = NA, PythonModule = "JSON", TestConnection = FALSE) {

  # 1. check input ----
  if (is.na(Eikonapplication_id)){
    try(Eikonapplication_id <- getOption(".EikonApiKey") )
    if(is.null(Eikonapplication_id)){stop("Please supply Eikonapplication_id")}
  }

  if (is.na(PythonModule)){
    try(PythonModule <- getOption(".RefinitivAPI") )
    if(is.null(PythonModule)){
      stop(paste("EikonConnect parameter PythonModule can only be RD (python) or JSON (direct JSON message) but is NA"))
      }
  }

  if(!is.logical(TestConnection)){
    stop("TestConnection should be TRUE or FALSE")
  }

  if(!(PythonModule %in% c("RD", "JSON", "Eikon"))){
    stop(paste("EikonConnect parameter PythonModule can only be RD (python) or JSON (direct JSON message) but is"
               , PythonModule))
  }

  if (is.na(UUID)){
    try(UUID <- getOption(".RefinitivUUID") )
  }

  #2. Run main programme ----
  options(.EikonApiKey = Eikonapplication_id)
  options(.EikonApplicationPort = Eikonapplication_port)
  options(.RefinitivAPI = PythonModule)
  options(.RefinitivUUID = UUID)

   # set virtual environment right
  # PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon") # import python eikon module

  if (.Options$.RefinitivAPI %in% c("Eikon", "RD")){
    if(!CondaExists()){stop("Conda/reticulate does not seem to be available please run install_eikon")}
    try(reticulate::use_miniconda(condaenv = "r-eikon"), silent = TRUE)
    PythonEK <- reticulate::import(module = "refinitiv.data.eikon") # import python eikon module
    PythonEK$set_app_key(app_key = .Options$.EikonApiKey)
    options(py_json = reticulate::import(module = "json"))
    GetandSetPyModuleNameandVersion(PythonEK)
  } else if(identical(.Options$.RefinitivAPI, "JSON")){

    PythonEK <- RefinitivJsonConnect()

  }

  if(TestConnection){
    # test Connection as set_app_key does not provide a interceptable error when no connection is available
    tryCatch({PythonEK$get_data(instruments = "SPY", fields = "DSPLY_NAME", raw_output=TRUE )
    }, error = function(e) {stop("Eikon/RDP Error: no proxy address identified. Check if Desktop is running.")})
  }

  return(PythonEK)
}


#' RD connection function to refinitiv Data libraries
#'
#' @param application_id refinitiv data api key
#' @param PythonModule character "JSON" or "RD"
#' @param UUID optional character parameter for custom instruments, not necessary for regular requests
#'
#' @return RD opbject
#' @export
#'
#' @examples
#' \dontrun{
#' rd <- RDConnect(application_id = "your key")
#' }
RDConnect <- function(application_id = NA, PythonModule = "JSON", UUID = NA) {

  # 1. check input ----
  if (is.na(application_id)){
    try(application_id <- getOption(".EikonApiKey") )
    if(is.null(application_id)){stop("Please supply application_id")}
  }

  if(!(PythonModule %in% c("RD", "JSON"))){
    stop(paste("RDConnect parameter PythonModule can only be RD (python) or JSON (direct JSON message) but is"
               , PythonModule))
  }

  if (is.na(UUID)){
    try(UUID <- getOption(".RefinitivUUID") )
  }

  #Set options for furture use
  options(.RefinitivAPI = PythonModule)
  options(.RefinitivUUID = UUID)


  if(PythonModule == "JSON"){

    rd <- RefinitivJsonConnect()
    return(rd)


  } else {

  if (is.na(application_id)){
    try(application_id <- getOption(".EikonApiKey") )
    if(is.null(application_id)){stop("Please supply application_id")}
  }

  if(!CondaExists()){stop("Conda/reticulate does not seem to be available please run install_eikon or change parameter PythonModule to 'JSON'")}




  try(reticulate::use_miniconda(condaenv = "r-eikon"), silent = TRUE)
  #2. Run main programme ----
  options(.EikonApiKey = application_id)
  rd <- reticulate::import(module = "refinitiv.data", convert = FALSE, delay_load = FALSE)
  rd$open_session()
  GetandSetPyModuleNameandVersion(rd)

  return(rd)

  }
}

#' RDPConnect alias to provide backwards compatability with code written for RDP
#'
#' @rdname RDConnect
#' @examples
#' \dontrun{
#' rd <- RDPConnect(application_id = "your key")
#' }
#' @export
RDPConnect <- RDConnect


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
#'
#' EikonShowAttributes(EikonObject = RefinitivJsonConnect())
#'
#' }
EikonShowAttributes <- function(EikonObject){
  if(all(class(EikonObject) == c("python.builtin.module", "python.builtin.object"))){
     PossibleAttributes <- reticulate::py_list_attributes(EikonObject)
  } else if(!is.null(EikonObject)){
     PossibleAttributes <- names(EikonObject)
  } else {
    stop("EikonObject should be supplied in function EikonShowAttributes")
  }
  return(PossibleAttributes)
}




#' Convert Eikon formula's in human readable names
#'
#' @param names vector of data.frame column names
#' @param SpaceConvertor converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is "."
#'
#' @return a data.frame in which the Eikon formula's are replaced with the Eikon display Name which is the last part of the formula.
#' @keywords internal
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
#' Refinitiv:::EikonNameCleaner(c("Instrument","Company Name","RDN_EXCHD2","Operating MIC"))
EikonNameCleaner <- function(names, SpaceConvertor = "."){

  #0. Helper Functions ----
  firstup <- function(x) {
              substr(x, 1, 1) <- toupper(substr(x, 1, 1))
              x
  }

  #1. Main Function ----

  returnNames <- unlist(qdapRegex::rm_between(names, '/*', '*/', extract = TRUE))

  returnNames[is.na(returnNames)] <- stringi::stri_split(str = names[is.na(returnNames)], fixed = " ") |>
                                     lapply(FUN =  firstup) |>
                                     lapply(FUN = paste, collapse = " ")


  if(isTRUE(SpaceConvertor %in% c(".", "," , "-", "_"))){
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
#' @keywords internal
#' @references \url{https://developers.refinitiv.com/eikon-apis/eikon-data-api/docs?content=49692&type=documentation_item}
#'
#' @examples
#' \dontrun{"internal function no examples"}
EikonChunker <- function(RICS, Eikonfields = NULL, MaxCallsPerChunk = 12000, Duration = NULL, MaxRicsperChunk = 300) {

  if(is.null(RICS)){
    stop("RICS have to be supplid other wise no api call can be made")
  }

  #clean out NA RICS
  RICS <- RICS[!is.na(RICS)]

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
#'
#' \dontrun{
#'   EikonJson <- RefinitivJsonConnect()
#'   ex1 <- EikonGetTimeseries(EikonObject = EikonJson, rics = c("MMM", "III.L"),
#'                    start_date = "2020-01-01T01:00:00",
#'                    end_date = paste0(Sys.Date(), "T01:00:00"), verbose = TRUE)
#' }
#'
#'
#'
#'
#'
EikonGetTimeseries <- function(EikonObject, rics, interval = "daily", calender = "tradingdays", corax = "adjusted", fields = c('TIMESTAMP', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE')
                              , start_date = "2020-01-01T01:00:00", end_date = paste0(Sys.Date(), "T01:00:00"), cast = TRUE, time_out = 60, verbose = FALSE, raw_output = FALSE){

  # Make sure that Python object has api key and change timeout
  # EikonObject$set_timeout(timeout = time_out)
  try(EikonObject$set_app_key(app_key = .Options$.EikonApiKey), silent = TRUE)

  #In case no rics are supplied return nothing
  if(is.null(rics)){
    warning("no rics are supplied to EikonGetTimeseries")
    return(data.frame())
  }

  #make sure that TIMESTAMP is in fields if fields is not null
  if(!is.null(fields)){
    fields <- unique(c("TIMESTAMP", fields))
  }

  # Make sure that monthly economic time series are returned
  if(any(grepl(pattern = "=", x = rics))){
    fields <- unique(c("VALUE", fields))
  }


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
                                                               , "\t, start_date =  \"", format(start_date, format = "%Y-%m-%dT%H:%M:%S"), "\"\n"
                                                               , "\t, end_date =  \"", format(end_date, format =  "%Y-%m-%dT%H:%M:%S"), "\"\n"
                                                               , "\t, normalize = True\n\t)"
    )
    )}

    if(is.null(fields) | length(fields) == 0L ){
          retry(EikonObject$get_timeseries( rics = ChunckedRics[[j]]
                                          , interval = interval
                                          , calendar = calender
                                          , fields = c()
                                          , start_date = format(start_date, format = "%Y-%m-%dT%H:%M:%S")
                                          , end_date = format(end_date, format =  "%Y-%m-%dT%H:%M:%S")
                                          , normalize = TRUE
                                          , raw_output = TRUE
                                          , corax = corax
                                          )


          )} else {
            retry(EikonObject$get_timeseries( rics = ChunckedRics[[j]]
                                            , interval = interval
                                            , calendar = calender
                                            , fields = fields
                                            , start_date = format(start_date, format = "%Y-%m-%dT%H:%M:%S")
                                            , end_date = format(end_date, format = "%Y-%m-%dT%H:%M:%S")
                                            , normalize = TRUE
                                            , raw_output = TRUE
                                            , corax = corax
            ))

      }
    })
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
#'
#'
#' \dontrun{
#' EikonJson <- RefinitivJsonConnect()
#' ex1 <- EikonGetData(EikonObject = EikonJson, rics = c("MMM", "III.L"),
#'              Eikonformulas = c("TR.PE(Sdate=0D)/*P/E (LTM) - Diluted Excl*/"
#'              , "TR.CompanyName"), verbose = TRUE)
#'
#' }
#'
#'
EikonGetData <- function(EikonObject, rics, Eikonformulas, Parameters = NULL, raw_output = FALSE, time_out = 60, verbose = FALSE, SpaceConvertor = "."){

#Make sure that Python object has api key
try(EikonObject$set_app_key(app_key = .Options$.EikonApiKey), silent = TRUE)
# EikonObject$set_timeout(timeout = time_out) #add timeout to reduce chance on timeout error chance.


# Divide RICS in chunks to satisfy api limits
ChunckedRics <- EikonChunker(RICS = rics, Eikonfields = Eikonformulas, MaxRicsperChunk = 200)


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



  if (!identical(EikonDataList[[j]], NA) ){DownloadCoordinator$succes[j] <- TRUE }

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
#'
#' \dontrun{
#'  EikonJson <- RefinitivJsonConnect()
#'  ex1 <- EikonGetSymbology(EikonObject = EikonJson, symbol =  "AAPL.O"
#'  , to_symbol_type = "ISIN" )
#' }
#'
#'
#'
EikonGetSymbology <- function( EikonObject, symbol, from_symbol_type = "RIC", to_symbol_type = c('CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker', 'lipperID', 'IMO', 'OAPermID')
                               , bestMatch = TRUE, time_out = 60, verbose = FALSE, raw_output = FALSE){

  #Make sure that Python object has api key
  try(EikonObject$set_app_key(app_key = .Options$.EikonApiKey), silent = TRUE)

  # Divide symbols in chunks to satisfy api limits
  ChunckedSymbols <- EikonChunker(RICS = symbol, Eikonfields = to_symbol_type)

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
                                     , best_match = bestMatch))


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
#' @importFrom utils str
#' @return boolean
#' @keywords internal
#'
#' @examples
#' Refinitiv:::InspectRequest(data.frame(), functionname = "test")
#' Refinitiv:::InspectRequest(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test")
InspectRequest <- function(df, functionname, verbose = TRUE){
  if(!verbose){
    return(NULL)
  }


  try(message(class(df)))
  # try(message(lapply(df, class)))
  try(message(names(df)))
  try(message(lapply(df, names)))

  if( is.logical(df) && is.na(df) ){
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




