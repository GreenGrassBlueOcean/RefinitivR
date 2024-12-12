#' Returns a list of news headlines
#'
#' @param EikonObject Connection Object result from EikonConnect()
#' @param query character array optional News headlines search criteria. The text can contain RIC codes, company names, country names and operators (AND, OR, NOT, IN, parentheses and quotes for explicit search…).Tip: Append 'R:' in front of RIC names to improve performance.
#' @param count integer, optional Max number of headlines retrieved.Value Range: [1-100].Default: 10
#' @param repository character, vector of characters, optionalPossible values: c("NewsWire","NewsRoom","WebNews") For "NewsRoom" and "WebNews" repositories a query must be defined.
#' @param date_from string or date, optional Beginning of date range. String format is: '\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. 2016-01-20T15:04:05.
#' @param date_to string or datetime, optional End of date range. String format is: '\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. 2016-01-20T15:04:05.
#' @param raw_output boolean if TRUE provide only the raw downloaded info from Eikon
#' @param debug boolean if TRUE prints out the python call to the console
#'
#' @return  Returns a data frame of news headlines with the following columns:
#' \itemize{
#'  \item"Index": Timestamp of the publication time
#'  \item"version_created": Date of the latest update on the news
#'  \item"text": Text of the Headline
#'  \item"story_id": Identifier to be used to retrieve the full story using the get_news_story legacy
#'  \item"source_code": Second news identifier
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'  Eikon <- Refinitiv::EikonConnect()
#'  headlines <- EikonGetNewsHeadlines( EikonObject = Eikon
#'                                    , query = c("R:MSFT.O", "R:AAPL.O") , count = 2, debug = TRUE)
#' }
#'
#' \dontrun{
#'   EikonJson <- RefinitivJsonConnect()
#'   headlines <- EikonGetNewsHeadlines( EikonObject = EikonJson, debug = TRUE
#'                                     , query = "R:MSFT.O", count = 2)
#' }
EikonGetNewsHeadlines <- function(EikonObject = EikonConnect()
                                 , query = NULL,count = 10L
                                 , repository = c("NewsWire","NewsRoom","WebNews")
                                 , date_from = NULL, date_to = NULL
                                 , raw_output = FALSE, debug = FALSE){

  if(is.null(query)){
    query <- ""
  }


  RawHeadlinesList <- as.list(rep(NA, times = length(query)))

  DownloadCoordinator <- data.frame( index = 1:length(query)
                                     , succes =  rep(FALSE, length(query))
                                     , retries = rep(0L, length(query))
                                     , stringsAsFactors = FALSE)

  while(!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)){

    HeadlinesTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

    for (j in HeadlinesTryList){
      RawHeadlinesList[[j]] <- try({
        retry(EikonObject$get_news_headlines( query = query[j]
                                              , count = as.integer(count)
                                              , repository = paste0(repository, collapse = ",")
                                              , date_from = date_from
                                              , date_to = date_to
                                              , raw_output = TRUE
                                              , debug = debug
        ),max =2)})
      Sys.sleep(time = 0.1)

      if (!identical(RawHeadlinesList[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE }

      if(debug){
        message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
      }
    }

    DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1

  }

  if(any(DownloadCoordinator$retries > 4L)){
    stop("EikonGetNewsHeadlines downloading data failed")
  }

  if(raw_output){
    return(RawHeadlinesList)
  } else {
    Return_DT <- lapply( X = RawHeadlinesList
                       , FUN = function(x){data.table::rbindlist(l=x$headlines,use.names = TRUE, fill = TRUE)}
                       ) |> data.table::rbindlist(use.names = TRUE, fill = TRUE, idcol = "query" )

    Return_DT$query <- query[Return_DT$query]
    Return_df <- data.table::setDF(Return_DT)
    return(Return_df)
  }
}


#' Return a single news story corresponding to the identifier provided in story_id
#'
#' @param EikonObject Connection Object result from EikonConnect()
#' @param story_id The story id. The story id is a field you will find in every headline you retrieved with the legacy get_news_headlines
#' @param raw_output boolean if TRUE provide only the raw downloaded info from Eikon
#' @param debug boolean if TRUE prints out the python call to the console
#' @param renderHTML boolean if TRUE renders HTML output file for use in website defaults to FALSE
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'  EikonJson <- RefinitivJsonConnect()
#'  headlines <- EikonGetNewsHeadlines(EikonObject = EikonJson
#'                                    , query = "R:MSFT.O", count = 2)
#'  stories <- EikonGetNewsStory(story_id = headlines$storyId
#'  , EikonObject = EikonJson)
#'
#' }
#'
#' \dontrun{
#'   Eikon <- Refinitiv::EikonConnect()
#'   story_id <- "urn:newsml:newswire.refinitiv.com:20230829:nRTVm1b2r:5"
#'   stories_RD <- EikonGetNewsStory(story_id = story_id
#'   , EikonObject = Eikon, debug = TRUE, raw_output  = FALSE)
#'
#'   EikonJson <- RefinitivJsonConnect()
#'   stories_JSON <- EikonGetNewsStory(story_id = story_id
#'   , EikonObject = EikonJson, debug = TRUE, raw_output  = FALSE)
#'
#'  identical(stories_RD, stories_JSON)
#' }
EikonGetNewsStory <- function(EikonObject = EikonConnect()
                             , story_id = NULL, raw_output = FALSE, debug=FALSE, renderHTML = FALSE){



  if(is.null(story_id)){
    stop("Parameter story_id has to be supplied and cannot be empty")
  }

  htmlFile <- NULL
  on.exit(unlink(htmlFile))

  EikonNewsList <- as.list(rep(NA, times = length(story_id)))

  DownloadCoordinator <- data.frame( index = 1:length(story_id)
                                   , succes =  rep(FALSE, length(story_id))
                                   , retries = rep(0L, length(story_id))
                                                   , stringsAsFactors = FALSE)

 while(!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)){

  NewsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

  for (j in NewsTryList){
    EikonNewsList[[j]] <- try({
       retry(EikonObject$get_news_story(story_id = story_id[j]
                                       , debug = debug
                                       , raw_output = raw_output),max =2)})
       Sys.sleep(time = 0.1)

  if (!identical(EikonNewsList[[j]], NA)){DownloadCoordinator$succes[j] <- TRUE }

  if(debug){
       message(paste0("Download Status:\n", paste0(capture.output(DownloadCoordinator), collapse = "\n"), collapse = "\n") )
       }
   }

  DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] <- DownloadCoordinator$retries[which(!DownloadCoordinator$succes)] + 1

  }

  if(any(DownloadCoordinator$retries > 4L)){
    stop("EikonGetNewsStory downloading data failed")
  }

  if(!raw_output){

    # Process the downloaded news stories
    Returnlines <- lapply(EikonNewsList, function(x) {
      if(is.list(x) && "story" %in% names(x)){
        return(x$story$storyHtml)
      } else if(is.list(x) && "webURL" %in% names(x)){
        return(x$webURL)
      } else {
        return(x)
      }
    }) |> unlist()

    if(renderHTML){

    # Normalize the temporary directory path to use forward slashes
    normalized_tempdir <- normalizePath(tempdir(check = TRUE), winslash = "/")
    if(debug){
      message(paste("Normalized temporary directory:", normalized_tempdir))
    }

    # Create a unique temporary subdirectory
    dir <- tempfile(tmpdir = normalized_tempdir)
    if(debug){
      message(paste("Creating temporary directory:", dir))
    }
    dir_created <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if(!dir_created){
      stop(sprintf("Failed to create temporary directory: %s", dir))
    }

    htmlFile <- file.path(dir, paste0("index.html"))
    if(debug){
      message(paste("HTML file path:", htmlFile))
    }


    Newslines <- lapply(EikonNewsList, function(x) {
      if(is.list(x) && "story" %in% names(x)){
        return(c(
          x$story$headlineHtml,
          x$story$storyHtml,
          x$story$storyInfoHtml
        ))
      } else if(is.list(x) && "webURL" %in% names(x)){
        return(x$webURL)
      } else {
        return(x)
      }
    }) |> unlist()

    #if(renderHTML){

    # Write the news content to the HTML file
    if(debug){
      message("Writing news content to HTML file...")
    }
    write_result <- try(writeLines(Newslines, con = htmlFile), silent = TRUE)
    if(inherits(write_result, "try-error")){
      stop(sprintf("Failed to write to HTML file: %s", htmlFile))
    }

    if(debug){
      message(sprintf("Successfully wrote to HTML file: %s", htmlFile))
    }

    # Open the HTML file in RStudio viewer or default browser
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("viewer")){
      if(debug){
        message("Opening HTML file in RStudio viewer...")
      }
      rstudioapi::viewer(htmlFile)

    } else {
      if(debug){
        message("Opening HTML file in default browser...")
      }
      utils::browseURL(htmlFile)

    }
    }
    return(Returnlines)
  }

  return(EikonNewsList)
}
