#' Returns a list of news headlines
#'
#' @param EikonObject Connection Object result from EikonConnect()
#' @param query character optional News headlines search criteria. The text can contain RIC codes, company names, country names and operators (AND, OR, NOT, IN, parentheses and quotes for explicit search…).Tip: Append 'R:' in front of RIC names to improve performance.
#' @param count integer, optional Max number of headlines retrieved.Value Range: [1-100].Default: 10
#' @param repository character, vector of characters, optionalPossible values: c("NewsWire","NewsRoom","WebNews") For "NewsRoom" and "WebNews" repositories a query must be defined.
#' @param date_from string or date, optional Beginning of date range. String format is: '\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. 2016-01-20T15:04:05.
#' @param date_to string or datetime, optional End of date range. String format is: '\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. 2016-01-20T15:04:05.
#' @param raw_output boolean if TRUE provide only the raw downloaded info from Eikon
#' @param debug boolean if TRUE prints out the python call to the console
#'
#' @return  Returns a data frame of news headlines with the following columns:
#' \itemize{
#'  \item{"Index" }{Timestamp of the publication time}
#'  \item{"version_created" }{Date of the latest update on the news}
#'  \item{"text" }{Text of the Headline}
#'  \item{"story_id" }{Identifier to be used to retrieve the full story using the get_news_story legacy}
#'  \item{"source_code" }{Second news identifier}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'  Eikon <- Refinitiv::EikonConnect()
#'  headlines <- EikonGetNewsHeadlines( EikonObject = Eikon
#'                                    , query = "R:MSFT.O", count = 2)
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

  RawHeadlines <- retry(EikonObject$get_news_headlines( query = query
                                                , count = as.integer(count)
                                                , repository = paste0(repository, collapse = ",")
                                                , date_from = date_from
                                                , date_to = date_to
                                                , raw_output = TRUE
                                                , debug = debug
                                                ))

  if(raw_output){
    return(RawHeadlines)
  } else {
    Return_df <- data.table::rbindlist(RawHeadlines$headlines, use.names = T, fill = T) |>
    data.table::setDF()
    return(Return_df)
  }
}


#' Return a single news story corresponding to the identifier provided in story_id
#'
#' @param EikonObject Connection Object result from EikonConnect()
#' @param story_id The story id. The story id is a field you will find in every headline you retrieved with the legacy get_news_headlines
#' @param raw_output boolean if TRUE provide only the raw downloaded info from Eikon
#' @param debug boolean if TRUE prints out the python call to the console
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'  EikonJson <- RefinitivJsonConnect()
#'  headlines <- EikonGetNewsHeadlines(EikonObject = EikonJson
#'                                    , query = "R:MSFT.O", count = 2)
#'  stories <- EikonGetNewsStory(story_id = headlines$storyId, EikonObject = EikonJson)
#'
#' }
#'
#' \dontrun{
#'   Eikon <- Refinitiv::EikonConnect()
#'   story_id <- "urn:newsml:newswire.refinitiv.com:20230829:nRTVm1b2r:5"
#'   stories <- EikonGetNewsStory(story_id = story_id, EikonObject = Eikon, debug = TRUE)
#' }
#'
#' testinglist <- list( EikonConnect(), check_Eikonapi(ExecutionMode = "Eikon"), RDConnect(), RefinitivJsonConnect())
#' story_id <- "urn:newsml:newswire.refinitiv.com:20231025:nNRAqewpdh:1"
#' stories <- vector(mode ="list", length = 4L)
#' for (i in testinglist){
#' stories[[i]] <- EikonGetNewsStory(story_id = story_id, EikonObject = i, debug = TRUE, raw_output=T)
#' }
EikonGetNewsStory <- function(EikonObject = EikonConnect()
                             , story_id = NULL, raw_output = FALSE, debug=FALSE){
  if(is.null(story_id)){
    stop("Parameter story_id has to be supplied and cannot be empty")
  }


  EikonNewsList <- as.list(rep(NA, times = length(story_id)))

  DownloadCoordinator <- data.frame( index = 1:length(story_id)
                                   , succes =  rep(FALSE, length(story_id))
                                   , retries = rep(0L, length(story_id))
                                                   , stringsAsFactors = FALSE)

 while(!all(DownloadCoordinator$succes) & !any(DownloadCoordinator$retries > 4L)){

  NewsTryList <- DownloadCoordinator$index[which(!DownloadCoordinator$succes)]

  for (j in NewsTryList){
    EikonNewsList[[j]] <- try({
       retry(EikonObject$get_news_story(story_id[j],debug),max =2)})
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
    # Newslines <- EikonNewsList[[1]]

    dir <- tempfile()
    dir.create(dir)
    htmlFile <- file.path(dir, "index.html")

    Newslines <- lapply( X = EikonNewsList
                       , FUN =  function(x){
                         if(is.list(x)){
                           return(c( x$story$headlineHtml
                                 , x$story$storyHtml
                                 , x$story$storyInfoHtml))
                           } else {return(x)}}
                       ) |> unlist()
    writeLines( Newslines, con = htmlFile)

    if (requireNamespace("rstudioapi", quietly = TRUE)
        && rstudioapi::hasFun("viewer")){
      rstudioapi::viewer(htmlFile)
    } else {
      utils::browseURL(htmlFile)
    }
  }

  return(EikonNewsList)
}
