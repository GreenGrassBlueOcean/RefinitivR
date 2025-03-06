#' Retrieve News Headlines from a Refinitiv RDP (JSON) Connection
#'
#' This function constructs an HTTP GET query to retrieve news headlines from the
#' Refinitiv RDP service. It builds a query string from the provided parameters using
#' \code{build_get_query_string()} and then sends the GET request via \code{send_json_request()}.
#'
#' The query parameters may include various tokens such as Reuters Instrument Codes (RIC),
#' language codes, explicit free-text tokens (using quotes), and date ranges. In addition,
#' the function supports pagination via a cursor.
#'
#' Examples of queries include:
#'
#' - **Explicit FreeText (use quotes):**
#'   Obtains headlines for stories containing the text "electric car" or "electric vehicle"
#'   \preformatted{
#'     rd_get_news_headlines(query = "\"electric car\" or \"electric vehicle\"")
#'   }
#'
#' - **SearchIn token (HeadlineOnly):**
#'   Obtains headlines for stories with "Reports" or "Announces" in their title, limiting the search to headlines only
#'   \preformatted{
#'     rd_get_news_headlines(query = "\"Reports\" or \"Announces\" and searchIn:HeadlineOnly")
#'   }
#'
#' - **SearchIn token (FullStory):**
#'   Obtains headlines for stories with "inflation" when searching in the full story text
#'   \preformatted{
#'     rd_get_news_headlines(query = "\"inflation\" and searchIn:FullStory")
#'   }
#'
#' - **Language Filter:**
#'   For French headlines:
#'   \preformatted{
#'     rd_get_news_headlines(query = "LFR")
#'   }
#'   For English headlines (disambiguated with the language prefix "L:"):
#'   \preformatted{
#'     rd_get_news_headlines(query = "L:EN")
#'   }
#'
#' - **Reuters Instrument Code (RIC):**
#'   \preformatted{
#'     rd_get_news_headlines(query = "MSFT.O")
#'   }
#'
#' - **Combination of RIC and Relevancy:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "MSFT.O", relevancy = "High")
#'   }
#'
#' - **Combination of Company Name, RIC and Relevancy:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "LEN and \"Microsoft\" and MSFT.O", relevancy = "High")
#'   }
#'
#' - **Most Read News (M:1RS):**
#'   \preformatted{
#'     rd_get_news_headlines(query = "M:1RS")
#'   }
#'
#' - **Newswire Specific RCS Codes (MRG):**
#'   \preformatted{
#'     rd_get_news_headlines(query = "MRG")
#'   }
#'
#' - **Explicit Token News Source (NS):**
#'   \preformatted{
#'     rd_get_news_headlines(query = "NS:RTRS or NS:PRN or NS:TWTR" )
#'   }
#'
#' - **Increasing the Limit:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "\"stock repurchase\"", limit = 50)
#'   }
#'
#' - **Pagination using Cursor:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "MSFT.O", cursor = "H4sIAAAAAAAA...", limit = 10)
#'   }
#'
#' - **Daterange using LAST syntax:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "MRG last 5 days")
#'   }
#'
#' - **Daterange with BETWEEN syntax:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "M:1RS BETWEEN 2024-03 AND 2024-04")
#'   }
#'
#' - **Daterange with explicit "from,to" syntax:**
#'   \preformatted{
#'     rd_get_news_headlines(query = "Major breaking news", dateFrom = "2024-04-13T00:00:00Z", dateTo = "2024-04-14T00:00:00Z")
#'   }
#'
#' @param query A character string or vector representing the search query.
#' @param limit An integer indicating the maximum number of headlines to retrieve.
#' @param sort An optional sort order (e.g., "NewToOld"). If not specified, the service default is used.
#' @param relevancy An optional relevancy filter (e.g., "All" or "High").
#' @param cursor An optional pagination cursor.
#' @param dateFrom An optional start date/time (ISO 8601 format).
#' @param dateTo An optional end date/time (ISO 8601 format).
#' @param raw_output If TRUE, returns the raw JSON response; otherwise, the response is processed.
#' @param debug If TRUE, prints debugging messages.
#' @param RDObject A connection object returned by \code{RefinitivJsonConnect()}. If not
#'   supplied, defaults to \code{RefinitivJsonConnect()}.
#'
#' @return The JSON response from the headlines endpoint as returned by \code{send_json_request()}.
#'
#' @export
rd_get_news_headlines <- function(RDObject   = RefinitivJsonConnect(),
                                  query      = NULL,
                                  limit      = 10L,
                                  sort       = "NewToOld",
                                  relevancy  = "All",
                                  cursor     = NULL,
                                  dateFrom   = NULL,
                                  dateTo     = NULL,
                                  raw_output = FALSE,
                                  debug      = TRUE) {
  if (is.null(query)) {
    query <- ""
  }
  orig_query <- query

  # Prepare a list to store responses
  HeadlinesList <- vector("list", length(query))
  DownloadCoordinator <- data.frame(
    index = seq_along(query),
    succes = FALSE,
    retries = 0L,
    stringsAsFactors = FALSE
  )

  # Retry until all queries have succeeded or maximum retries reached.
  while (!all(DownloadCoordinator$succes) && !any(DownloadCoordinator$retries > 1L)) {
    pending <- which(!DownloadCoordinator$succes)
    for (j in pending) {
      HeadlinesList[[j]] <- try({
        retry(
          RDObject$rd_get_news_headlines(
            query     = query[j],
            limit     = as.integer(limit),
            sort      = sort,
            relevancy = relevancy,
            cursor    = cursor,
            dateFrom  = dateFrom,
            dateTo    = dateTo,
            raw_output= TRUE,
            debug     = debug
          ),
          max = 2
        )
      })
      if (!identical(HeadlinesList[[j]], NA)) {
        DownloadCoordinator$succes[j] <- TRUE
      }
      Sys.sleep(0.1)
      if (debug) {
        message("Download Status:\n",
                paste(capture.output(DownloadCoordinator), collapse = "\n"))
      }
    }
    DownloadCoordinator$retries[!DownloadCoordinator$succes] <-
      DownloadCoordinator$retries[!DownloadCoordinator$succes] + 1
  }

  if (any(DownloadCoordinator$retries > 1L)) {
    stop("rd_get_news_headlines: retrieving data failed after multiple retries.")
  }

  if (raw_output) {
    return(HeadlinesList)
  }

  # Process each response: if the headlines are stored in data$headlines, use that;
  # otherwise assume data itself is a list of headline items.
  dtlist <- lapply(HeadlinesList, function(x) {
    if (!is.list(x) || is.null(x$data)) {
      return(data.table::data.table())
    }
    if (!is.null(x$data$headlines)) {
      return(data.table::rbindlist(x$data$headlines, fill = TRUE, use.names = TRUE))
    } else {
      return(data.table::rbindlist(x$data, fill = TRUE, use.names = TRUE))
    }
  })

  if (length(dtlist) == 0 || all(sapply(dtlist, nrow) == 0)) {
    return(data.frame())
  }

  out <- data.table::rbindlist(dtlist, fill = TRUE, use.names = TRUE, idcol = "query_index")
  if ("query_index" %in% names(out)) {
    query_index <- NULL
    out[, query := orig_query[out$query_index]]
    out[, query_index := NULL]
  } else {
    out[, query := orig_query[1]]
  }

  return(data.table::setDF(out))
}





#' Convert plain URLs in a string to clickable HTML links
#'
#' This internal helper function finds URL substrings in the input text and wraps them
#' in HTML anchor tags with a line break so that they become clickable when rendered.
#'
#' @param text A character string.
#'
#' @return A character string with plain URLs replaced by clickable links.
#'
#' @keywords internal
make_links_clickable <- function(text) {
  # The regex matches any substring starting with http:// or https:// up to the first whitespace or a double quote or '<'
  clickable <- gsub("((http[s]?://[^\\s\"<]+))",
                    "<a href=\"\\1\" target=\"_blank\">\\1</a><br/>",
                    text, perl = TRUE)
  return(clickable)
}

#' Retrieve Full News Story from a Refinitiv RDP (JSON) Connection
#'
#' This function retrieves a full news story identified by its story ID via a
#' Refinitiv JSON connection. In the RDP response the story text may be found in
#' different places:
#'
#' - If the response comes from the legacy UDF service, the story is expected in the
#'   \code{story$storyHtml} element.
#'
#' - If the response comes from the RDP service, the content will be located under
#'   \code{newsItem$contentSet}. In that case, the function first checks for HTML content
#'   in \code{newsItem$contentSet$inlineXML} (if available) and, if not, in
#'   \code{newsItem$contentSet$inlineData}.
#'
#' If \code{renderHTML} is TRUE, any plain URLs in the resulting HTML will be converted
#' to clickable links via a helper function \code{make_links_clickable()} (which you should
#' define elsewhere in your package), and the combined HTML is opened in the viewer.
#'
#' @param RDObject A connection object returned by \code{RefinitivJsonConnect()}. If not
#'   supplied, defaults to \code{RefinitivJsonConnect()}.
#' @param story_id Vector of story IDs.
#' @param raw_output If TRUE, returns the raw list of responses.
#' @param debug If TRUE, prints debug messages.
#' @param renderHTML If TRUE, the function will open the combined HTML in a browser viewer,
#'   and also return the HTML string.
#'
#' @return If \code{raw_output = FALSE} (the default), a character vector of the story HTML
#'   (or a single combined HTML string if \code{renderHTML = TRUE}). Otherwise, the raw list
#'   of responses.
#'
#' @importFrom rlang %||%
#' @export
#'
#' @examples
#' \dontrun{
#'   # Retrieve and process a story:
#'   RObj <- RefinitivJsonConnect()
#'   story_html <- rd_get_news_story(RObj, story_id = "urn:newsml:newsroom:20250209:nNRAvd1b4o:0")
#'
#'   # Retrieve the story and open it in your RStudio viewer:
#'   rd_get_news_story(RObj, story_id = "urn:newsml:newsroom:20250209:nNRAvd1b4o:0", renderHTML = TRUE)
#' }
rd_get_news_story <- function(RDObject   = RefinitivJsonConnect(),
                              story_id   = NULL,
                              raw_output = FALSE,
                              debug      = FALSE,
                              renderHTML = FALSE) {
  if (is.null(story_id)) {
    stop("rd_get_news_story: must supply 'story_id'")
  } else {
    story_id <- unique(story_id)
  }

  NewsList <- vector("list", length(story_id))
  DownloadCoordinator <- data.frame(
    index   = seq_along(story_id),
    success  = FALSE,
    retries = 0L,
    stringsAsFactors = FALSE
  )

  # Retry until all queries have succeeded or maximum retries reached.
  while (!all(DownloadCoordinator$success) && !any(DownloadCoordinator$retries > 4L)) {
    pending <- which(!DownloadCoordinator$success)
    for (j in pending) {
      NewsList[[j]] <- try({
        retry(
          RDObject$rd_get_news_story(
            story_id   = story_id[j],
            raw_output = raw_output,
            debug      = debug
          ),
          max = 2
        )
      })
      if (!identical(NewsList[[j]], NA)) {
        DownloadCoordinator$succes[j] <- TRUE
      }
      Sys.sleep(0.1)
      if (debug) {
        message("Download Status:\n", paste(capture.output(DownloadCoordinator), collapse = "\n"))
      }
    }
    DownloadCoordinator$retries[!DownloadCoordinator$succes] <-
      DownloadCoordinator$retries[!DownloadCoordinator$succes] + 1
  }

  if (any(DownloadCoordinator$retries > 4L)) {
    stop("rd_get_news_story: retrieving data failed after multiple retries.")
  }

  if (raw_output) {
    return(NewsList)
  }

  # Process each response.
  outvec <- vapply(
    X = NewsList,
    FUN = function(x) {
      if (is.list(x) && "story" %in% names(x)) {
        # Legacy UDF response: storyHtml should be present.
        return(x$story$storyHtml %||% "")
      } else if (is.list(x) && "webURL" %in% names(x)) {
        # If a hyperlink is provided.
        return(x$webURL %||% "")
      } else if (is.list(x) && "newsItem" %in% names(x)) {
        # RDP response: check for inlineXML, then inlineData.
        if (!is.null(x$newsItem$contentSet$inlineXML) &&
            !is.null(x$newsItem$contentSet$inlineXML$`$`)) {
          return(x$newsItem$contentSet$inlineXML$`$`)
        } else if (!is.null(x$newsItem$contentSet$inlineData) &&
                   !is.null(x$newsItem$contentSet$inlineData$`$`)) {
          return(x$newsItem$contentSet$inlineData$`$`)
        } else {
          return("")
        }
      } else {
        return("")
      }
    },
    FUN.VALUE = character(1)
  )

  if (renderHTML) {
    combined_html <- paste0(outvec, collapse = "<hr/>")
    # Convert plain URL substrings into clickable links.
    combined_html <- make_links_clickable(combined_html)
    tmpfile <- tempfile(fileext = ".html")
    writeLines(combined_html, con = tmpfile)
    if (debug) {
      message("Opening story content in browser: ", tmpfile)
    }
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("viewer")) {
      rstudioapi::viewer(tmpfile)
    } else {
      utils::browseURL(tmpfile)
    }
    return(combined_html)
  } else {
    return(outvec)
  }
}


#' Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection
#'
#' This function retrieves the top news packages from the Refinitiv RDP service using the
#' endpoint `/data/news/v1/top-news`. The endpoint returns a nested JSON structure containing
#' top news groups. Each group contains one or more pages, where each page represents a specific
#' news category.
#'
#' **Overview of Top News Groups and Pages:**
#'
#' - **Main**: Typically includes pages such as "Front Page", "Central Banks & Global Economy",
#'   "Sustainable Finance", "World News", and "Regulation & Compliance".
#'
#' - **Breakingviews**: Generally contains the page "Reuters Breakingviews".
#'
#' - **Banking & Finance**: Often includes pages like "Financial Services", "Investment Banking",
#'   "IFR", and "Digital Finance & Crypto".
#'
#' - **Markets**: Usually features pages such as "Global Markets", "Foreign Exchange",
#'   "Fixed Income", "Emerging Markets", and "IFR".
#'
#' - **Commodities**: Contains pages like "Commodities", "Energy", "Metals",
#'   "Agricultural Commodities", and "Global Gas, Power & LNG".
#'
#' - **Industries**: Contains pages such as "Technology, Media & Telecoms",
#'   "Heavy Industry & Transport", "Consumer & Retail", and "Healthcare & Pharma".
#'
#' - **Regional**: Groups news by region with pages such as "Australia & New Zealand",
#'   "Japan & the Koreas", "Greater China", "Southeast Asia", "India & South Asia",
#'   "Middle East & Africa", "Europe & Russia", "United Kingdom", "Latin America",
#'   "United States", and "Canada".
#'
#' - **National Languages**: Offers news in various languages with pages such as
#'   "日本語トップニュース", "路透中文新闻", "Deutschland", "L’essentiel de l'actualité",
#'   "Brasil", and "Россия".
#'
#' - **Sports & Lifestyle**: Contains pages like "Sport" and "Lifestyle & Entertainment".
#'
#' - **AWP Top News**: Includes pages such as "AWP German Top News" and "AWP French Top News".
#'
#' In addition to returning key fields from the top news packages (group, page name, revision information,
#' and the **topNewsId**), this function now makes an additional GET call for each page by calling
#' `/data/news/v1/top-news/<topNewsId>`. This call retrieves the actual story details including the story
#' identifier (in **storyId**), the title (in **text**), and a summary (in **snippet**) that can subsequently
#' be used with \code{rd_get_news_story}.
#'
#' @param RDObject A connection object returned by \code{RefinitivJsonConnect()}. If not supplied,
#'   defaults to \code{RefinitivJsonConnect()}.
#' @param group Optional character string (or regular expression) to filter the top news groups by name.
#' @param page Optional character string (or regular expression) to filter the pages by name.
#' @param raw_output If TRUE, returns the raw JSON response from the top-news endpoint.
#' @param debug If TRUE, prints debugging messages.
#'
#' @return A data frame with one row per top news page and the following columns:
#'   \code{group}, \code{page_name}, \code{po}, \code{revisionId}, \code{revisionDate},
#'   \code{topNewsId}, \code{storyId}, \code{title} (news headline), and \code{snippet} (news summary).
#'
#' @importFrom data.table rbindlist setDF
#' @export
#'
#' @examples
#' \dontrun{
#'   # Retrieve all top news packages along with the actual story details:
#'   top_news <- rd_get_top_news()
#'
#'   # Retrieve only the "Main" group:
#'   top_news_main <- rd_get_top_news(group = "Main")
#'
#'   # Retrieve only pages whose name contains "Front":
#'   top_news_front <- rd_get_top_news(page = "Front")
#' }
rd_get_top_news <- function(RDObject = RefinitivJsonConnect(),
                            group = NULL,
                            page = NULL,
                            raw_output = FALSE,
                            debug = FALSE) {
  # Define the top-news endpoint for RDP.
  EndPoint <- "data/news/v1/top-news"

  # Send the GET request for top news packages.
  response <- send_json_request(service = "rdp",
                                request_type = "GET",
                                EndPoint = EndPoint,
                                debug = debug)

  if (debug) {
    message("Top news endpoint response received.")
  }

  # If raw_output is requested, return the raw JSON response.
  if (raw_output) {
    return(response)
  }

  # If no data is found, return an empty data frame.
  if (is.null(response$data)) {
    if (debug) message("No data found in response.")
    return(data.frame())
  }

  # Parse the nested JSON structure.
  dt <- data.table::rbindlist(lapply(response$data, function(g) {
    if (is.null(g$pages)) return(NULL)
    data.table::rbindlist(lapply(g$pages, function(p) {
      data.table::data.table(
        group        = g$name,
        page_name    = p$name,
        po           = p$po,
        revisionId   = p$revisionId,
        revisionDate = p$revisionDate,
        topNewsId    = p$topNewsId
      )
    }), fill = TRUE)
  }), fill = TRUE)

  # Apply filtering if requested.
  if (!is.null(group)) {
    dt <- dt[grepl(group, dt$group, ignore.case = TRUE)]
  }
  if (!is.null(page)) {
    dt <- dt[grepl(page, dt$page_name, ignore.case = TRUE)]
  }

  # For each topNewsId, retrieve the additional story details.
  details <- lapply(dt$topNewsId, function(tnid) {
    # Construct endpoint for an individual top news page.
    top_story_endpoint <- paste0("data/news/v1/top-news/", tnid)
    if (debug) message("Requesting story details for topNewsId: ", tnid)
    story_resp <- try(send_json_request(service = "rdp",
                                        request_type = "GET",
                                        EndPoint = top_story_endpoint,
                                        debug = debug), silent = TRUE)
    if (inherits(story_resp, "try-error") || is.null(story_resp$data)) {
      if (debug) message("Failed to retrieve story details for ", tnid)
      return(list(storyId = NA_character_, title = NA_character_, snippet = NA_character_))
    }

    # Helper function to extract a given field. If more than one value is returned,
    # they are concatenated with a comma.
    extract_field <- function(field) {
      result <- NA_character_
      tryCatch({
        if (is.list(story_resp$data)) {
          # If the data directly contains the field, use it.
          if (!is.null(story_resp$data[[field]])) {
            result <- as.character(story_resp$data[[field]])
          } else {
            # Otherwise assume data is a list of story items.
            vals <- sapply(story_resp$data, function(x) x[[field]])
            # Remove any NULL or NA values.
            vals <- vals[!is.null(vals) & !is.na(vals)]
            if (length(vals) > 1) {
              result <- paste(vals, collapse = ",")
            } else if (length(vals) == 1) {
              result <- vals[1]
            }
          }
        }
      }, error = function(e) NA_character_)
      result
    }

    list(
      storyId = extract_field("storyId"),
      title   = extract_field("text"),
      snippet = extract_field("snippet")
    )
  })

  snippet <- storyId <-  title <- NULL

  # Add the retrieved story details to the data table.
  dt[, storyId := sapply(details, function(x) x$storyId)]
  dt[, title   := sapply(details, function(x) x$title)]
  dt[, snippet := sapply(details, function(x) x$snippet)]

  return(data.table::setDF(dt))
}

