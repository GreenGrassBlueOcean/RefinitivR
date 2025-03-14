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
#'     rd_get_news_headlines(query = "NS:RTRS or NS:PRN or NS:TWTR")
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
#' **Note:** The parameter \code{limit} must not exceed 100. If a value greater than 100
#' is provided, the function will throw an error. When \code{raw_output = TRUE}, a list of raw JSON responses is returned.
#'
#' @param RDObject A connection object returned by \code{RefinitivJsonConnect()}. Defaults to \code{RefinitivJsonConnect()} if not supplied.
#' @param query A character string (or vector) representing the search query.
#' @param limit An integer indicating the maximum number of headlines to retrieve. Maximum allowed value is 100.
#' @param sort An optional sort order (e.g. \code{"NewToOld"}). If not specified, the service default is used.
#' @param relevancy An optional relevancy filter (e.g. \code{"All"} or \code{"High"}).
#' @param cursor An optional pagination cursor.
#' @param dateFrom An optional start date/time (ISO 8601 format).
#' @param dateTo An optional end date/time (ISO 8601 format).
#' @param raw_output If \code{TRUE}, returns the raw JSON response; otherwise, the response is flattened into a \code{data.frame}.
#' @param debug If \code{TRUE}, prints debugging messages.
#'
#' @return A \code{data.frame} with flattened fields, or the raw JSON if \code{raw_output = TRUE}.
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
  # Check that limit is not greater than 100
  if (limit > 100) {
    stop("rd_get_news_headlines: 'limit' cannot exceed 100.")
  }

  if (is.null(query)) {
    query <- ""
  }
  orig_query <- query

  # Prepare a list to store responses
  HeadlinesList <- vector("list", length(query))
  DownloadCoordinator <- data.frame(
    index = seq_along(query),
    success = FALSE,
    retries = 0L,
    stringsAsFactors = FALSE
  )

  # Retry until all queries have succeeded or maximum retries reached.
  while (!all(DownloadCoordinator$success) && !any(DownloadCoordinator$retries > 1L)) {
    pending <- which(!DownloadCoordinator$success)
    for (j in pending) {
      HeadlinesList[[j]] <- try({
        retry(
          RDObject$rd_get_news_headlines(
            query      = query[j],
            limit      = as.integer(limit),
            sort       = sort,
            relevancy  = relevancy,
            cursor     = cursor,
            dateFrom   = dateFrom,
            dateTo     = dateTo,
            raw_output = TRUE,
            debug      = debug
          ),
          max = 2
        )
      })
      if (!identical(HeadlinesList[[j]], NA)) {
        DownloadCoordinator$success[j] <- TRUE
      }
      Sys.sleep(0.1)
      if (debug) {
        message("Download Status:\n", paste(capture.output(DownloadCoordinator), collapse = "\n"))
      }
    }
    DownloadCoordinator$retries[!DownloadCoordinator$success] <-
      DownloadCoordinator$retries[!DownloadCoordinator$success] + 1
  }

  if (any(DownloadCoordinator$retries > 1L)) {
    stop("rd_get_news_headlines: retrieving data failed after multiple retries.")
  }

  if (raw_output) {
    return(HeadlinesList)
  }

  # -- Flatten each headline in each response --
  # Each element of HeadlinesList is presumed to be a list with a 'data' field.
    dtlist <- lapply(seq_along(HeadlinesList), function(i) {
    x <- HeadlinesList[[i]]
    if (!is.list(x) || is.null(x$data)) {
      return(data.table::data.table())
    }
    # Assume x$data is a list of headlines
    headlines_vec <- x$data
    # Flatten each news item
    flattened_list <- lapply(headlines_vec, flatten_headline_item)
    flattened_list <- Filter(Negate(is.null), flattened_list)
    if (length(flattened_list) == 0) {
      return(data.table::data.table())
    }
    outdt <- data.table::rbindlist(flattened_list, fill = TRUE)
    outdt[, query := orig_query[i]]
    return(outdt)
  })

  if (length(dtlist) == 0 || all(sapply(dtlist, nrow) == 0)) {
    return(data.frame())
  }

  final_dt <- data.table::rbindlist(dtlist, fill = TRUE, use.names = TRUE)
  # Ensure that the returned product is a data.frame
  return(data.table::setDF(final_dt))
}



#' Flatten a Headline JSON Object
#'
#' This helper function takes a JSON object representing a news headline (extracted
#' from a Refinitiv RDP response) and flattens it into a named list. It extracts key
#' fields such as \code{storyId}, \code{version}, \code{urgency}, \code{firstCreated},
#' \code{versionCreated}, and \code{title}. Additionally, array fields such as
#' \code{creator}, \code{infoSource}, \code{language}, and \code{subject} are collapsed
#' into comma-separated strings.
#'
#'
#' @param h A list representing a single headline JSON object as returned by the RDP service.
#'
#' @return A named list with the following elements:
#' \itemize{
#'   \item \code{storyId} - The unique identifier for the headline.
#'   \item \code{version} - The revision version of the news.
#'   \item \code{urgency} - The urgency level of the news.
#'   \item \code{firstCreated} - The timestamp when the news was first created.
#'   \item \code{versionCreated} - The timestamp when the news was updated.
#'   \item \code{title} - The headline title.
#'   \item \code{creator} - A comma-separated string of creator \code{_qcode}s.
#'   \item \code{infoSource} - A comma-separated string of infoSource \code{_qcode}s.
#'   \item \code{language} - A comma-separated string of language tags.
#'   \item \code{subject} - A comma-separated string of subject \code{_qcode}s.
#' }
#'
#' @examples
#' \dontrun{
#' # Create a dummy headline JSON structure
#' dummy_headline <- list(
#'   storyId = "urn:newsml:reuters.com:20250312:nABC123:1",
#'   newsItem = list(
#'     `_version` = 1L,
#'     contentMeta = list(
#'       creator = list(list(`_qcode` = "NS:RTRS", `_role` = "sRole:source")),
#'       infoSource = list(list(`_qcode` = "NS:RTRS", `_role` = "sRole:source")),
#'       language = list(list(`_tag` = "en")),
#'       subject = list(list(`_qcode` = "G:1"), list(`_qcode` = "M:1QD")),
#'       urgency = list(`$` = 3L)
#'     ),
#'     itemMeta = list(
#'       firstCreated = list(`$` = "2025-03-12T15:55:31.127Z"),
#'       versionCreated = list(`$` = "2025-03-12T15:55:31.127Z"),
#'       title = list(list(`$` = "Dummy headline"))
#'     )
#'   )
#' )
#'
#' # Flatten the dummy headline
#' flat <- flatten_headline_item(dummy_headline)
#' print(flat)
#' }
#'
#' @keywords Internal
#' @noRd
flatten_headline_item <- function(h) {

  # Helper to extract a single field (e.g., title)
  extract_single_field <- function(x_list) {
    if (!is.null(x_list) && length(x_list) > 0 && !is.null(x_list[[1]]$`$`)) {
      return(x_list[[1]]$`$`)
    }
    return(NA_character_)
  }

  # Helper to extract multiple '_qcode' values into a comma-separated string
  extract_qcodes <- function(x_list) {
    if (!is.null(x_list) && length(x_list) > 0) {
      codes <- sapply(x_list, function(x) x[["_qcode"]])
      return(paste(codes, collapse = ","))
    }
    return(NA_character_)
  }


  if (!is.list(h) || is.null(h[["newsItem"]])) {
    return(NULL)
  }

  news_item <- h[["newsItem"]]
  cm <- news_item[["contentMeta"]]
  im <- news_item[["itemMeta"]]

  # Inline defaulting without %||%
  storyId_val        <- if (is.null(h[["storyId"]])) NA_character_ else h[["storyId"]]
  version_val        <- if (is.null(news_item[["_version"]])) NA_integer_ else news_item[["_version"]]
  urgency_val        <- if (!is.null(cm$urgency$`$`)) cm$urgency$`$` else NA_integer_
  firstCreated_val   <- if (!is.null(im$firstCreated$`$`)) im$firstCreated$`$` else NA_character_
  versionCreated_val <- if (!is.null(im$versionCreated$`$`)) im$versionCreated$`$` else NA_character_

  out <- list(
    storyId        = storyId_val,
    version        = version_val,
    urgency        = urgency_val,
    firstCreated   = firstCreated_val,
    versionCreated = versionCreated_val,

    # Use helper for single-field extraction
    title          = extract_single_field(im$title),

    # Use helper for qcode extraction
    creator        = extract_qcodes(cm$creator),
    infoSource     = extract_qcodes(cm$infoSource),

    # For language, extract the '_tag' field from each element
    language       = if (!is.null(cm$language) && length(cm$language) > 0) {
      tags <- sapply(cm$language, function(x) x[["_tag"]])
      paste(tags, collapse = ",")
    } else {
      NA_character_
    },
    subject        = extract_qcodes(cm$subject)
  )

  return(out)
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
#' @param RDObject A connection object returned by \code{RefinitivJsonConnect()}. Defaults to \code{RefinitivJsonConnect()} if not supplied.
#' @param story_id Vector of story IDs.
#' @param raw_output If \code{TRUE}, returns the raw list of responses.
#' @param debug If \code{TRUE}, prints debugging messages.
#' @param renderHTML If \code{TRUE}, the function will open the combined HTML in a browser viewer,
#'   and also return the HTML string.
#'
#' @return If \code{raw_output = FALSE} (the default), a character vector of the story HTML
#'   (or a single combined HTML string if \code{renderHTML = TRUE}). Otherwise, the raw list
#'   of responses.
#'
#' @export
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
    success = FALSE,
    retries = 0L,
    stringsAsFactors = FALSE
  )

  # -- The retry loop --
  while (!all(DownloadCoordinator$success) && !any(DownloadCoordinator$retries > 4L)) {
    pending <- which(!DownloadCoordinator$success)
    for (j in pending) {
      NewsList[[j]] <- try({
        retry(
          RDObject$rd_get_news_story(
            story_id   = story_id[j],
            raw_output = raw_output,   # This is OK since your dummy_RD function ignores it anyway
            debug      = debug
          ),
          max = 2
        )
      })
      # If not an error, mark as success
      if (!inherits(NewsList[[j]], "try-error") && !is.null(NewsList[[j]])) {
        DownloadCoordinator$success[j] <- TRUE
      }
      Sys.sleep(0.1)
      if (debug) {
        message("Download Status:\n", paste(capture.output(DownloadCoordinator), collapse = "\n"))
      }
    }
    DownloadCoordinator$retries[!DownloadCoordinator$success] <-
      DownloadCoordinator$retries[!DownloadCoordinator$success] + 1
  }

  # -- Stop if still failing after 4 tries --
  if (any(DownloadCoordinator$retries > 4L)) {
    stop("rd_get_news_story: retrieving data failed after multiple retries.")
  }

  # -- If raw_output=TRUE, return the raw list now --
  if (raw_output) {
    return(NewsList)
  }

  # -- Otherwise, continue processing as before --
  outvec <- vapply(
    X = NewsList,
    FUN = function(x) {
      if (is.list(x) && "story" %in% names(x)) {
        if (!is.null(x$story$storyHtml)) {
          return(x$story$storyHtml)
        } else {
          return("")
        }
      } else if (is.list(x) && "webURL" %in% names(x)) {
        if (!is.null(x$webURL)) {
          return(x$webURL)
        } else {
          return("")
        }
      } else if (is.list(x) && "newsItem" %in% names(x)) {
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
  return(outvec)
}


#' Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection, Then Fetch Stories
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
#' - **Companies**: Typically includes sub-groups such as "U.S. Companies", "European Companies",
#'   and "Asian Companies".
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
#' and the **topNewsId**), this function makes an additional GET call for each page by calling
#' `/data/news/v1/top-news/<topNewsId>`. This call retrieves the actual story details including the
#' story identifier (in **storyId**), the title (in **text**), and a summary (in **snippet**) that can subsequently
#' be used with \code{rd_get_news_story}.
#'
#' **Examples for Filtering:**
#'
#' @examples
#' \dontrun{
#' # Example 1: Retrieve all top news from the "Main" group
#' main_news <- rd_get_top_news(group = "^Main$")
#'
#' # Example 2: Retrieve only the "Front Page" of top news by filtering on page name
#' front_page_news <- rd_get_top_news(page = "^Front Page$")
#'
#' # Example 3: Retrieve stories from the "Sports & Lifestyle" group where the page is "Sport"
#' sports_news <- rd_get_top_news(group = "Sports & Lifestyle", page = "Sport")
#'
#' # Example 4: Filtering yields no results (empty data frame)
#' no_news <- rd_get_top_news(group = "NonExistent")
#' }
#'
#' @param RDObject A connection object returned by \code{RefinitivJsonConnect()}. If not supplied,
#'   defaults to \code{RefinitivJsonConnect()}.
#' @param group Optional character string (or regular expression) to filter top news groups by name.
#' @param page Optional character string (or regular expression) to filter pages by name.
#' @param raw_output If \code{TRUE}, returns the raw JSON response (list) for each page in a named list keyed by \code{topNewsId}.
#' @param debug If \code{TRUE}, prints debugging messages.
#'
#' @return A data frame (by default) with one row per story and the following columns:
#'   \itemize{
#'     \item \code{group}
#'     \item \code{page_name}
#'     \item \code{po}
#'     \item \code{revisionId}
#'     \item \code{revisionDate}
#'     \item \code{topNewsId}
#'     \item \code{storyId}
#'     \item \code{title} (the headline)
#'     \item \code{snippet} (the short text summary)
#'   }
#'   If \code{raw_output = TRUE}, a named list of raw responses, keyed by each \code{topNewsId}, is returned.
#'
#' @importFrom data.table rbindlist setDF
#' @export
rd_get_top_news <- function(RDObject = RefinitivJsonConnect(),
                            group = NULL,
                            page = NULL,
                            raw_output = FALSE,
                            debug = FALSE) {
  # 1) Request the top-news overview
  base_endpoint <- "data/news/v1/top-news"
  response <- send_json_request(
    service      = "rdp",
    request_type = "GET",
    EndPoint     = base_endpoint,
    debug        = debug
  )

  if (debug) {
    message("Top-news overview response received.")
  }

  if (is.null(response$data)) {
    if (debug) message("No data found in /top-news response.")
    return(if (raw_output) list() else data.frame())
  }

  # 2) Parse the nested JSON
  dt_pages <- data.table::rbindlist(
    lapply(response$data, function(g) {
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
    }),
    fill = TRUE
  )

  # 3) Filter (handle vector inputs by collapsing into a single OR pattern)
  if (!is.null(group)) {
    group_pattern <- paste(group, collapse = "|")  # "Main|Breakingviews|Companies", etc.
    dt_pages <- dt_pages[grepl(group_pattern, dt_pages$group, ignore.case = TRUE)]
  }
  if (!is.null(page)) {
    page_pattern <- paste(page, collapse = "|")    # "Front Page|U\\.S\\. Companies|...", etc.
    dt_pages <- dt_pages[grepl(page_pattern, dt_pages$page_name, ignore.case = TRUE)]
  }

  if (nrow(dt_pages) == 0) {
    if (debug) message("No matching top news pages after filtering.")
    return(if (raw_output) list() else data.frame())
  }

  # 4) Retrieve stories for unique topNewsIds
  unique_ids <- unique(dt_pages$topNewsId)
  top_news_dict <- list()

  for (tnid in unique_ids) {
    if (!is.null(top_news_dict[[tnid]])) next

    top_story_endpoint <- paste0("data/news/v1/top-news/", tnid)
    if (debug) {
      message(sprintf("Requesting story details for topNewsId='%s'", tnid))
    }

    story_resp <- try(
      send_json_request(
        service      = "rdp",
        request_type = "GET",
        EndPoint     = top_story_endpoint,
        debug        = debug
      ),
      silent = TRUE
    )

    if (inherits(story_resp, "try-error") || is.null(story_resp$data)) {
      if (debug) message("Failed to retrieve stories for ", tnid)
      top_news_dict[[tnid]] <- NULL
    } else {
      top_news_dict[[tnid]] <- story_resp
    }

    Sys.sleep(0.05)
  }

  if (raw_output) {
    return(top_news_dict)
  }

  # 5) Expand each page into multiple rows (one per story)
  expanded_rows <- lapply(seq_len(nrow(dt_pages)), function(i) {
    row_info <- dt_pages[i, ]
    tnid     <- row_info$topNewsId

    resp <- top_news_dict[[tnid]]
    if (is.null(resp) || is.null(resp$data)) {
      row_info$storyId <- NA_character_
      row_info$title   <- NA_character_
      row_info$snippet <- NA_character_
      return(row_info)
    }

    story_data <- resp$data
    if (!is.list(story_data[[1]])) {
      story_data <- list(story_data)
    }

    dt_stories <- data.table::rbindlist(
      lapply(story_data, function(s) {
        data.table::data.table(
          storyId = if (!is.null(s$storyId)) s$storyId else NA_character_,
          title   = if (!is.null(s$text))    s$text    else NA_character_,
          snippet = if (!is.null(s$snippet)) s$snippet else NA_character_
        )
      }),
      fill = TRUE
    )

    row_expanded <- row_info[rep(1, nrow(dt_stories)), ]
    row_expanded$storyId <- dt_stories$storyId
    row_expanded$title   <- dt_stories$title
    row_expanded$snippet <- dt_stories$snippet
    row_expanded
  })

  out_dt <- data.table::rbindlist(expanded_rows, fill = TRUE)
  data.table::setDF(out_dt)
}


