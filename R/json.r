#' Builds a json like list for the Eikon Data API
#'
#' @param directions - Where
#' @param payload - What
#'
#' @return Nested list that fit into the JSON scheme
#' @noRd
#' @examples
#' directions <- "DataGrid_StandardAsync"
#' payload <- list("requests" = list(list(
#'   "instruments" = list("TSLA.O"),
#'   "fields" = lapply(list("TR.RICCode"), \(x) list("name" = x))
#' )))
#'
#' json_builder(directions, payload)
json_builder <- function(directions, payload) {
  list("Entity" = list("E" = directions, "W" = payload))
}


#' Takes a list with elements and converts them to a JSON string
#'
#' Converts a payload list to a JSON string using \code{jsonlite::toJSON()},
#' which handles special-character escaping correctly.
#'
#' Non-\code{parameters} fields are serialised as JSON arrays. The optional
#' \code{parameters} field is serialised as a JSON object with scalar string
#' values. An empty \code{parameters} list is silently dropped.
#'
#' @param payload a named list. Non-\code{parameters} fields may be character
#'   vectors or lists; \code{parameters} (if present) must be a named list of
#'   scalar values.
#'
#' @return character JSON string
#' @noRd
#' @examples
#' payload <- list(universe = "AAPL.O", fields = c("BID", "ASK"))
#' payload <- list(
#'   universe = "AAPL.O", fields = c("TR.Revenue", "TR.GrossProfit"),
#'   parameters = list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01")
#' )
#' payload <- list(universe = "AAPL.O", fields = c("TR.Revenue", "TR.GrossProfit"), parameters = list())
#' jsonDataGridConstructor(payload)
jsonDataGridConstructor <- function(payload) {
  if ("parameters" %in% names(payload)) {
    if (length(payload[["parameters"]]) == 0) {
      payload[["parameters"]] <- NULL
    }
  }

  # Force non-parameter fields into lists so jsonlite renders them as JSON
  # arrays even when they contain only a single element.
  for (nm in names(payload)) {
    if (nm != "parameters") {
      payload[[nm]] <- as.list(payload[[nm]])
    }
  }

  # auto_unbox = TRUE keeps parameter values as JSON scalars ("USD") rather
  # than single-element arrays (["USD"]).
  jsonlite::toJSON(payload, auto_unbox = TRUE)
}


#' Constructs the url to send data requests to
#'
#' @param service "eikon" or "rdp"
#' @param EndPoint for RDP only
#'
#' @return url (string)
#' @noRd
#'
#' @examples
#' Construct_url(service = "eikon")
#' Construct_url(service = "rdp", EndPoint = "discovery/search/v1/")
Construct_url <- function(service = "eikon", EndPoint = NULL) {
  if (tolower(service) %in% c("eikon", "udf")) {
    url <- paste0(
      getOption("refinitiv_base_url"), ":",
      getOption("eikon_port"),
      getOption("eikon_api")
    )

    # "http://localhost:9000/api/udf"
  } else if (tolower(service) == "rdp") {
    url <- paste0(
      getOption("refinitiv_base_url"), ":",
      getOption("eikon_port"),
      getOption("rdp_api"),
      EndPoint
    )
  } else {
    stop(paste0(
      "wrong service selected in function Construct_url, only rdp, udf or eikon allowed but ",
      service, " is chosen"
    ))
  }
  return(url)
}


#' Send JSON POST request to the given service
#'
#' Sends a POST request to the given service with a json like object
#'
#' @param json - The nested list resembling JSON that should be sent to the server
#' @param service - !!WIP!! The service to send the message to
#' @param debug - Default to FALSE, turns on debugging messages
#' @param request_type character: Json Request type POST or GET
#' @param EndPoint character url endpoint from Refinitiv
#' @param url character url to overwrite default url, not required only for testing
#'
#' @return Returns the results from the query
#' @noRd
#'
#' @examples
#' # data reuqest
#' directions <- "DataGrid_StandardAsync"
#' payload <- list("requests" = list(list(
#'   "instruments" = list("TSLA.O"),
#'   "fields" = lapply(list("TR.RICCode"), function(x) list("name" = x))
#' )))
#'
#' json <- json_builder(directions, payload)
#' print(json)
#' send_json_request(json)
#'
#' # time series request
#' directions <- "TimeSeries"
#' # Builds the payload to be sent
#' payload <- list(
#'   "rics" = list("AAPL.O"), "fields" = c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE"),
#'   "interval" = "daily", "calender" = "tradingdays", "corax" = "adjusted",
#'   "startdate" = "2020-01-01T01:00:00",
#'   "enddate" = "2021-01-01T01:00:00"
#' )
#' json <- json_builder(directions, payload)
#' print(json)
#' send_json_request(json)
send_json_request <- function(json = NULL, service = "eikon", debug = FALSE, request_type = "POST", EndPoint = NULL, url = NULL, apikey = refinitiv_vault_get("api_key")) {
  # 0. url manipulation ----
  if (is.null(url)) {
    Request_url <- Construct_url(service = service, EndPoint = EndPoint)
  } else {
    Request_url <- url
  }


  if (debug) {
    message(Request_url)
    if (!is.null(json)) {
      message(json)
    }
  }

  # 1. Build base request (common to all methods) ----
  method <- toupper(request_type)
  timeout <- if (method == "POST") 60 else 30

  build_request <- function() {
    req <- httr2::request(base_url = Request_url) |>
      httr2::req_headers("x-tr-applicationid" = apikey) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_user_agent("RefinitivR (https://github.com/GreenGrassBlueOcean/RefinitivR)") |>
      httr2::req_timeout(timeout) |>
      # HTTP-level transient error handling (429 rate limit, 503 service unavailable).
      # Fires during req_perform(), before the response reaches the server polling loop.
      # Respects Retry-After header (both seconds and HTTP-date formats).
      # Uses truncated exponential backoff with full jitter when no header present.
      httr2::req_retry(
        max_tries    = 3L,
        max_seconds  = getOption("refinitiv_rate_limit_max_wait", 60),
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)
      )

    if (method %in% c("POST", "PUT") && !is.null(json)) {
      req <- req |> httr2::req_body_json(json)
    }

    if (method %in% c("DELETE", "PUT")) {
      req <- req |> httr2::req_method(method)
    }

    req
  }

  # 2. Server polling loop (async tickets & retriable LSEG error codes) ----
  max_retries <- 6L
  counter <- 0L
  results <- NA

  while (counter < max_retries) {
    query <- build_request() |> httr2::req_perform()

    if (debug && method != "DELETE") {
      message(query$status_code)
    }

    if (method != "DELETE") {
      tryresults <- httr2::resp_body_json(query, check_type = FALSE)
      if ("responses" %in% names(tryresults)) {
        tryresults <- tryresults$responses[[1]]
      }

      ticket <- NULL
      # Fetches the content from the query
      # Checks for ErrorCode and then aborts after printing message
      if (is.numeric(tryresults$ErrorCode) || is.numeric(tryresults$estimatedDuration)) {
        if (is.numeric(tryresults$ErrorCode) && tryresults$ErrorCode %in% c(2504, 500, 400)) {
          message("Retriable error code: ", tryresults$ErrorCode)
          Sys.sleep(5 * counter)
          counter <- counter + 1L
        } else if (is.numeric(tryresults$estimatedDuration)) {
          WaitTime <- NULL
          WaitTime <- try(tryresults$estimatedDuration / 1000, silent = TRUE)
          ticket <- try(tryresults$ticket, silent = TRUE)
          message(paste("request not ready, server is asking to wait for", WaitTime, "seconds so waiting patiently"))
          if (!is.null(WaitTime) && WaitTime <= 60) {
            Sys.sleep(WaitTime)
          } else {
            Sys.sleep(60)
          } # maximize waiting time to 60 seconds, not waiting for more than 60 seconds for server response
          # Check if new json body with ticket should be created for the next request
          if (!is.null(ticket) && !is.null(query$request$body)) {
            json <- ConstructTicketJsonBody(ticket = ticket, query = query, debug = debug)
          }
          counter <- counter + 1L
        } else {
          stop(paste0("Error code: ", tryresults$ErrorCode, " ", tryresults$ErrorMessage))
        }
      } else {
        results <- tryresults
        break
      }
    } else {
      break
    }
  }

  if (identical(results, NA) && counter >= max_retries) {
    warning("send_json_request: server polling loop exhausted after ",
      max_retries, " retries. Returning NA.",
      call. = FALSE
    )
  }

  results
}


#' rewrite JSON body in case a waiting ticket is assigned so that the correct json is requested
#'
#' @param query httr2_response
#' @param ticket character hash code
#' @param debug boolean print revised json message with ticket number
#'
#' @return revised json body with ticket hash
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # See tests for examples
#' }
ConstructTicketJsonBody <- function(query, ticket, debug) {
  body <- query$request$body

  if ("Entity" %in% names(body$data)) {
    body$data$Entity$W$requests[[1]] <- list("ticket" = ticket)
  } else {
    body$data <- list("ticket" = ticket)
  }

  if (debug) {
    message(body$data)
  }

  return(body$data)
}


#' return element as a list even it is not a list
#'
#' @param x anything
#' @param ElementNames character with length 1
#'
#' @return list
#'
#' @seealso [RefinitivJsonConnect()]
#' @noRd
#'
#' @examples
#' JsonListBuilder(x = "a")
#' JsonListBuilder(x = list("a"))
JsonListBuilder <- function(x) {
  if (length(x) == 1 & !is.list(x)) {
    ReturnList <- list(x)
  } else {
    ReturnList <- x
  }

  return(ReturnList)
}


#' Create a JSON connection object for the LSEG Workspace proxy
#'
#' @param Eikonapplication_id Optional application key.  When \code{NA}
#'   (the default) the function checks the credential vault, then falls back
#'   to \code{"DEFAULT_WORKSPACE_APP_KEY"}.
#' @param Eikonapplication_port Deprecated and ignored (always 9000).
#'
#' @return A connection environment with API method closures.
#' @importFrom rlang env
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- RefinitivJsonConnect()
#' }
RefinitivJsonConnect <- function(Eikonapplication_id = NA, Eikonapplication_port = 9000L) {
  CheckTerminalType()

  # Resolve API key: explicit arg > vault > default workspace key
  if (is.na(Eikonapplication_id)) {
    Eikonapplication_id <- refinitiv_vault_get("api_key")
    if (is.null(Eikonapplication_id)) {
      Eikonapplication_id <- "DEFAULT_WORKSPACE_APP_KEY"
    }
  }

  refinitiv_vault_set("api_key", Eikonapplication_id)
  options(.RefinitivPyModuleName = "JSON")
  options(.RefinitivPyModuleType = "direct JSON connection through httr2 package")

  JSON_EK <- rlang::env(
    set_app_key = function(app_key = Eikonapplication_id) {
      refinitiv_vault_set("api_key", app_key)
    },
    get_app_key = function() {
      return(refinitiv_vault_get("api_key"))
    },
    set_app_port = function(app_port = Eikonapplication_id) {
      options(.EikonApplicationPort = app_port)
    },
    get_app_port = function() {
      return(getOption(".EikonApplicationPort"))
    },
    get_timeseries = function(rics, interval, calendar, fields,
                              start_date, end_date, corax, normalize, raw_output) {
      directions <- "TimeSeries"
      # Builds the payload to be sent
      payload <- NULL
      payload <- list(
        "rics" = JsonListBuilder(rics),
        "fields" = JsonListBuilder(fields),
        "interval" = interval,
        "calender" = calendar,
        "corax" = corax,
        "startdate" = start_date,
        "enddate" = end_date
      )
      json <- json_builder(directions, payload)
      return(send_json_request(json))
    },
    get_data = function(instruments, fields, parameters = NULL, SyncFields = FALSE,
                        debug, raw_output) {
      payload <- NULL
      directions <- "DataGrid_StandardAsync"

      requests <- list(
        "instruments" = JsonListBuilder(instruments),
        "fields" = lapply(fields, function(x) {
          list("name" = x)
        }),
        "parameters" = parameters,
        "layout" = if (SyncFields) {
          list(
            "columns" = list(list("item" = "dataitem")),
            "rows" = list(list("item" = "instrument"), list("item" = "date"))
          )
        } else {
          NULL
        }
      )


      requests[sapply(requests, is.null)] <- NULL
      payload <- list("requests" = list(requests))

      json <- json_builder(directions, payload)
      returnvar <- send_json_request(json)
      return(returnvar)
    },
    get_data_rdp = function(universe, fields, parameters = NULL,
                            output = NULL, debug, raw_output) {
      EndPoint <- "data/datagrid/beta1/"
      payload <- NULL

      if (length(parameters) == 0) {
        parameters <- NULL
      }
      payload <- list(
        "universe" = JsonListBuilder(universe),
        "fields" = JsonListBuilder(fields),
        "parameters" = parameters,
        "output" = output
      )
      payload[sapply(payload, is.null)] <- NULL


      response <- send_json_request(payload,
        service = "rdp",
        EndPoint = EndPoint,
        request_type = "POST"
      )


      return(response)
    },
    get_symbology = function(symbol, from_symbol_type,
                             to_symbol_type, raw_output,
                             debug, best_match) {
      payload <- NULL
      directions <- "SymbologySearch"
      payload <- list(
        "symbols" = JsonListBuilder(symbol),
        "from" = from_symbol_type,
        "to" = to_symbol_type,
        "bestMatchOnly" = best_match
      )
      json <- json_builder(directions, payload)
      returnvar <- send_json_request(json)
    },
    search = function(query = NULL, view = "SearchAll",
                      select = NULL, top = NULL, filter = NULL,
                      boost = NULL, order_by = NULL, group_by = NULL,
                      group_count = NULL, navigators = NULL, features = NULL) {
      EndPoint <- "discovery/search/v1/"
      payload <- NULL

      payload <- list(
        "Query" = query,
        "View" = view,
        "Select" = select,
        "Top" = top,
        "Filter" = filter,
        "Boost" = boost,
        "OrderBy" = order_by,
        "GroupBy" = group_by,
        "GroupCount" = group_count,
        "Navigators" = navigators,
        "Features" = features
        # , 'Skip'= skip
      )
      payload[sapply(payload, is.null)] <- NULL
      # json <- json_builder(directions, payload)

      response <- send_json_request(payload, service = "rdp", EndPoint = EndPoint, request_type = "POST")
      return_DT <- ConvertNestedlisttoDT(response$Hits)

      # Check for lists columns with null inside and fix those
      ListCols <- names(which(lapply(return_DT, class) == "list"))

      if (!identical(ListCols, character(0))) {
        NullRemover <- function(x) {
          replaceInList(x, function(y) if (is.null(y) || identical(y, "")) NA else y)
        }
        for (i in 1:length(ListCols)) {
          return_DT[[ListCols[i]]] <- unlist(NullRemover(return_DT[[ListCols[i]]]))
        }
      }

      return(return_DT)
    },
    get_search_metadata = function(RDP = NULL, searchView) {
      EndPoint <- paste0("discovery/search/v1/metadata/views/", searchView)
      returnvar <- send_json_request(json = NULL, service = "rdp", request_type = "GET", EndPoint = EndPoint)

      return_DT <- data.table::rbindlist(returnvar$Properties,
        fill = TRUE, use.names = TRUE,
        idcol = "Refinitiv_index"
      )

      for (i in seq_along(return_DT)) data.table::set(return_DT, i = which(is.na(return_DT[[i]])), j = i, value = FALSE)

      Properties <- NULL
      if ("Properties" %in% names(return_DT)) {
        return_DT <- return_DT[, Properties := NULL]
      }

      return(data.table::setDF(return_DT))
    },
    get_historical_pricing = function(EikonObject, universe,
                                      interval, start, end,
                                      adjustments, count, fields,
                                      sessions,
                                      debug = FALSE) {
      # construct endpoint ----
      payload <- list(
        "universe" = paste(universe, collapse = ","),
        "interval" = interval,
        "start" = start,
        "end" = end,
        "adjustments" = if (is.null(adjustments)) {
          NULL
        } else {
          paste(adjustments, collapse = ",")
        },
        "count" = if (is.null(count)) {
          NULL
        } else {
          as.integer(count)
        },
        "fields" = if (is.null(fields)) {
          NULL
        } else {
          paste(fields, collapse = ",")
        },
        "sessions" = if (is.null(sessions)) {
          NULL
        } else {
          paste(sessions, collapse = ",")
        }
      )
      payload <- payload[!unlist(lapply(payload, is.null))]

      universeIndex <- grep(x = names(payload), pattern = "universe")

      NonUniverseParameters <- paste0(paste0(names(payload[-universeIndex]), "=", payload[-universeIndex]), collapse = "&")

      AllParameters <- paste0(payload[[universeIndex]], "?", NonUniverseParameters)


      EndPoint <- paste0("data/historical-pricing/beta1/views/summaries/")
      EndPoint <- paste0(EndPoint, AllParameters)

      # Execute request ----

      returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint = EndPoint, debug = debug)


      return(returnvar)
    }, get_intraday_custominstrument_pricing = function(EikonObject, universe,
                                                        interval, start, end,
                                                        adjustments, count, fields,
                                                        sessions,
                                                        debug = FALSE) {
      # construct endpoint ----
      payload <- list(
        "universe" = paste(universe, collapse = ","),
        "interval" = interval,
        "start" = start,
        "end" = end,
        "adjustments" = if (is.null(adjustments)) {
          NULL
        } else {
          paste(adjustments, collapse = ",")
        },
        "count" = as.integer(count),
        "fields" = if (is.null(fields)) {
          NULL
        } else {
          paste(fields, collapse = ",")
        },
        "sessions" = if (is.null(sessions)) {
          NULL
        } else {
          paste(sessions, collapse = ",")
        }
      )
      payload <- payload[!unlist(lapply(payload, is.null))]

      universeIndex <- grep(x = names(payload), pattern = "universe")

      NonUniverseParameters <- paste0(paste0(names(payload[-universeIndex]), "=", payload[-universeIndex]), collapse = "&")

      AllParameters <- paste0(payload[[universeIndex]], "?", NonUniverseParameters)


      EndPoint <- paste0("data/custom-instruments/v1/intraday-summaries/")
      EndPoint <- paste0(EndPoint, AllParameters)

      # Execute request ----

      returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint = EndPoint, debug = debug)


      return(returnvar)
    }, get_interday_custominstrument_pricing = function(EikonObject, universe,
                                                        interval, start, end,
                                                        adjustments, count, fields,
                                                        sessions,
                                                        debug = FALSE) {
      # construct endpoint ----
      payload <- list(
        "universe" = paste(universe, collapse = ","),
        "interval" = interval,
        "start" = start,
        "end" = end,
        "adjustments" = if (is.null(adjustments)) {
          NULL
        } else {
          paste(adjustments, collapse = ",")
        },
        "count" = as.integer(count),
        "fields" = if (is.null(fields)) {
          NULL
        } else {
          paste(fields, collapse = ",")
        },
        "sessions" = if (is.null(sessions)) {
          NULL
        } else {
          paste(sessions, collapse = ",")
        }
      )
      payload <- payload[!unlist(lapply(payload, is.null))]

      universeIndex <- grep(x = names(payload), pattern = "universe")

      NonUniverseParameters <- paste0(paste0(names(payload[-universeIndex]), "=", payload[-universeIndex]), collapse = "&")

      AllParameters <- paste0(payload[[universeIndex]], "?", NonUniverseParameters)


      EndPoint <- paste0("data/custom-instruments/v1/interday-summaries/")
      EndPoint <- paste0(EndPoint, AllParameters)

      # Execute request ----

      returnvar <- send_json_request(payload, service = "rdp", request_type = "GET", EndPoint = EndPoint, debug = debug)


      return(returnvar)
    }, get_rdp_streaming_url = function(debug = FALSE) {
      EndPoint <- "streaming/pricing/v1/"

      payload <- NULL


      response <- send_json_request(payload,
        service = "rdp",
        EndPoint = EndPoint,
        request_type = "GET",
        debug = debug
        # , url = "http://localhost:9000/api/rdp/data/custom-instruments/v1/instruments"
      )
    }, create_custom_instrument = function(symbol = NULL,
                                           formula = NULL,
                                           type = NULL,
                                           basket = NULL,
                                           udc = NULL,
                                           currency = NULL,
                                           instrumentName = NULL,
                                           exchangeName = NULL,
                                           holidays = NULL,
                                           timeZone = NULL,
                                           description = NULL,
                                           debug = FALSE) {
      EndPoint <- "data/custom-instruments/v1/instruments"

      payload <- list(
        "symbol" = symbol,
        "formula" = formula,
        "type" = type # formula, basket, udc
        , "basket" = basket,
        "udc" = udc,
        "currency" = currency,
        "instrumentName" = instrumentName,
        "exchangeName" = exchangeName,
        "holidays" = holidays,
        "timeZone" = timeZone,
        "description" = description
      )

      payload[sapply(payload, is.null)] <- NULL


      response <- send_json_request(payload,
        service = "rdp",
        EndPoint = EndPoint,
        request_type = "POST",
        debug = debug
        # , url = "http://localhost:9000/api/rdp/data/custom-instruments/v1/instruments"
      )
    }, search_custom_instrument = function(debug = FALSE) {
      EndPoint <- "data/custom-instruments/v1/search"

      payload <- NULL


      response <- send_json_request(payload,
        service = "rdp",
        EndPoint = EndPoint,
        request_type = "GET",
        debug = debug
        # , url = "http://localhost:9000/api/rdp/data/custom-instruments/v1/instruments"
      )
    }, manage_custom_instrument = function(symbol = NULL # custom symbol!
                                           , Id = NULL,
                                           operation = c("GET", "UPDATE", "DELETE"),
                                           type = NULL,
                                           formula = NULL,
                                           basket = NULL,
                                           udc = NULL,
                                           currency = NULL,
                                           instrumentName = NULL,
                                           exchangeName = NULL,
                                           holidays = NULL,
                                           timeZone = NULL,
                                           description = NULL,
                                           UUID = getOption(".RefinitivUUID"),
                                           debug = FALSE) {
      if (!is.null(symbol) & !is.null(Id)) {
        stop("supply either symbol or Id to get_customInstrument")
      }


      if (!(length(operation) == 1L && toupper(operation) %in% c("GET", "UPDATE", "DELETE"))) {
        stop("parameter operation should be length 1 and be either GET, UPDATE or DELETE")
      }

      payload <- NULL
      EndPoint <- paste0("data/custom-instruments/v1/instruments/")
      if (!is.null(symbol)) {
        EndPoint <- paste0(EndPoint, symbol)
      } else {
        EndPoint <- paste0(EndPoint, Id)
      }


      if (identical(toupper(operation), "UPDATE")) {
        operation <- "PUT"
        Arglist <- as.list(match.call(expand.dots = FALSE))
        Arglist[[1]] <- Arglist[["UUID"]] <- Arglist[["debug"]] <-
          Arglist[["operation"]] <- NULL
        Arglist <- lapply(Arglist, eval, envir = parent.frame(1))
        # perform get request to obtain missing data for put payload message
        GET_data <- send_json_request(payload,
          service = "rdp",
          request_type = "GET",
          EndPoint = EndPoint,
          debug = debug
        )

        Update_payload <- Arglist
        Update_payload[sapply(Update_payload, is.null)] <- NULL

        NotChangingPayload <- GET_data[names(GET_data) %in% setdiff(names(GET_data), names(Update_payload))]
        payload <- c(NotChangingPayload, Update_payload)
      }

      # Execute request ----
      returnvar <- send_json_request(payload,
        service = "rdp",
        request_type = operation,
        EndPoint = EndPoint,
        debug = TRUE
      )
    }, get_news_headlines = function(query = NULL, count = 20L,
                                     repository = NULL,
                                     date_from = NULL,
                                     date_to = NULL,
                                     raw_output = FALSE,
                                     debug = FALSE) { #

      EndPoint <- paste0("News_Headlines")

      payload <- list(
        "number" = as.character(count),
        "query" = query,
        "repository" = paste0(repository, collapse = ","),
        "productName" = refinitiv_vault_get("api_key"),
        "attributionCode" = "",
        "dateFrom" = date_from,
        "dateTo" = date_to
      )
      payload[sapply(payload, is.null)] <- NULL

      json <- json_builder(directions = EndPoint, payload)

      returnvar <- send_json_request(json,
        service = "udf",
        request_type = "POST",
        EndPoint = NULL,
        debug = debug
      )
    }, rd_get_news_headlines = function(query = NULL,
                                        limit = 20L,
                                        sort = NULL,
                                        relevancy = NULL,
                                        cursor = NULL,
                                        dateFrom = NULL,
                                        dateTo = NULL,
                                        raw_output = FALSE,
                                        debug = FALSE) { #

      EndPoint <- paste0("data/news/v1/headlines")


      payload <- list(
        "query" = query,
        "limit" = limit,
        "sort" = sort,
        "relevancy" = relevancy,
        "cursor" = cursor,
        "dateFrom" = dateFrom,
        "dateTo" = dateTo
      )

      EndPointParams <- build_get_query_string(params = payload)
      EndPoint <- paste0(EndPoint, EndPointParams)

      returnvar <- send_json_request(
        service = "rdp",
        request_type = "GET",
        EndPoint = EndPoint,
        debug = debug
      )
    }, rd_get_news_story = function(story_id = NULL, raw_output = FALSE, debug = FALSE) {
      EndPoint <- paste0("data/news/v1/stories/")
      EndPoint <- paste0(EndPoint, story_id)

      returnvar <- send_json_request(
        service = "rdp",
        request_type = "GET",
        EndPoint = EndPoint,
        debug = debug
      )
    }, get_news_story = function(story_id = NULL, raw_output = FALSE, debug = FALSE) {
      EndPoint <- paste0("News_Story")

      payload <- list(
        "storyId" = story_id,
        "attributionCode" = "",
        "productName" = refinitiv_vault_get("api_key")
      )


      payload[sapply(payload, is.null)] <- NULL

      json <- json_builder(directions = EndPoint, payload)

      returnvar <- send_json_request(json,
        service = "udf",
        request_type = "POST",
        EndPoint = NULL,
        debug = debug
      )
    },

    # ── ESG endpoint ──
    get_esg = function(universe, view = "scores-full",
                       start = NULL, end = NULL,
                       debug = FALSE) {
      view_path <- if (view == "universe") {
        "universe"
      } else {
        paste0("views/", view)
      }
      EndPoint <- paste0(
        "data/environmental-social-governance/v2/",
        view_path
      )
      payload <- list(
        universe = paste(universe, collapse = ","),
        start    = start,
        end      = end
      )
      EndPoint <- paste0(
        EndPoint,
        build_get_query_string(payload)
      )
      send_json_request(
        service      = "rdp",
        request_type = "GET",
        EndPoint     = EndPoint,
        debug        = debug
      )
    },

    # ── Estimates endpoint ──
    get_estimates = function(universe,
                             view = "view-summary/annual",
                             package = NULL,
                             debug = FALSE) {
      EndPoint <- paste0(
        "data/estimates/v1/", view
      )
      payload <- list(
        universe = paste(universe, collapse = ","),
        package  = package
      )
      EndPoint <- paste0(
        EndPoint,
        build_get_query_string(payload)
      )
      send_json_request(
        service      = "rdp",
        request_type = "GET",
        EndPoint     = EndPoint,
        debug        = debug
      )
    },

    # ── Ownership endpoint ──
    get_ownership = function(universe,
                             view = "consolidated/breakdown",
                             statType = NULL,
                             limit = NULL,
                             offset = NULL,
                             sortOrder = NULL,
                             frequency = NULL,
                             start = NULL,
                             end = NULL,
                             count = NULL,
                             debug = FALSE) {
      EndPoint <- paste0(
        "data/ownership/v1/views/", view
      )
      payload <- list(
        universe  = paste(universe, collapse = ","),
        statType  = statType,
        limit     = limit,
        offset    = offset,
        sortOrder = sortOrder,
        frequency = frequency,
        start     = start,
        end       = end,
        count     = count
      )
      EndPoint <- paste0(
        EndPoint,
        build_get_query_string(payload)
      )
      send_json_request(
        service      = "rdp",
        request_type = "GET",
        EndPoint     = EndPoint,
        debug        = debug
      )
    }
  )

  class(JSON_EK) <- c("RefinitivConnection", "environment")
  return(JSON_EK)
}


#' Build GET Query String
#'
#' This function takes a named list (which may include \code{NULL} elements) and converts
#' the non-\code{NULL} elements into a URL-encoded query string suitable for use in an HTTP GET request.
#' All reserved characters (for example, spaces, colons) are encoded. If the input list is empty (or becomes
#' empty after omitting \code{NULL} elements), the function returns an empty string.
#'
#' @param params A named list of parameters. Any element that is \code{NULL} is omitted.
#'
#' @return A character string representing the URL-encoded query string. If any parameters remain,
#' the string is prepended with a "?".
#'
#' @importFrom utils URLencode
#'
#' @examples
#' \dontrun{
#' query_list <- list(
#'   query    = "R:TSLA.O AND Language:EN",
#'   limit    = 5,
#'   dateFrom = "2023-01-01T00:00:00Z",
#'   extra    = NULL
#' )
#' qs <- build_get_query_string(query_list)
#' # qs will be:
#' # "?query=R%3ATSLA.O%20AND%20Language%3AEN&limit=5&dateFrom=2023-01-01T00%3A00%3A00Z"
#'
#' # Passing an empty list returns an empty string:
#' build_get_query_string(list())
#' }
#'
#' @keywords internal
build_get_query_string <- function(params) {
  if (!is.list(params)) {
    stop("Input must be a named list")
  }

  # Remove NULL elements
  params <- params[!sapply(params, is.null)]

  # If the list is empty after removal, return an empty string.
  if (length(params) == 0) {
    return("")
  }

  # All elements must have a name.
  if (is.null(names(params)) || any(names(params) == "")) {
    stop("All elements of the list must be named")
  }

  # URL-encode each key and value (reserved = TRUE encodes reserved characters)
  encoded_params <- mapply(function(key, value) {
    key_enc <- URLencode(as.character(key), reserved = TRUE)
    value_enc <- URLencode(as.character(value), reserved = TRUE)
    paste0(key_enc, "=", value_enc)
  }, names(params), params, SIMPLIFY = TRUE)

  query_string <- paste(encoded_params, collapse = "&")

  # Prepend a "?" if there is any parameter.
  return(paste0("?", query_string))
}
