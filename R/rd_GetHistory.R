#' The get_history function allows you to retrieve pricing history, as well as Fundamental and Reference data history through a single function call.
#'
#' @param RD Refinitiv JSON connection object, default is RDConnect()
#' @param universe Instruments to request	str or vector
#' @param fields Fields to request	str or vector
#' @param parameters a named key value list for setting parameters, Default: NULL
#' @param interval Date interval. Supported intervals are: ["minute", "1min", "5min", "10min", "30min", "60min", "hourly", "1h", "daily", "1d", "1D", "7D", "7d", "weekly", "1W", "monthly", "1M", "quarterly", "3M", "6M", "yearly", "12M", "1Y"]
#' @param start The start date and timestamp of the requested history	str, date
#' @param end The end date and timestamp of the requested history	str, date
#' @param adjustments Tells the system whether to apply or not apply CORAX (Corporate Actions) events or exchange/manual corrections or price and volume adjustment according to trade/quote qualifier summarization actions to historical time series data. Possible values are ["exchangeCorrection", "manualCorrection", "CCH", "CRE", "RTS", "RPO", "unadjusted", "qualifiers"]
#' @param count The maximum number of data points returned. Values range: 1 - 10000
#' @param use_field_names_in_headers boolean 	If True - returns field name as column headers for data instead of title, it is advisable to leave this setting to TRUE to prevent the issue with two date columns when using specific fields like e.g.
#' @param SpaceConvertor  converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is NULL, use this for compatability with eikon
#' @param debug boolean, default = FALSE, if TRUE, prints out the url used to retrieve the data
#' @param cache Controls caching. \code{NULL} (default) defers to
#'   \code{getOption("refinitiv_cache", FALSE)}. \code{TRUE} uses the
#'   function default TTL (300 s). \code{FALSE} disables caching. A positive
#'   numeric value sets the cache TTL in seconds. See \code{\link{rd_ClearCache}}.
#'
#' @return data.frame
#' @export
#'
#' @details
#'
#'  #section regarding adjustments parameters:
#'
#'  The vector of adjustment types (comma delimiter) that tells the system whether
#'  to apply or not apply CORAX (Corporate Actions) events or
#'  exchange/manual corrections to historical time series data.
#'
#'  The supported values of adjustments :
#' \itemize{
#'  \item \bold{"unadjusted"}: Not apply both exchange/manual corrections and CORAX
#'  \item \bold{"exchangeCorrection"}: Apply exchange correction adjustment to historical pricing
#'  \item \bold{"manualCorrection"}: Apply manual correction adjustment to historical pricing i.e. annotations made by content analysts
#'  \item \bold{"CCH"}: Apply Capital Change adjustment to historical Pricing due to Corporate Actions e.g. stock split
#'  \item \bold{"CRE"}:Apply Currency Redenomination adjustment when there is redenomination of currency
#'  \item \bold{"RPO"}:Apply Reuters Price Only adjustment to adjust historical price only not volume
#'  \item \bold{"RTS"}:Apply Reuters TimeSeries adjustment to adjust both historical price and volume
#'  \item \bold{"qualifiers"}:Apply price or volume adjustment to historical pricing according to trade/quote qualifier summarization actions
#' }
#'
#'
#' @section Merge behavior:
#' When the requested \code{fields} include both historical-pricing fields
#' (e.g. \code{BID}, \code{TRDPRC_1}) and fundamental/reference fields
#' (e.g. \code{TR.Revenue}), the function routes them to two different
#' backend endpoints and merges the results with a \strong{full outer join}
#' on \code{(Instrument, Date)}.
#'
#' If the two endpoints return data at different date granularity (e.g. daily
#' prices vs. quarterly fundamentals), the merged result will contain rows
#' where one set of columns is populated and the other is \code{NA}. A
#' warning is emitted when the date-count ratio exceeds 5:1 for any
#' instrument.
#'
#' The result carries a \code{"merge_info"} attribute (when both endpoints
#' contributed data) with row counts and merge type. Inspect with
#' \code{attr(result, "merge_info")}.
#'
#' For full control over date alignment, call
#' \code{\link{rd_GetHistoricalPricing}} and \code{\link{rd_GetData}}
#' separately.
#'
#' @examples
#' \dontrun{
#' RDObject <- RDConnect("your api key here")
#' timeseries1 <- rd_GetHistory(universe = c("AAPL.O", "NVDA.O"))
#' timeseries2 <- rd_GetHistory(
#'   universe = "GOOG.O",
#'   fields = c("BID", "ASK"), interval = "tick", count = 5
#' )
#'
#' test <- rd_GetHistory(
#'   universe = "AAPL.O",
#'   fields = c(
#'     "TR.IssueMarketCap(Scale=6,ShType=FFL)",
#'     "TR.FreeFloatPct()/100/*FreefloatWeight*/",
#'     "TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/",
#'     "TR.CLOSEPRICE(Adjusted=0)/*close*/"
#'   ),
#'   parameters = list(
#'     "Curn" = "USD",
#'     "SDate" = "2020-10-27", "EDate" = "2020-12-01"
#'   )
#' )
#'
#' test <- rd_GetHistory(
#'   universe = c("GOOG.O", "AAPL.O"),
#'   fields = c("TR.Revenue", "TR.GrossProfit"),
#'   parameters = list("SDate" = "0CY", "Curn" = "CAD")
#' )
#' test <- rd_GetHistory(
#'   universe = c("GOOG.O", "AAPL.O"),
#'   fields = c("TR.PriceTargetMean(SDate:0CY)", "TR.LOWPRICE(SDate:0d)")
#' )
#'
#'
#' test <- rd_GetHistory(
#'   universe = c("GOOG.O", "MSFT.O", "FB.O", "AMZN.O"),
#'   fields = c("TR.Revenue.date", "TR.Revenue", "TR.GrossProfit"),
#'   parameters = list(
#'     "Scale" = 6, "SDate" = 0,
#'     "EDate" = -3, "FRQ" = "FY", "Curn" = "EUR"
#'   )
#' )
#' }
rd_GetHistory <- function(
  RD = rd_connection(),
  universe = NULL,
  fields = NULL,
  parameters = NULL,
  interval = NULL,
  start = NULL,
  end = NULL,
  adjustments = NULL,
  count = NULL,
  use_field_names_in_headers = TRUE,
  SpaceConvertor = NULL,
  debug = FALSE,
  cache = NULL
) {
  # ── Cache lookup ──
  ttl <- resolve_cache(cache, fn_default_ttl = 300)
  if (!isFALSE(ttl)) {
    .ck <- cache_key(
      "rd_GetHistory", universe, fields, parameters,
      interval, start, end, adjustments, count,
      use_field_names_in_headers, SpaceConvertor
    )
    .hit <- cache_get(.ck)
    if (.hit$found) {
      if (debug) message("[RefinitivR] Cache hit")
      return(.hit$value)
    }
  }

  # Check if universe is supplied
  if (is.null(universe)) {
    stop("Parameter universe should be supplied and is not")
  }

  if ((is.null(count) || count < 0) && is.null(start) && is.null(end)) {
    message("No or negative count, start or end date supplied, defaulting to count = 20")
    count <- 20L
  }

  if (!is.null(fields) && !use_field_names_in_headers) {
    FieldsCheck <- grepl(".date", fields, fixed = TRUE)
    if (any(FieldsCheck)) {
      stop(paste(
        "Fields should not contain Tr.Eikonformula.date  and use_field_names_in_headers = FALSE for use in GetHistory function removing",
        fields[FieldsCheck], "you will receive than multiple columns that have the same name (Date) and is therefore forbidden"
      ))
    }
  }


  if (!is.null(parameters) && ((is.null(start) | is.null(end)) & any(c("SDate", "EDate") %in% names(parameters)))) {
    if ("SDate" %in% names(parameters)) {
      if (dateFormatCheck(parameters[["SDate"]])) {
        start <- parameters[["SDate"]]
      } else {
        warning(paste0(
          "List parameters has 'SDate' which is supplied in a format which is not 'YYYY-MM-DD', but is: ",
          parameters[["SDate"]],
          ", this might result in missing results for historical timeseries, it is better to supply in YYYY-MM-DD format or use start parameter"
        ))
      }
    }

    if ("EDate" %in% names(parameters)) {
      if (dateFormatCheck(parameters[["EDate"]])) {
        end <- parameters[["EDate"]]
      } else {
        warning(paste0(
          "List parameters has 'EDate' which is supplied in a format which is not 'YYYY-MM-DD', but is: ",
          parameters[["EDate"]],
          ", this might result in missing results for historical timeseries, it is better to supply in YYYY-MM-DD format or use end parameter"
        ))
      }
    }
  }


  force(RD)

  # check if custom Instrument

  UUID <- getOption(".RefinitivUUID")
  if (!is.null(UUID) && any(CheckifCustomInstrument(symbol = universe, UUID = UUID))) {
    stop("Custom Instruments are currently not supported yet in rd_GetHistory using JSON")
  }

  # historical pricing fields
  if (!is.null(fields)) {
    GetDataFields <- setdiff(fields, getOption("HistoricalPricingFields")) # in x buy not in Y
    HistorticalPricingFields <- setdiff(fields, GetDataFields)
    if (identical(HistorticalPricingFields, character(0))) {
      HistorticalPricingFields <- NULL
    } else if (identical(GetDataFields, character(0))) {
      GetDataFields <- NULL
    }
  } else {
    GetDataFields <- NULL
    HistorticalPricingFields <- NULL
  }

  GetDataOutput <- HistoricalPricingOutput <- data.table::data.table()
  # If required obtain data from GetData
  if (!is.null(GetDataFields)) {
    if (!is.null(start)) {
      if (is.null(parameters)) {
        parameters <- list()
      }
      parameters[["SDate"]] <- format(as.Date(start), "%Y-%m-%d")
      if (!is.null(end)) {
        parameters[["EDate"]] <- format(as.Date(end), "%Y-%m-%d")
      }
    }

    GetDataOutput <- rd_GetData(
      RDObject = RD,
      rics = universe,
      Eikonformulas = GetDataFields,
      Parameters = parameters,
      raw_output = FALSE,
      time_out = 60,
      verbose = debug,
      SpaceConvertor = SpaceConvertor,
      use_field_names_in_headers = use_field_names_in_headers,
      SyncFields = TRUE # , output = 'Col,T|Va,Row,In,date|'
    )


    GetDataOutput <- data.table::as.data.table(GetDataOutput)

    if (use_field_names_in_headers) {
      ToUpperCols <- setdiff(names(GetDataOutput), c("Instrument", "Date"))

      data.table::setnames(
        x =  GetDataOutput,
        old = ToUpperCols,
        new = toupper(ToUpperCols)
      )
    }
  }

  if (is.null(fields) || !is.null(HistorticalPricingFields)) {
    if (is.null(interval)) {
      interval <- "P1D"
    }

    HistoricalPricingOutput <- rd_GetHistoricalPricing(
      RDObject = RD,
      universe = universe,
      interval = interval,
      start = start,
      end = end,
      adjustments = adjustments,
      count = count,
      fields = if (is.null(fields)) {
        NULL
      } else {
        HistorticalPricingFields
      },
      sessions = NULL,
      debug = debug,
      SpaceConvertor = SpaceConvertor
    )

    HistoricalPricingOutput <- HistoricalPricingOutput |>
      data.table::as.data.table()

    if ("NAVALUE" %in% names(HistoricalPricingOutput)) {
      NAVALUE <- NULL
      HistoricalPricingOutput[NAVALUE == FALSE, NAVALUE := NA]
    }

    if ("Universe" %in% names(HistoricalPricingOutput)) {
      data.table::setnames(HistoricalPricingOutput,
        old = c("Universe"),
        new = c("Instrument")
      )
    }
  }

  # ── Unify data from both endpoints ──
  merge_info <- NULL
  if (nrow(GetDataOutput) > 0 & nrow(HistoricalPricingOutput) > 0) {
    .check_date_granularity(HistoricalPricingOutput, GetDataOutput)
    # Deduplicate on join keys to prevent many-to-many fan-out.
    # Both endpoints produce one row per (Instrument, Date) by design;
    # this is a safety net for upstream anomalies.
    Return_DT <- data.table::merge.data.table(
      x = unique(HistoricalPricingOutput, by = c("Instrument", "Date")),
      y = unique(GetDataOutput, by = c("Instrument", "Date")),
      all.x = TRUE, all.y = TRUE,
      by = c("Instrument", "Date")
    )
    merge_info <- list(
      pricing_rows     = nrow(HistoricalPricingOutput),
      fundamental_rows = nrow(GetDataOutput),
      merged_rows      = nrow(Return_DT),
      merge_type       = "full_outer"
    )
  } else if (nrow(HistoricalPricingOutput) == 0) {
    Return_DT <- unique(GetDataOutput, by = c("Instrument", "Date"))
  } else if (nrow(GetDataOutput) == 0) {
    Return_DT <- unique(HistoricalPricingOutput, by = c("Instrument", "Date"))
  }


  try(data.table::setcolorder(Return_DT, c("Date", "Instrument", sort(toupper(fields)))),
    silent = TRUE
  )

  Date <- NULL
  Return_DT <- Return_DT[, Date := as.Date(Date, tz = "UTC")]
  data.table::setorderv(Return_DT, c("Date", "Instrument"))

  # Remove rows where all data columns are NA (join-key-only noise from outer join)
  columns_to_check <- setdiff(names(Return_DT), c("Instrument", "Date"))
  if (length(columns_to_check) > 0L) {
    Return_DT <- Return_DT[
      !rowSums(is.na(Return_DT[, columns_to_check, with = FALSE])) == length(columns_to_check)
    ]
  }

  # Return a data frame
  ReturnElement <- data.table::setDF(Return_DT)
  if (!is.null(merge_info)) {
    merge_info$final_rows <- nrow(ReturnElement)
    attr(ReturnElement, "merge_info") <- merge_info
  }

  # ── Cache store (skip errors / NULL) ──
  if (!isFALSE(ttl) && !is.null(ReturnElement) && !inherits(ReturnElement, "try-error")) {
    cache_set(.ck, ReturnElement, ttl)
  }

  return(ReturnElement)
}


# ── Internal helper: detect date granularity mismatch before merge ──
#' @keywords internal
.check_date_granularity <- function(hp_dt, gd_dt) {
  Instrument <- Date <- hp_n <- gd_n <- ratio <- NULL
  hp_counts <- hp_dt[, list(hp_n = data.table::uniqueN(Date)), by = Instrument]
  gd_counts <- gd_dt[, list(gd_n = data.table::uniqueN(Date)), by = Instrument]
  # Inner join: only instruments present on both sides

  merged <- data.table::merge.data.table(hp_counts, gd_counts, by = "Instrument")
  if (nrow(merged) == 0L) {
    return(invisible(NULL))
  }
  merged[, ratio := pmax(hp_n, gd_n) / pmax(pmin(hp_n, gd_n), 1)]
  mismatched <- merged[ratio > 5]
  if (nrow(mismatched) > 0L) {
    instruments <- paste(mismatched$Instrument, collapse = ", ")
    warning(
      "Date granularity mismatch detected for: ", instruments, ".\n",
      "Historical pricing returned a different number of dates than the ",
      "fundamental/reference fields (ratio > 5:1). The full outer join will ",
      "produce rows with NAs where one side has no matching date.\n",
      "For full control, call rd_GetHistoricalPricing() and rd_GetData() separately.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
