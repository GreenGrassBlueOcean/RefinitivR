#' Translate Frequency to ISO 8601 Duration
#'
#' This function translates a given frequency string such as 'minute', 'hour',
#' 'daily', 'weekly', etc. into the corresponding ISO 8601 duration format
#' (e.g., 'PT1M', 'P1D', 'P1W'). It defaults to 'P1D' (daily) if the input is
#' not recognized.
#'
#' This function specially helps transitioning legacy EikonGetTimeseries Code
#' to get rd_GetHistoricalPricing() when using this function one do not need to worry
#' about the correct interval
#'
#'
#' @param frequency A character string representing the time frequency.
#'   Accepted values include 'minute', '1min', '5min', '10min', '30min', '60min',
#'   'hour', 'hourly', '1h', 'daily', '1d', '1D', '7D', '7d', 'weekly', '1W',
#'   'monthly', '1M', 'quarterly', '3M', 'yearly', '12M', '1Y', or ISO 8601
#'   durations like 'PT1M', 'PT5M', 'PT10M', 'PT30M', 'PT60M', 'PT1H', 'P1D',
#'   'P7D', 'P1W', 'P1M', 'P3M', 'P12M', 'P1Y'. Default is 'daily'.
#' @param default The fallback duration to return if the frequency is not recognized.
#'   Default is 'P1D'.
#'
#' @return A character string representing the ISO 8601 duration equivalent of the input frequency.
#' @examples
#' Refinitiv:::translate_to_iso8601_duration("minute") # Returns "PT1M"
#' Refinitiv:::translate_to_iso8601_duration("1min") # Returns "PT1M"
#' Refinitiv:::translate_to_iso8601_duration("1D") # Returns "P1D"
#' Refinitiv:::translate_to_iso8601_duration("daily") # Returns "P1D"
#' Refinitiv:::translate_to_iso8601_duration("PT1M") # Returns "PT1M"
#' Refinitiv:::translate_to_iso8601_duration("hour") # Returns "PT1H"
#' Refinitiv:::translate_to_iso8601_duration("weekly") # Returns "P1W"
#' Refinitiv:::translate_to_iso8601_duration("unknown") # Returns "P1D" (default)
#'
#' @keywords internal
translate_to_iso8601_duration <- function(frequency = "daily", default = "P1D") {
  freq <- iso8601 <- NULL

  # Handle NULL, NA, and empty input
  if (is.null(frequency) || length(frequency) == 0L || is.na(frequency) || identical(frequency, "")) {
    return(default)
  }

  freq_clean <- tolower(trimws(frequency))
  freq_upper <- toupper(trimws(frequency))

  # Supported ISO 8601 intervals
  allowed_iso <- c(
    "PT1M", "PT5M", "PT10M", "PT30M", "PT60M", "PT1H",
    "P1D", "P7D", "P1W", "P1M", "P3M", "P12M", "P1Y"
  )

  if (freq_upper %in% allowed_iso) {
    return(freq_upper)
  }

  frequency_map <- data.table::data.table(
    freq = c(
      "minute", "1min", "1 minute",
      "5 minutes", "5min",
      "10 minutes", "10min",
      "30 minutes", "30min",
      "60 minutes", "60min",
      "hour", "hourly", "1h", "1 hour",
      "daily", "1d", "1 day", "day",
      "7 days", "7d",
      "weekly", "1w", "1 week", "week",
      "monthly", "1m", "1 month", "month",
      "quarterly", "3m", "quarter",
      "6m",
      "yearly", "12m", "1y", "1 year", "year"
    ),
    iso8601 = c(
      "PT1M", "PT1M", "PT1M",
      "PT5M", "PT5M",
      "PT10M", "PT10M",
      "PT30M", "PT30M",
      "PT60M", "PT60M",
      "PT1H", "PT1H", "PT1H", "PT1H",
      "P1D", "P1D", "P1D", "P1D",
      "P7D", "P7D",
      "P1W", "P1W", "P1W", "P1W",
      "P1M", "P1M", "P1M", "P1M",
      "P3M", "P3M", "P3M",
      "P3M",
      "P1Y", "P12M", "P1Y", "P1Y", "P1Y"
    )
  )

  matched <- frequency_map[freq == freq_clean, list(iso8601)]
  if (nrow(matched) > 0L && !is.na(matched$iso8601[1L])) {
    return(matched$iso8601[1L])
  }

  return(default)
}
