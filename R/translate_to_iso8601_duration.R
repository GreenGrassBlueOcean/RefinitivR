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
#' Accepted values include 'minute', 'hour', 'daily', 'weekly', 'monthly',
#' 'quarterly', 'yearly'. Default is 'daily'.
#'
#' @return A character string representing the ISO 8601 duration equivalent of the input frequency.
#' @examples
#' translate_to_iso8601_duration('minute')   # Returns "PT1M"
#' translate_to_iso8601_duration('hour')     # Returns "PT1H"
#' translate_to_iso8601_duration('weekly')   # Returns "P1W"
#' translate_to_iso8601_duration('unknown')  # Returns "P1D" (default)
#'
#' @export
translate_to_iso8601_duration <- function(frequency = 'daily') {
  # Handle NULL and NA input
  if (is.null(frequency) || is.na(frequency)) {
    return('P1D')
  }
  InputFrequency <- frequency

  # Define a data.table with the mapping
  frequency_map <- data.table::data.table(
    frequency = c('minute', '5 minutes', '10 minutes', '30 minutes', 'hour',
                  'daily', 'weekly', '7 days', 'monthly', 'quarterly', 'yearly'),
    iso8601 = c('PT1M', 'PT5M', 'PT10M', 'PT30M', 'PT1H',
                'P1D', 'P1W', 'P7D', 'P1M', 'P3M', 'P1Y')
  )

  # Attempt to find the corresponding ISO 8601 duration
  iso8601 <- NULL
  result <- frequency_map[InputFrequency == frequency_map$frequency, iso8601]

  # Default to 'P1D' (daily) if the input is not found
  if (length(result) == 0) {
    result <- 'P1D'
  }

  return(result)
}
