#' Map Eikon Time Fields to Refinitiv Data (RD) Fields
#'
#' This function translates a vector of column names from the Eikon format to corresponding Refinitiv Data (RD) names based on a predefined mapping. If a column name does not have a mapping, it is kept unchanged.
#'
#' @param cols A character vector of original column names in Eikon format. Defaults to c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE").
#'
#' @return A character vector of translated column names for Refinitiv Data (RD).
#' @importFrom stats setNames
#' @examples
#' mapEikonTimefieldsToRd()
#' mapEikonTimefieldsToRd(c("TIMESTAMP", "UNKNOWN_COLUMN", "CLOSE"))
#'
#' @export
#' @seealso [rd_GetHistoricalPricing()]
mapEikonTimefieldsToRd <- function(cols = c("TIMESTAMP", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE")) {
  # Define the mapping from old column names to new column names
  mapping <- c(
    "TIMESTAMP" = "TIMESTAMP",
    "VOLUME" = "ACVOL_UNS",
    "HIGH" = "MKT_HIGH",
    "LOW" = "MKT_LOW",
    "OPEN" = "MKT_OPEN",
    "CLOSE" = "CLS_AUC"
  )

  # Translate the columns based on mapping
  translated_cols <- setNames(mapping[cols], cols)

  # Handle unmapped columns by keeping them unchanged
  missing_cols <- is.na(translated_cols)
  translated_cols[missing_cols] <- cols[missing_cols]

  return(translated_cols)
}
