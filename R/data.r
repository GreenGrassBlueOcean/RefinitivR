#' Public Holidays between the years 1990 and 2030
#'
#' A dataset containing the public holidays for 100 Countries.
#'
#' @format A list containing:
#' \describe{
#'   \item{NagerHolidayData}{a data.frame with the specifics of the Holiday dates by country and year}
#'   \item{HolidayVectorperCountry}{a list of vectors containing the public holiday dates per country}
#'   \item{HolidayDataFrame}{a dataframe with two columns date (of the holiday) and the iso3c countryindication of the country where the holiday is located}
#' }
#' @source \url{https://date.nager.at/}
"NagerHolidayData"


#' Operating Mic lookup from Reuters Exchange Code
#'
#' A data.frame containing a lookup list from Reuters exchange codes to operating mics
#'
#' @format A data.frame containing:
#' \describe{
#'   \item{RDN_EXCHD2}{Reuters Exchange Code}
#'   \item{repairedMIC}{An operating mic for this Reuters exchange Code}
#' }
#' @source derived from Reuters Eikon terminal
"OperatingMicLookup"

#' Lookup code for SearchViews to alternate between RD and JSON format SearchViews
#'
#' A data.table containing a lookup list from RD SearchViews to JSON SearchViews
#'
#' @format A data.table containing:
#' \describe{
#'   \item{SearchViews_RD}{SearchViews valid for RD python API}
#'   \item{SearchViews_JSON_RDP}{SearchViews valid for RDP python API and direct JSON calls}
#' }
#' @source Refinitiv Api Documentation
"SearchViewsLookup"
