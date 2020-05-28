## code to prepare `NagerHolidayData` dataset goes here

# CountryInfo <- jsonlite::fromJSON("https://date.nager.at/api/v2/CountryInfo/?countryCode=BE")
Countries <- jsonlite::fromJSON("https://date.nager.at/api/v2/AvailableCountries")
years <- seq(from = 1990, to = as.numeric(format(Sys.Date(), "%Y")) + 10, by = 1)

NagerHolidayDataAllList <- vector(mode = "list", length = length(years))
for (j in years) {
print(j)
  NagerHolidayDatayear <- vector(mode = "list", length = length(Countries$key))
  for (i in 1:length(Countries$key)) {
    NagerHolidayDatayear[[i]] <- jsonlite::fromJSON(paste0("https://date.nager.at/api/v2/publicholidays/", j, "/", Countries$key[i] ))

  }
  NagerHolidayDataAllList[[j]] <- do.call("rbind",  NagerHolidayDatayear)
}

NagerHolidayData <- do.call("rbind", NagerHolidayDataAllList)

# Set Country Names in correct iso3c format
NagerHolidayData$countryCode <- countrycode::countrycode(NagerHolidayData$countryCode, origin = 'iso2c', destination = 'iso3c')


HolidayVectorperCountry <- lapply( X = sort(unique(NagerHolidayData$countryCode))
                            , FUN = function(x, NagerHolidayData){na.omit(as.Date(unique(unlist(NagerHolidayData[NagerHolidayData$countryCode == x,]$date)), origin = "1970-01-01"))}
                            , NagerHolidayData
                            )

names(HolidayVectorperCountry) <- sort(unique(NagerHolidayData$countryCode))


NagerHolidayData <- list(  "NagerHolidayData" = NagerHolidayData
                        ,  "HolidayVectorperCountry" = HolidayVectorperCountry
                        )

HolidayDataFrame <- lapply( X = NagerHolidayData$HolidayVectorperCountry
                          , FUN = function(x, CountryNames){data.frame(x, rep(CountryNames, length(x)))}
                          , CountryNames = names(NagerHolidayData$HolidayVectorperCountry)
                          )
HolidayDataFrame <- do.call("rbind", HolidayDataFrame)
names(HolidayDataFrame) <- c("date", "iso3c")
rownames(HolidayDataFrame) <- NULL

NagerHolidayData[["HolidayDataFrame"]] <- HolidayDataFrame


usethis::use_data(NagerHolidayData, overwrite = TRUE)
