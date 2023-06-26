test_that("rd_GetHistory fails when it should", {

  expect_error(rd_GetHistory(RD = NULL, universe="GOOG.O",fields = c("BID", "ASK"),interval="tick",count=-1)
              , "count should be integer > 0")

  expect_error(rd_GetHistory(RD = NULL, fields = c("BID", "ASK"))
               , "Parameter universe should be supplied and is not")

})


test_that("rd_GetHistory can handle most simple request", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory(universe="AAPL.O"))

  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
              , list(Date = c("IDate", "Date"), Instrument = "character"
                    , variable = "character", value = "character")
  )

  expect_equal(unique(test$Instrument), "AAPL.O")

})

test_that("rd_GetHistory can handle request with simple fields", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory(universe = c("GOOG.O","MSFT.O"),fields = c("TR.Revenue.date","TR.Revenue","TR.GrossProfit")
                        ,parameters = list("Scale" = 6,"SDate" = 0,"EDate" = -3,"FRQ" = "FY", "Curn" = "EUR")))

  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               , list(Date = c("IDate", "Date"), Instrument = "character"
                      , variable = "character", value = "character")
  )


  expect_equal(sort(unique(test$Instrument)), c("GOOG.O", "MSFT.O"))
  expect_equal(sort(unique(test$variable)), c("Date", "Gross Profit", "Revenue"))
})

test_that("rd_GetHistory can handle request with explicit date", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory( universe = c("GOOG.O","MSFT.O")
                       , start = "2020-01-01T01:00:00"
                       , end = "2020-01-10T01:00:00"
                       ))

  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               , list(Date = c("IDate", "Date"), Instrument = "character"
                      , variable = "character", value = "character")
  )

  expect_equal(sort(unique(test$Instrument)), c("GOOG.O", "MSFT.O"))
  expect_equal(sort(unique(test$Date))
              , structure(c(18263L, 18264L, 18267L, 18268L, 18269L, 18270L, 18271L)
                         , class = c("IDate", "Date")))
})


test_that("rd_GetHistory can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory(universe= "AAPL.O"
                      , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
                                  ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")
                      , parameters = list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01")))


  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               , list( Date = c("IDate", "Date"), Instrument = "character"
                     , variable = "character", value = "character")
  )


  expect_equal(sort(unique(test$Date))
               , structure(c(18536L, 18562L, 18563L, 18564L, 18565L, 18567L, 18568L,
                             18569L, 18570L, 18571L, 18572L, 18575L, 18576L, 18577L, 18578L,
                             18579L, 18582L, 18583L, 18584L, 18585L, 18586L, 18589L, 18590L,
                             18591L, 18593L, 18596L, 18597L), class = c("IDate", "Date")))

  expect_equal(sort(unique(test$Instrument)), c("AAPL.O"))

})


test_that("rd_GetHistory can handle with fields and dates", {
  testthat::skip_if(is.null(getOption(".EikonApiKey")))
  test <- try(rd_GetHistory(universe= "AAPL.O"
                        , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
                                     ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")
                        , parameters = list("Curn" = "USD")
                        , start = "2020-10-27"
                        , end = "2020-12-01"
                        ))


  expect_equal(class(test), "data.frame")
  expect_equal(lapply(test, class)
               , list( Date = c("IDate", "Date"), Instrument = "character"
                       , variable = "character", value = "character")
  )


  expect_equal(sort(unique(test$Date))
               , structure(c(18536L, 18562L, 18563L, 18564L, 18565L, 18567L, 18568L,
                             18569L, 18570L, 18571L, 18572L, 18575L, 18576L, 18577L, 18578L,
                             18579L, 18582L, 18583L, 18584L, 18585L, 18586L, 18589L, 18590L,
                             18591L, 18593L, 18596L, 18597L), class = c("IDate", "Date")))

  expect_equal(sort(unique(test$Instrument)), c("AAPL.O"))

})

