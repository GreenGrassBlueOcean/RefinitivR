test_that("EikonGetSymbology works", {

  Eikon <- check_Eikonapi()

  #ex1
  expect_equal( EikonGetSymbology( EikonObject = Eikon, symbol =  "AAPL.O"
                                 , to_symbol_type = "ISIN" )
              , structure(list(ISIN = "US0378331005", RIC = "AAPL.O")
                         , row.names = c(NA, -1L), class = "data.frame")
              )

  #ex2
  expect_equal( EikonGetSymbology( EikonObject = Eikon
                                 , symbol =  "US0378331005"
                                 , from_symbol_type = "ISIN"
                                 , to_symbol_type = "RIC"
                                 )
              , structure(list(RIC = "AAPL.O", ISIN = "US0378331005")
                          , row.names = c(NA, -1L), class = "data.frame"))

  #ex3
  ex3 <- EikonGetSymbology( EikonObject = Eikon, symbol =  "US67066G1040"
                          , from_symbol_type = "ISIN"
                          , to_symbol_type = "RIC"
                          , verbose = TRUE, bestMatch = FALSE
                          )

  expect_equal(names(ex3), c("RICs", "bestMatch", "ISIN"))
  expect_equal(unique(ex3$ISIN), "US67066G1040")
  expect_equal(class(ex3), "data.frame")

  #ex4
  expect_equal( EikonGetSymbology( EikonObject = Eikon
                                , symbol =  "AAPL.O", to_symbol_type = "ISIN")
               , structure( list(ISIN = "US0378331005", RIC = "AAPL.O")
                          , row.names = c(NA, -1L), class = "data.frame")
               )

  #ex5
  expect_equal(EikonGetSymbology( EikonObject = Eikon
                                , symbol =  "AAPL.O"
                                , to_symbol_type = "ISIN")
              , structure( list(ISIN = "US0378331005", RIC = "AAPL.O")
                         , row.names = c(NA, -1L), class = "data.frame")
              )

  #ex6
  ex6 <- EikonGetSymbology(EikonObject = Eikon
                          , symbol =  c("GB00B03MLX29", "US67066G1040")
                          , from_symbol_type = "ISIN"
                          ,  to_symbol_type = "RIC"
                          , bestMatch = FALSE)

  expect_equal(class(ex6), "data.frame")
  expect_equal(names(ex6), c("RICs", "bestMatch", "ISIN"))
  expect_equal(unique(ex6$ISIN), c("GB00B03MLX29", "US67066G1040"))

  #ex7
  ex7 <- EikonGetSymbology( EikonObject = Eikon
                          , symbol =  c("US67066G1040", "US0378331005")
                          , from_symbol_type = "ISIN" ,  to_symbol_type = "RIC"
                          , verbose = TRUE, bestMatch = FALSE)

  expect_equal(class(ex7), "data.frame")
  expect_equal(names(ex7), c("RICs", "bestMatch", "ISIN"))
  expect_equal(unique(ex7$ISIN), c("US67066G1040", "US0378331005"))

})


test_that("EikonGetSymbology works with raw_output", {

  Eikon <- check_Eikonapi()

  ex7_raw <- EikonGetSymbology( EikonObject = Eikon
                            , symbol =  c("US67066G1040", "US0378331005")
                            , from_symbol_type = "ISIN" ,  to_symbol_type = "RIC"
                            , verbose = TRUE, bestMatch = FALSE, raw_output = TRUE)

  expect_equal(length(ex7_raw[[1]]$mappedSymbols),2L)
  expect_equal(names(ex7_raw[[1]]$mappedSymbols[[1]]), c("RICs", "bestMatch", "symbol"))

})

test_that("EikonGetSymbology works with existing and non existing rics", {

testthat::skip_if(is.null(getOption(".EikonApiKey")))

isin = c("NL0015001S78", "US0970231058")

RefSymbologyList_RD <- EikonGetSymbology( EikonObject = EikonConnect(PythonModule = "RD")
                                          , symbol = isin #CurrentIsins2$ISIN
                                          , from_symbol_type = "ISIN"
                                          , to_symbol_type = "RIC"
                                          , verbose = TRUE
                                          , bestMatch = FALSE
                                          , raw_output = FALSE)

expect_equal(RefSymbologyList_RD$RICs |> is.na() |> sum(), 1)


RefSymbologyList_JSON <- EikonGetSymbology( EikonObject = EikonConnect(PythonModule = "JSON")
                                            , symbol = isin #CurrentIsins2$ISIN
                                            , from_symbol_type = "ISIN"
                                            , to_symbol_type = "RIC"
                                            , verbose = TRUE
                                            , bestMatch = FALSE
                                            , raw_output = FALSE)


expect_equal(RefSymbologyList_JSON$RICs |> is.na() |> sum(), 1)

expect_equal(RefSymbologyList_JSON, RefSymbologyList_RD)

})

test_that("EikonGetSymbology works with single non existing ric", {

  testthat::skip_if(is.null(getOption(".EikonApiKey")))

  isin = c("NL0015001S78")

  RefSymbologyList_RD <- EikonGetSymbology( EikonObject = EikonConnect(PythonModule = "RD")
                                            , symbol = isin #CurrentIsins2$ISIN
                                            , from_symbol_type = "ISIN"
                                            , to_symbol_type = "RIC"
                                            , verbose = TRUE
                                            , bestMatch = FALSE
                                            , raw_output = FALSE)


  RefSymbologyList_JSON <- EikonGetSymbology( EikonObject = EikonConnect(PythonModule = "JSON")
                                              , symbol = isin #CurrentIsins2$ISIN
                                              , from_symbol_type = "ISIN"
                                              , to_symbol_type = "RIC"
                                              , verbose = TRUE
                                              , bestMatch = FALSE
                                              , raw_output = FALSE)

 expect_equal(RefSymbologyList_JSON, RefSymbologyList_RD)

})
