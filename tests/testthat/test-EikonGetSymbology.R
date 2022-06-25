test_that("EikonGetSymbology works", {

  check_Eikonapi <- function() {
    if (is.null(getOption(".EikonApiKey"))) {
      skip("API not available")
    }
    print("Eikon API available performing test")
  }
  check_Eikonapi()
  Eikon <- Refinitiv::EikonConnect(PythonModule = NA)


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
  ex3 <- EikonGetSymbology( EikonObject = Eikon, symbol =  "US0378331005"
                          , from_symbol_type = "ISIN"
                          , to_symbol_type = "RIC"
                          , verbose = TRUE, bestMatch = FALSE
                          )

  expect_equal(names(ex3), c("RICs", "bestMatch", "ISIN"))
  expect_equal(unique(ex3$ISIN), "US0378331005")
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
                          , symbol =  c("GB00B03MLX29", "NL0015476987")
                          , from_symbol_type = "ISIN"
                          ,  to_symbol_type = "RIC"
                          , bestMatch = FALSE)

  expect_equal(class(ex6), "data.frame")
  expect_equal(names(ex6), c("RICs", "bestMatch", "ISIN", "error"))
  expect_equal(unique(ex6$ISIN), c("GB00B03MLX29", "NL0015476987"))

  #ex7
  ex7 <- EikonGetSymbology( EikonObject = Eikon
                          , symbol =  c("GB00B03MLX29", "US0378331005")
                          , from_symbol_type = "ISIN" ,  to_symbol_type = "RIC"
                          , verbose = TRUE, bestMatch = FALSE)

  expect_equal(class(ex7), "data.frame")
  expect_equal(names(ex7), c("RICs", "bestMatch", "ISIN"))
  expect_equal(unique(ex7$ISIN), c("GB00B03MLX29", "US0378331005"))

})
