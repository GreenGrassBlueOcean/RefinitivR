test_that("CustomInstrumentBasketBuilder works", {

  expect_equal(CustomInstrumentBasketBuilder( RICs = c("AAPL.O", "AMZN.O")
                                            , Weights = c(0.5, 0.5)
                                            )
               ,structure(list(constituents = list(list(ric = "AAPL.O", weight = 50),
                                                  list(ric = "AMZN.O", weight = 50))
                              , normalizeByWeight = TRUE), class = "Refinitiv_basket")

               )

  expect_equal(CustomInstrumentBasketBuilder( RICs = c("AAPL.O", "AMZN.O")
                                              , Weights = c(0.5, 0.5)
                                              , Normalize = FALSE)
               , structure(list(constituents = list(list(ric = "AAPL.O", weight = 50),
                                                    list(ric = "AMZN.O", weight = 50)), normalizeByWeight = FALSE), class = "Refinitiv_basket"))


})


test_that("CustomInstrumentBasketBuilder fails when it should", {


  expect_error(CustomInstrumentBasketBuilder(Weights = c(0.5, 0.5))
               , "Parameter RICs can not be NULL", fixed = TRUE)


  expect_error(CustomInstrumentBasketBuilder(RICs = c("AAPL.O", "AMZN.O"))
               , "Parameter Weights can not be NULL", fixed = TRUE)


  expect_error(CustomInstrumentBasketBuilder( RICs = c("AAPL.O", "AMZN.O")
                                              , Weights = c(0.5))
               , "length of Rics and Weights should be the same")
})
