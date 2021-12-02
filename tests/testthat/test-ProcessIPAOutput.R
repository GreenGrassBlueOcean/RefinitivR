test_that("ProcessIPAOutput works with options", {

  IPAoutput <- list(headers = list(list(type = "String", name = "InstrumentTag"),
                                   list(type = "String", name = "InstrumentDescription"), list(
                                     type = "DateTime", name = "ValuationDate"), list(type = "String",
                                                                                      name = "InstrumentCode"), list(type = "DateTime", name = "EndDate"),
                                   list(type = "Float", name = "StrikePrice"), list(type = "String",
                                                                                    name = "OptionType"), list(type = "String", name = "ExerciseStyle"),
                                   list(type = "String", name = "ExerciseType"), list(type = "String",
                                                                                      name = "OptionPriceSide"), list(type = "String", name = "OptionTimeStamp"),
                                   list(type = "Float", name = "OptionPrice"), list(type = "String",
                                                                                    name = "OptionCcy"), list(type = "Float", name = "LotSize"),
                                   list(type = "String", name = "LotsUnits"), list(type = "String",
                                                                                   name = "UnderlyingRIC"), list(type = "String", name = "UnderlyingPriceSide"),
                                   list(type = "String", name = "UnderlyingTimeStamp"), list(
                                     type = "Float", name = "UnderlyingPrice"), list(type = "String",
                                                                                     name = "UnderlyingCcy"), list(type = "String", name = "DividendType"),
                                   list(type = "Float", name = "DividendYieldPercent"), list(
                                     type = "Float", name = "MarketValueInDealCcy"), list(
                                       type = "String", name = "VolatilityType"), list(type = "Float",
                                                                                       name = "VolatilityPercent"), list(type = "Float", name = "RiskFreeRatePercent"),
                                   list(type = "Float", name = "DeltaPercent"), list(type = "Float",
                                                                                     name = "GammaPercent"), list(type = "Float", name = "RhoPercent"),
                                   list(type = "Float", name = "ThetaPercent"), list(type = "Float",
                                                                                     name = "VegaPercent"), list(type = "Float", name = "GearingInDealCcy"),
                                   list(type = "Float", name = "IntrinsicInDealCcy"), list(type = "Float",
                                                                                           name = "TimeValueInDealCcy"), list(type = "Float", name = "BreakEvenTimeInDealCcy"),
                                   list(type = "Float", name = "VannaInDealCcy"), list(type = "Float",
                                                                                       name = "VolgaInDealCcy"), list(type = "Float", name = "SpeedInDealCcy"),
                                   list(type = "Float", name = "CharmInDealCcy"), list(type = "Float",
                                                                                       name = "ColorInDealCcy"), list(type = "Float", name = "DeltaInDealCcy"),
                                   list(type = "Float", name = "GammaInDealCcy"), list(type = "Float",
                                                                                       name = "RhoInDealCcy"), list(type = "Float", name = "ThetaInDealCcy"),
                                   list(type = "Float", name = "VegaInDealCcy"), list(type = "String",
                                                                                      name = "DiscountCurveId"), list(type = "String", name = "DividendCurveId"),
                                   list(type = "String", name = "ErrorCode"), list(type = "String",
                                                                                   name = "ErrorMessage"), list(type = "String", name = "ProcessingInformation")),
                    data = list(list(NULL, "CashOption_AMER_AAPLL032112500.U",
                                     "2021-12-02T00:00:00Z", "AAPLL032112500.U", "2021-12-03T00:00:00Z",
                                     125, "Vanilla", "AMER", "CALL", "Last", "Default", 35.8,
                                     "USD", 100, "SHARE", "AAPL.O", "Last", "Default", 161.2999,
                                     "USD", "ForecastYield", 0.223, 35.8, "Implied", NULL,
                                     0.078739, "NaN", "NaN", 0, 36.2999, 0, 4.50558379888268,
                                     36.2999, -0.499900000000011, 0, 0, 0, 0, 0, 0, "NaN",
                                     "NaN", 0, 3629.99, 0, "IRCurve_USD-SwapSB/3MLibor_2021-12-02T00:00:00",
                                     "ForecastTable_AAPLDIVCF.OQ_0001-01-01T00:00:00", "QPS-Pricer.4011",
                                     "Unable to calculate the Implied Volatility.", ""), list(
                                       NULL, "CashOption_AMER_AAPLL032113700.U", "2021-12-02T00:00:00Z",
                                       "AAPLL032113700.U", "2021-12-03T00:00:00Z", 137, "Vanilla",
                                       "AMER", "CALL", "Last", "Default", 30.85, "USD", 100,
                                       "SHARE", "AAPL.O", "Last", "Default", 161.2999, "USD",
                                       "ForecastYield", 0.223, 30.85, "Implied", 510.817213295606,
                                       0.078739, 0.772271428452385, 0.00706570715314288, 0.00256759741645851,
                                       -6.55009999994197, 0.0240683750973858, 5.22852188006483,
                                       24.2999, 6.55009999999999, 269.854632711134, 0, 0, 0,
                                       0, 0, 77.2271428452385, 0.706570715314288, 0.256759741645851,
                                       -655.009999994197, 2.40683750973858, "IRCurve_USD-SwapSB/3MLibor_2021-12-02T00:00:00",
                                       "ForecastTable_AAPLDIVCF.OQ_0001-01-01T00:00:00", "",
                                       "", "")))


    Test <- Refinitiv:::ProcessIPAOutput(IPAoutput)
    Correct_Outcome <- structure(list(InstrumentTag = c(NA, NA)
                                      , InstrumentDescription = c("CashOption_AMER_AAPLL032112500.U", "CashOption_AMER_AAPLL032113700.U")
                                      , ValuationDate = c("2021-12-02T00:00:00Z", "2021-12-02T00:00:00Z")
                                      , InstrumentCode = c("AAPLL032112500.U", "AAPLL032113700.U")
                                      , EndDate = c("2021-12-03T00:00:00Z", "2021-12-03T00:00:00Z")
                                      , StrikePrice = c(125, 137), OptionType = c("Vanilla", "Vanilla")
                                      , ExerciseStyle = c("AMER", "AMER"), ExerciseType = c("CALL", "CALL")
                                      , OptionPriceSide = c("Last", "Last")
                                      , OptionTimeStamp = c("Default", "Default")
                                      , OptionPrice = c(35.8, 30.85), OptionCcy = c("USD", "USD")
                                      , LotSize = c(100, 100), LotsUnits = c("SHARE", "SHARE")
                                      , UnderlyingRIC = c("AAPL.O", "AAPL.O")
                                      , UnderlyingPriceSide = c("Last", "Last")
                                      , UnderlyingTimeStamp = c("Default", "Default"), UnderlyingPrice = c(161.2999, 161.2999)
                                      , UnderlyingCcy = c("USD", "USD"), DividendType = c("ForecastYield", "ForecastYield")
                                      , DividendYieldPercent = c(0.223, 0.223), MarketValueInDealCcy = c(35.8, 30.85)
                                      , VolatilityType = c("Implied", "Implied"), VolatilityPercent = c(NA, 510.817213295606)
                                      , RiskFreeRatePercent = c(0.078739, 0.078739)
                                      , DeltaPercent = c("NaN", "0.772271428452385")
                                      , GammaPercent = c("NaN", "0.00706570715314288")
                                      , RhoPercent = c(0, 0.00256759741645851)
                                      , ThetaPercent = c(36.2999, -6.55009999994197), VegaPercent = c(0, 0.0240683750973858)
                                      , GearingInDealCcy = c(4.50558379888268, 5.22852188006483)
                                      , IntrinsicInDealCcy = c(36.2999, 24.2999)
                                      , TimeValueInDealCcy = c(-0.499900000000011, 6.55009999999999)
                                      , BreakEvenTimeInDealCcy = c(0, 269.854632711134)
                                      , VannaInDealCcy = c(0, 0), VolgaInDealCcy = c(0, 0)
                                      , SpeedInDealCcy = c(0, 0), CharmInDealCcy = c(0, 0)
                                      , ColorInDealCcy = c(0, 0), DeltaInDealCcy = c("NaN","77.2271428452385")
                                      , GammaInDealCcy = c("NaN", "0.706570715314288")
                                      , RhoInDealCcy = c(0, 0.256759741645851), ThetaInDealCcy = c(3629.99, -655.009999994197)
                                      , VegaInDealCcy = c(0, 2.40683750973858)
                                      , DiscountCurveId = c("IRCurve_USD-SwapSB/3MLibor_2021-12-02T00:00:00", "IRCurve_USD-SwapSB/3MLibor_2021-12-02T00:00:00")
                                      , DividendCurveId = c("ForecastTable_AAPLDIVCF.OQ_0001-01-01T00:00:00", "ForecastTable_AAPLDIVCF.OQ_0001-01-01T00:00:00")
                                      , ErrorCode = c("QPS-Pricer.4011", NA)
                                      , ErrorMessage = c("Unable to calculate the Implied Volatility.", NA)
                                      , ProcessingInformation = c(NA, NA)), row.names = c(NA, -2L), class = "data.frame")

    expect_equal(Test, Correct_Outcome)
})
