test_that("JsonHeaderAnalyzer works", {

   jsonreturn <- list(list(links = list(count = 8L), variability = "", universe = list(
     list(Instrument = "GOOG.O", `Company Common Name` = "Alphabet Inc",
          `Organization PermID` = "5030853586", `Reporting Currency` = "USD"),
     list(Instrument = "MSFT.O", `Company Common Name` = "Microsoft Corp",
          `Organization PermID` = "4295907168", `Reporting Currency` = "USD")),
    data = list(list("GOOG.O", "2022-12-31T00:00:00", "2022-12-31T00:00:00", 264281.9584, 146357.8752)
                , list("GOOG.O", "2021-12-31T00:00:00", "2021-12-31T00:00:00", 226632.96342, 129044.36268)
                , list("GOOG.O", "2020-12-31T00:00:00", "2020-12-31T00:00:00", 149453.1076, 80074.546)
                , list("GOOG.O", "2019-12-31T00:00:00", "2019-12-31T00:00:00", 144386.15542, 80250.60966)
                , list("MSFT.O", "2023-06-30T00:00:00", "2023-06-30T00:00:00", 194239.16985, 133869.80268)
                , list("MSFT.O", "2022-06-30T00:00:00", "2022-06-30T00:00:00", 189153.5454, 129384.1924)
                , list("MSFT.O", "2021-06-30T00:00:00", "2021-06-30T00:00:00", 141787.27064, 97728.01168)
                , list("MSFT.O", "2020-06-30T00:00:00", "2020-06-30T00:00:00", 127339.12585, 86311.73543))
    , messages = list(codes = list(list(-1L, -1L, -1L, -1L, -1L)
                                   , list(-1L, -1L, -1L, -1L, -1L)
                                   , list(-1L, -1L, -1L, -1L, -1L)
                                   , list(-1L, -1L, -1L, -1L, -1L)
                                   , list(-1L, -1L, -1L, -1L, -1L)
                                   , list(-1L, -1L, -1L, -1L, -1L)
                                   , list(-1L, -1L))
                      , descriptions = list(list(code = -1L, description = "ok")))
    , headers = list(list(name = "instrument", title = "Instrument", type = "string", description = "The requested Instrument as defined by the user."),
                          list(name = "date", title = "Date", type = "datetime",
                          description = "Date associated with the returned data."),
                          list(name = "TR.Revenue", title = "Date", type = "datetime",
                          description = "Is used for industrial and utility companies. It consists of revenue from the sale of merchandise, manufactured goods and services, and the distribution of regulated energy resources, depending on a specific company's industry."),
                          list(name = "TR.Revenue", title = "Revenue", type = "number",
                          decimalChar = ".", description = "Is used for industrial and utility companies. It consists of revenue from the sale of merchandise, manufactured goods and services, and the distribution of regulated energy resources, depending on a specific company's industry."),
                          list(name = "TR.GrossProfit", title = "Gross Profit",
                          type = "number", decimalChar = ".", description = "Represents a measure of a company's operating performance. Gross Profit states the profits earned directly from a company's revenues and direct costs."))))


  TestOutcome <- JsonHeaderAnalyzer(jsonreturn)

  CorrectOutcome <- c("Instrument", "Date", "TR.Revenue.date", "TR.Revenue", "TR.GrossProfit")

  expect_equal(TestOutcome, CorrectOutcome)
})

test_that("JsonHeaderAnalyzer works for historical pricing", {

test <- list(universe = list(ric = "VOD.L"), status = list(code = "TS.Interday.UserRequestError.70007",
                                                  message = "The universe does not support the following fields: [universe]."),
    interval = "P1D", summaryTimestampLabel = "endPeriod", adjustments = list(
      "exchangeCorrection", "manualCorrection", "CCH", "CRE",
      "RTS", "RPO"), defaultPricingField = "OFF_CLOSE"
    , headers = list(
          list(name = "DATE", type = "string")
        , list(name = "TRDPRC_1", type = "number", decimalChar = ".")
        , list(name = "MKT_HIGH", type = "number", decimalChar = ".")
        , list(name = "MKT_LOW",type = "number", decimalChar = ".")
        , list(name = "ACVOL_UNS",type = "number", decimalChar = ".")
        , list(name = "MKT_OPEN", type = "number", decimalChar = ".")
        , list(name = "BID", type = "number", decimalChar = ".")
        , list(name = "ASK",type = "number", decimalChar = ".")
        , list(name = "TRNOVR_UNS",type = "number", decimalChar = ".")
        , list(name = "VWAP", type = "number", decimalChar = ".")
        , list(name = "MID_PRICE", type = "number", decimalChar = ".")
        , list(name = "PERATIO", type = "number", decimalChar = ".")
        , list(name = "ORDBK_VOL",type = "number", decimalChar = ".")
        , list(name = "NUM_MOVES",type = "number", decimalChar = ".")
        , list(name = "IND_AUCVOL",type = "number", decimalChar = ".")
        , list(name = "OFFBK_VOL",type = "number", decimalChar = ".")
        , list(name = "HIGH_1",type = "number", decimalChar = ".")
        , list(name = "ORDBK_VWAP",type = "number", decimalChar = ".")
        , list(name = "IND_AUC", type = "number", decimalChar = ".")
        , list(name = "OPEN_PRC", type = "number", decimalChar = ".")
        , list(name = "LOW_1", type = "number", decimalChar = ".")
        , list(name = "OFF_CLOSE", type = "number", decimalChar = ".")
        , list(name = "CLS_AUCVOL",type = "number", decimalChar = ".")
        , list(name = "OPN_AUCVOL",type = "number", decimalChar = ".")
        , list(name = "OPN_AUC",type = "number", decimalChar = ".")
        , list(name = "CLS_AUC",type = "number", decimalChar = ".")
        , list(name = "TRD_STATUS",type = "number", decimalChar = ".")
        , list(name = "INT_AUC",type = "number", decimalChar = ".")
        , list(name = "INT_AUCVOL",type = "number", decimalChar = ".")
        , list(name = "EX_VOL_UNS", type = "number", decimalChar = ".")
        , list(name = "ALL_C_MOVE",type = "number", decimalChar = ".")
        , list(name = "ELG_NUMMOV",type = "number", decimalChar = ".")
        , list(name = "NAVALUE", type = "number", decimalChar = ".")))


TestOutcome <- JsonHeaderAnalyzer(test, "name")


CorrectOutcome <- c("DATE", "TRDPRC_1", "MKT_HIGH", "MKT_LOW",
                                          "ACVOL_UNS", "MKT_OPEN", "BID", "ASK", "TRNOVR_UNS", "VWAP",
                                          "MID_PRICE", "PERATIO", "ORDBK_VOL", "NUM_MOVES", "IND_AUCVOL",
                                          "OFFBK_VOL", "HIGH_1", "ORDBK_VWAP", "IND_AUC", "OPEN_PRC", "LOW_1",
                                          "OFF_CLOSE", "CLS_AUCVOL", "OPN_AUCVOL", "OPN_AUC", "CLS_AUC",
                                          "TRD_STATUS", "INT_AUC", "INT_AUCVOL", "EX_VOL_UNS", "ALL_C_MOVE",
                                          "ELG_NUMMOV", "NAVALUE")
expect_equal(TestOutcome, CorrectOutcome)
})

test_that("JsonHeaderAnalyzer works for historical pricing", {

inputjson <- list(links = list(count = 8L), variability = ""
                  , universe = list(
                    list(Instrument = "GOOG.O", `Company Common Name` = "Alphabet Inc",`Organization PermID` = "5030853586", `Reporting Currency` = "USD"),
                    list(Instrument = "MSFT.O", `Company Common Name` = "Microsoft Corp",`Organization PermID` = "4295907168", `Reporting Currency` = "USD")),
  data = list(list("GOOG.O", "2022-12-31T00:00:00", "2022-12-31T00:00:00", 264281.9584, 146357.8752)
              , list("GOOG.O", "2021-12-31T00:00:00","2021-12-31T00:00:00", 226632.96342, 129044.36268)
              , list("GOOG.O", "2020-12-31T00:00:00", "2020-12-31T00:00:00",149453.1076, 80074.546)
              , list("GOOG.O", "2019-12-31T00:00:00","2019-12-31T00:00:00", 144386.15542, 80250.60966)
              , list("MSFT.O", "2023-06-30T00:00:00", "2023-06-30T00:00:00",194239.16985, 133869.80268)
              , list("MSFT.O", "2022-06-30T00:00:00","2022-06-30T00:00:00", 189153.5454, 129384.1924)
              , list("MSFT.O", "2021-06-30T00:00:00", "2021-06-30T00:00:00", 141787.27064, 97728.01168)
              , list("MSFT.O", "2020-06-30T00:00:00", "2020-06-30T00:00:00", 127339.12585, 86311.73543))
  , messages = list(codes = list(list(-1L, -1L, -1L, -1L, -1L), list(-1L,-1L, -1L, -1L, -1L)
                                 , list(-1L, -1L, -1L, -1L, -1L),list(-1L, -1L, -1L, -1L, -1L)
                                 , list(-1L, -1L, -1L, -1L, -1L), list(-1L, -1L, -1L, -1L, -1L)
                                 , list(-1L, -1L, -1L, -1L, -1L), list(-1L, -1L, -1L,-1L, -1L))
                    , descriptions = list(list(code = -1L,description = "ok")))
  , headers = list(list(name = "instrument",title = "Instrument", type = "string", description = "The requested Instrument as defined by the user.")
                   ,list(name = "date", title = "Date", type = "datetime",description = "Date associated with the returned data.")
                   ,list(name = "TR.Revenue", title = "Date", type = "datetime", description = "Is used for industrial and utility companies. It consists of revenue from the sale of merchandise, manufactured goods and services, and the distribution of regulated energy resources, depending on a specific company's industry.")
                   ,list(name = "TR.Revenue", title = "Revenue", type = "number", decimalChar = ".", description = "Is used for industrial and utility companies. It consists of revenue from the sale of merchandise, manufactured goods and services, and the distribution of regulated energy resources, depending on a specific company's industry.")
                   ,list(name = "TR.GrossProfit", title = "Gross Profit",type = "number", decimalChar = ".", description = "Represents a measure of a company's operating performance. Gross Profit states the profits earned directly from a company's revenues and direct costs.")))

Output <- JsonHeaderAnalyzer(inputjson, "name")

TestOutcome <- c("Instrument", "Date", "TR.Revenue.date", "TR.Revenue", "TR.GrossProfit")

expect_equal(Output, TestOutcome)
})
