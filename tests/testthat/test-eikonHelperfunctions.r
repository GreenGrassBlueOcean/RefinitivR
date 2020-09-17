## TR_field ------

test_that("TR_field returns an error when it should", {
  expect_error(TR_Field())
  expect_error(TR_Field(Field_name = 'tr.revenue', sort_dir = 1))
  expect_error(TR_Field(Field_name = 'tr.revenue', sort_dir = "a"))
  expect_error(TR_Field(Field_name = 'tr.revenue', Parameters = list("a", "b")))
  expect_error(TR_Field(Field_name = 'tr.revenue', sort_priority = "a"))
})

test_that("TR_field satisfies testcases", {
  expect_equal(TR_Field(Field_name = 'tr.revenue'), list("tr.revenue" = list()))
  expect_equal(TR_Field(Field_name ='tr.open', sort_dir ='asc', sort_priority = 1), list("tr.open" = list("asc", 1)))
  expect_equal( TR_Field(Field_name ='TR.GrossProfit', Parameters = list('Scale' = 6, 'Curn'= 'EUR'), sort_dir = 'asc', sort_priority = 0)
              , list("TR.GrossProfit" = list(params = list("Scale" = 6, "Curn" =  "EUR"), "asc", 0)))
})

## EikonRepairMic -----

test_that("EikonRepairMic returns an error when it should", {
  expect_error(EikonRepairMic())
  expect_error(EikonRepairMic(Fundamentals_Data = "a"))
  expect_error(EikonRepairMic(Fundamentals_Data = data.frame()))
})


test_that("EikonRepairMic satisfies testcases", {

  # Check with all Mic NA
  testdf1 <- data.frame( "RDN_EXCHD2" = Refinitiv::OperatingMicLookup$RDN_EXCHD2
                      , "Operating MIC" =  NA, stringsAsFactors = FALSE
                      )

  # Check with all Mic ""
  testdf2 <- data.frame( "RDN_EXCHD2" = Refinitiv::OperatingMicLookup$RDN_EXCHD2
                       , "Operating MIC" =  "", stringsAsFactors = FALSE
                       )

  # Check with some information missing
  testdf3 <- data.frame( "RDN_EXCHD2" = Refinitiv::OperatingMicLookup$RDN_EXCHD2
                         , "Operating MIC" =  Refinitiv::OperatingMicLookup$repairedMIC
                         , stringsAsFactors = FALSE)
  testdf3$Operating.MIC[c(1,5)] <- c("", NA)

  test1 <- EikonRepairMic(Fundamentals_Data = testdf1)
  test2 <- EikonRepairMic(Fundamentals_Data = testdf2)
  test3 <- EikonRepairMic(Fundamentals_Data = testdf3)

  expect_equal(test1$Operating.MIC, Refinitiv::OperatingMicLookup$repairedMIC)
  expect_equal(test1$RDN_EXCHD2, Refinitiv::OperatingMicLookup$RDN_EXCHD2)
  expect_equal(test2$Operating.MIC, Refinitiv::OperatingMicLookup$repairedMIC)
  expect_equal(test2$RDN_EXCHD2, Refinitiv::OperatingMicLookup$RDN_EXCHD2)
  expect_equal(test3$Operating.MIC, Refinitiv::OperatingMicLookup$repairedMIC)
  expect_equal(test3$RDN_EXCHD2, Refinitiv::OperatingMicLookup$RDN_EXCHD2)

})

## GetISO103883_MIC -----------------

test_that("GetISO103883_MIC satisfies testcases", {
  test <- NULL
  test <- GetISO103883_MIC()
  expect_true(all(c("MIC","OPERATINGMIC", "iso3c") %in% names(test)))
  expect_identical(class(test), "data.frame")

})





test_that("ProcessSymbology returns an error when it should", {
  expect_error(ProcessSymbology(EikonSymbologyResult = list(data.frame()), from_symbol_type = "ISIN", to_symbol_type = "RIC")
               , "ProcessSymbology retrieved input in wrong format"
  )

})




test_that("ProcessSymbology satisfies testcases", {

#run test cases 2

Ex1Outcome <- list(list(mappedSymbols = list(list(bestMatch = list(ISIN = "US0378331005"),symbol = "AAPL.O"))))
Ex2Outcome <- list(list(mappedSymbols = list(list(bestMatch = list(RIC = "RDSa.AS"),symbol = "GB00B03MLX29"))))
Ex3Outcome <- list(list(mappedSymbols = list(list(RICs = c("RDSa.AS", "RDSaEUR.xbo",
                                             "RDSaGBP.xbo", "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAl.BS", "RDSAa.CHI",
                                             "RDSAa.BS", "RDSAl.CHI", "RDSa.BE", "RDSAa.TQ", "RDSAl.TQ", "RDSa.S",
                                             "RDSa.MU", "RDSa.H", "RDSa.D", "RDSa.SG", "RYDAF.PK", "R6Cd.BS",
                                             "R6Cd.CHI", "RDSa.HA", "RDSAa.ED", "RDSAl.ED", "RDSAa.SIG", "RDSa.BN",
                                             "RDSA.PR", "RDSa.xt", "RDSAas.TRE", "RDSa.AS1", "RYDAF.PQ", "RDSa.TG",
                                             "RDSAl.AQX", "RDSAa.DXE", "R6Cd.DXE", "R6Cd.AQX", "RDSAa.AQX",
                                             "RDSAa.NXT", "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM",
                                             "RDSAl.EDM", "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU",
                                             "RDSAas.IGDM", "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv",
                                             "RDSAa.EDv", "R6Cd.AQXd", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd",
                                             "0LN9.L^A20", "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                             "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                             "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                             "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                             "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                             "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                             "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                             "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                             "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                             "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09",
                                             "RDSAnl.PZ^A12")
                                    , bestMatch = list(RIC = "RDSa.AS"), symbol = "GB00B03MLX29"))))

Ex4Outcome <- list(list(mappedSymbols = list(list(bestMatch = list(ISIN = "GB00B03MLX29"), symbol = "RDSa.AS"))))
Ex5Outcome <- list(list(mappedSymbols = list(list(bestMatch = list(ISIN = "GB00B03MLX29"), symbol = "RDSa.L"))))

Ex6Outcome <- list(list(mappedSymbols = list(list(RICs = c("RDSa.AS", "RDSaEUR.xbo","RDSaGBP.xbo", "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAl.BS", "RDSAa.CHI",
                                             "RDSAa.BS", "RDSAl.CHI", "RDSa.BE", "RDSAa.TQ", "RDSAl.TQ", "RDSa.S",
                                             "RDSa.MU", "RDSa.H", "RDSa.D", "RDSa.SG", "RYDAF.PK", "R6Cd.BS",
                                             "R6Cd.CHI", "RDSa.HA", "RDSAa.ED", "RDSAl.ED", "RDSAa.SIG", "RDSa.BN",
                                             "RDSA.PR", "RDSa.xt", "RDSAas.TRE", "RDSa.AS1", "RYDAF.PQ", "RDSa.TG",
                                             "RDSAl.AQX", "RDSAa.DXE", "R6Cd.DXE", "R6Cd.AQX", "RDSAa.AQX",
                                             "RDSAa.NXT", "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM",
                                             "RDSAl.EDM", "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU",
                                             "RDSAas.IGDM", "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv",
                                             "RDSAa.EDv", "R6Cd.AQXd", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd",
                                             "0LN9.L^A20", "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                             "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                             "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                             "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                             "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                             "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                             "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                             "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                             "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                             "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09",
                                             "RDSAnl.PZ^A12"), bestMatch = list(RIC = "RDSa.AS"), symbol = "GB00B03MLX29"),
                               list(bestMatch = list(error = "No best match available"),
                                    error = "Unknown symbol", symbol = "NL0015476987"))))

Ex7Outcome <- list(list(mappedSymbols = list(list(RICs = c("RDSa.AS", "RDSaEUR.xbo",
                                                           "RDSaGBP.xbo", "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAl.BS", "RDSAa.CHI",
                                                           "RDSAa.BS", "RDSAl.CHI", "RDSa.BE", "RDSAa.TQ", "RDSAl.TQ", "RDSa.S",
                                                           "RDSa.MU", "RDSa.H", "RDSa.D", "RDSa.SG", "RYDAF.PK", "R6Cd.BS",
                                                           "R6Cd.CHI", "RDSa.HA", "RDSAa.ED", "RDSAl.ED", "RDSAa.SIG", "RDSa.BN",
                                                           "RDSA.PR", "RDSa.xt", "RDSAas.TRE", "RDSa.AS1", "RYDAF.PQ", "RDSa.TG",
                                                           "RDSAl.AQX", "RDSAa.DXE", "R6Cd.DXE", "R6Cd.AQX", "RDSAa.AQX",
                                                           "RDSAa.NXT", "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM",
                                                           "RDSAl.EDM", "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU",
                                                           "RDSAas.IGDM", "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv",
                                                           "RDSAa.EDv", "R6Cd.AQXd", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd",
                                                           "0LN9.L^A20", "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                                           "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                                           "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                                           "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                                           "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                                           "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                                           "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                                           "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                                           "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                                           "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09",
                                                           "RDSAnl.PZ^A12"), bestMatch = list(RIC = "RDSa.AS"), symbol = "GB00B03MLX29"),
                                             list(RICs = c("AAPL.O", "AAPLEUR.xbo", "0R2V.L", "AAPL.OQ",
                                                           "AAPL.DG", "AAPL.Z", "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL.BE",
                                                           "AAPL-RM.MM", "AAPL.MU", "AAPL.SG", "AAPL.D", "AAPLE.MI",
                                                           "AAPL.H", "AAPL.HA", "AAPL.MX", "AAPLUSD.S", "AAPL.S", "AAPL.DY",
                                                           "AAPL.PH", "AAPL.ZY", "AAPL.DF", "AAPL.VI", "AAPL.CE", "AAPL.MW",
                                                           "AAPL.SN", "AAPL.LM", "AAPL.BN", "US_AAPL.KZ", "AAPL.UAX",
                                                           "AAPL.xt", "APCde.TRE", "AAPL-RMPSRE.MMV", "AAPL-RMEQRE.MMV",
                                                           "AAPL-RMEQRP.MMV", "AAPL-RMPSRP.MMV", "AAPL-RMPSRD.MMV",
                                                           "AAPL-RMEQRD.MMV", "AAPLn.TQ", "AAPLEtah.MI", "AAPLEUR.S",
                                                           "AAPL.ARC", "AAPL.NB", "AAPL.CN", "AAPL.PFT", "AAPL.BIV",
                                                           "AAPL.TI", "AAPL.PFTQ", "AAPL.TG", "AAPL.N", "AAPL.P", "AAPL.ITC",
                                                           "AAPL.BAT", "AAPL.EI", "AAPL.A", "AAPL.C", "AAPL.BYX", "AAPL.BT1",
                                                           "AAPL.DEU", "AAPL.VIf", "AAPL.PFTP", "AAPL.PFTR", "APCde.DAp",
                                                           "0R2Vl.BCU", "APCd.BCU", "APCde.ICEM", "AAPL.LTS", "APC.QTX",
                                                           "AAPL.MEM", "1ASPAAPL.PIPB", "1AAAPL.PIPB", "AAPL.MCO", "US0378331005.GTX",
                                                           "AAPL.MP", "AAPLq.L^K07", "AAPL.T^L04", "AAPLEUR.Lp^H16",
                                                           "AAPLz.F^D94", "AAPLq.L^A00", "AAPL.CD^K00", "AAPLqEUR.PAp^K07",
                                                           "AAPLqGBP.PAp^K07", "AAPLEUR.DEp^A10", "AAPLGBP.DEp^A10",
                                                           "AAPLEUR.PAp^B17", "AAPLGBP.PAp^F11", "0HDZ.L^A08", "AAPLEUR.Lp^F08",
                                                           "0HDZ.L^L08", "0JQ4.L^D10", "AAPLUSD.DEp^D13", "AAPLEUR.DEp^D13",
                                                           "AAPL.DEU^A04", "AAPLUSD.PAp^B17", "AAPL.S^K08", "AAPL.SI^D02",
                                                           "AAPL.B^J07"), bestMatch = list(RIC = "AAPL.O"), symbol = "US0378331005"))))




testEx1 <- ProcessSymbology(EikonSymbologyResult = Ex1Outcome, from_symbol_type= "RIC", to_symbol_type = "ISIN")
testEx2 <- ProcessSymbology(EikonSymbologyResult = Ex2Outcome, from_symbol_type= "ISIN", to_symbol_type = "RIC")
testEx3 <- ProcessSymbology(EikonSymbologyResult = Ex3Outcome, from_symbol_type= "ISIN", to_symbol_type = "RIC")
testEx4 <- ProcessSymbology(EikonSymbologyResult = Ex4Outcome, from_symbol_type= "RIC", to_symbol_type = "ISIN")
testEx5 <- ProcessSymbology(EikonSymbologyResult = Ex5Outcome, from_symbol_type= "RIC", to_symbol_type = "ISIN")
testEx6 <- ProcessSymbology(EikonSymbologyResult = Ex6Outcome, from_symbol_type= "ISIN", to_symbol_type = "RIC")
testEx7 <- ProcessSymbology(EikonSymbologyResult = Ex7Outcome, from_symbol_type= "ISIN", to_symbol_type = "RIC")


expect_equal(testEx1, structure(list(ISIN = list("US0378331005"), RIC = "AAPL.O"), row.names = c(NA, -1L), class = "data.frame") )
expect_equal(testEx2, structure(list(RIC = list("RDSa.AS"), ISIN = "GB00B03MLX29"), row.names = c(NA, -1L), class = "data.frame"))
expect_equal(testEx3, structure(list(RICs = c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                              "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAl.BS", "RDSAa.CHI", "RDSAa.BS",
                                              "RDSAl.CHI", "RDSa.BE", "RDSAa.TQ", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                              "RDSa.H", "RDSa.D", "RDSa.SG", "RYDAF.PK", "R6Cd.BS", "R6Cd.CHI",
                                              "RDSa.HA", "RDSAa.ED", "RDSAl.ED", "RDSAa.SIG", "RDSa.BN", "RDSA.PR",
                                              "RDSa.xt", "RDSAas.TRE", "RDSa.AS1", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                              "RDSAa.DXE", "R6Cd.DXE", "R6Cd.AQX", "RDSAa.AQX", "RDSAa.NXT",
                                              "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM", "RDSAl.EDM",
                                              "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU", "RDSAas.IGDM",
                                              "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv", "RDSAa.EDv",
                                              "R6Cd.AQXd", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "0LN9.L^A20",
                                              "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                              "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                              "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                              "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                              "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                              "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                              "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                              "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                              "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                              "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09",
                                              "RDSAnl.PZ^A12")
                                     , bestMatch = list("RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                        "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS"),
                                     ISIN = c("GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                              "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29"
                                     )), row.names = c(NA, -99L), class = "data.frame")
             )
expect_equal(testEx4, structure(list(ISIN = list("GB00B03MLX29"), RIC = "RDSa.AS"), row.names = c(NA, -1L), class = "data.frame"))
expect_equal(testEx5, structure(list(ISIN = list("GB00B03MLX29"), RIC = "RDSa.L"), row.names = c(NA, -1L), class = "data.frame"))


expect_equal(testEx6, structure(list(RICs = c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                     "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAl.BS", "RDSAa.CHI", "RDSAa.BS",
                                     "RDSAl.CHI", "RDSa.BE", "RDSAa.TQ", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                     "RDSa.H", "RDSa.D", "RDSa.SG", "RYDAF.PK", "R6Cd.BS", "R6Cd.CHI",
                                     "RDSa.HA", "RDSAa.ED", "RDSAl.ED", "RDSAa.SIG", "RDSa.BN", "RDSA.PR",
                                     "RDSa.xt", "RDSAas.TRE", "RDSa.AS1", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                     "RDSAa.DXE", "R6Cd.DXE", "R6Cd.AQX", "RDSAa.AQX", "RDSAa.NXT",
                                     "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM", "RDSAl.EDM",
                                     "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU", "RDSAas.IGDM",
                                     "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv", "RDSAa.EDv",
                                     "R6Cd.AQXd", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "0LN9.L^A20",
                                     "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                     "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                     "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                     "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                     "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                     "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                     "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                     "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                     "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                     "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09",
                                     "RDSAnl.PZ^A12", NA), bestMatch = list("RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                            "RDSa.AS", "No best match available")
                                                        , ISIN = c("GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                   "GB00B03MLX29", "GB00B03MLX29", "NL0015476987")
                            , error = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                      , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA, NA, "Unknown symbol"))
                       , row.names = c(NA, -100L), class = "data.frame")



)
expect_equal(testEx7, structure(list(RICs = c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                              "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAl.BS", "RDSAa.CHI", "RDSAa.BS",
                                              "RDSAl.CHI", "RDSa.BE", "RDSAa.TQ", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                              "RDSa.H", "RDSa.D", "RDSa.SG", "RYDAF.PK", "R6Cd.BS", "R6Cd.CHI",
                                              "RDSa.HA", "RDSAa.ED", "RDSAl.ED", "RDSAa.SIG", "RDSa.BN", "RDSA.PR",
                                              "RDSa.xt", "RDSAas.TRE", "RDSa.AS1", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                              "RDSAa.DXE", "R6Cd.DXE", "R6Cd.AQX", "RDSAa.AQX", "RDSAa.NXT",
                                              "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM", "RDSAl.EDM",
                                              "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU", "RDSAas.IGDM",
                                              "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv", "RDSAa.EDv",
                                              "R6Cd.AQXd", "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "0LN9.L^A20",
                                              "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                              "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                              "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                              "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                              "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                              "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                              "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                              "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                              "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                              "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09",
                                              "RDSAnl.PZ^A12", "AAPL.O", "AAPLEUR.xbo", "0R2V.L", "AAPL.OQ",
                                              "AAPL.DG", "AAPL.Z", "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL.BE",
                                              "AAPL-RM.MM", "AAPL.MU", "AAPL.SG", "AAPL.D", "AAPLE.MI", "AAPL.H",
                                              "AAPL.HA", "AAPL.MX", "AAPLUSD.S", "AAPL.S", "AAPL.DY", "AAPL.PH",
                                              "AAPL.ZY", "AAPL.DF", "AAPL.VI", "AAPL.CE", "AAPL.MW", "AAPL.SN",
                                              "AAPL.LM", "AAPL.BN", "US_AAPL.KZ", "AAPL.UAX", "AAPL.xt", "APCde.TRE",
                                              "AAPL-RMPSRE.MMV", "AAPL-RMEQRE.MMV", "AAPL-RMEQRP.MMV", "AAPL-RMPSRP.MMV",
                                              "AAPL-RMPSRD.MMV", "AAPL-RMEQRD.MMV", "AAPLn.TQ", "AAPLEtah.MI",
                                              "AAPLEUR.S", "AAPL.ARC", "AAPL.NB", "AAPL.CN", "AAPL.PFT", "AAPL.BIV",
                                              "AAPL.TI", "AAPL.PFTQ", "AAPL.TG", "AAPL.N", "AAPL.P", "AAPL.ITC",
                                              "AAPL.BAT", "AAPL.EI", "AAPL.A", "AAPL.C", "AAPL.BYX", "AAPL.BT1",
                                              "AAPL.DEU", "AAPL.VIf", "AAPL.PFTP", "AAPL.PFTR", "APCde.DAp",
                                              "0R2Vl.BCU", "APCd.BCU", "APCde.ICEM", "AAPL.LTS", "APC.QTX",
                                              "AAPL.MEM", "1ASPAAPL.PIPB", "1AAAPL.PIPB", "AAPL.MCO", "US0378331005.GTX",
                                              "AAPL.MP", "AAPLq.L^K07", "AAPL.T^L04", "AAPLEUR.Lp^H16", "AAPLz.F^D94",
                                              "AAPLq.L^A00", "AAPL.CD^K00", "AAPLqEUR.PAp^K07", "AAPLqGBP.PAp^K07",
                                              "AAPLEUR.DEp^A10", "AAPLGBP.DEp^A10", "AAPLEUR.PAp^B17", "AAPLGBP.PAp^F11",
                                              "0HDZ.L^A08", "AAPLEUR.Lp^F08", "0HDZ.L^L08", "0JQ4.L^D10", "AAPLUSD.DEp^D13",
                                              "AAPLEUR.DEp^D13", "AAPL.DEU^A04", "AAPLUSD.PAp^B17", "AAPL.S^K08",
                                              "AAPL.SI^D02", "AAPL.B^J07"), bestMatch = list("RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS", "RDSa.AS",
                                                                                             "RDSa.AS", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                                                             "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O"), ISIN = c("GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29", "GB00B03MLX29",
                                                                                                                                               "GB00B03MLX29", "GB00B03MLX29", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005", "US0378331005", "US0378331005", "US0378331005",
                                                                                                                                               "US0378331005")), row.names = c(NA, -198L), class = "data.frame"))

})



#' ex1 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "AAPL.O"
#'  , to_symbol_type = "ISIN" )
#' ex2 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE)
#' ex3 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  "GB00B03MLX29", from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
#' ex4 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.AS"
#' , to_symbol_type = "ISIN"  , verbose = TRUE)
#' ex5 <- EikonGetSymbology(EikonObject = Eikon, symbol =  "RDSa.L"
#' , to_symbol_type = "ISIN"  , verbose = TRUE)
#' ex6 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  c("GB00B03MLX29", "NL0015476987"), from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
#' ex7 <- EikonGetSymbology(EikonObject = Eikon
#' , symbol =  c("GB00B03MLX29", "US0378331005"), from_symbol_type = "ISIN"
#' ,  to_symbol_type = "RIC" , verbose = TRUE, bestMatch = FALSE)
