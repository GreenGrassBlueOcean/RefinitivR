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


test_that("ProcessSymbology still works when an error is returned for one item", {


  ProcessSymbologyInput <- list(structure(list(RICs = list(c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                                             "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAa.CHI", "RDSAa.BS", "RDSAl.BS",
                                                             "RDSAl.CHI", "RDSAa.TQ", "RDSa.BE", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                                             "RDSa.D", "RDSa.SG", "R6Cd.BS", "R6Cd.CHI", "RYDAF.PK", "RDSa.H",
                                                             "RDSa.HA", "RDSAa.ED", "RDSAa.SIG", "RDSAl.ED", "RDSa.BN", "RDSA.PR",
                                                             "RDSa.xt", "RDSa.AS1", "RDSAas.TRE", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                                             "R6Cd.AQX", "RDSAa.DXE", "R6Cd.DXE", "RDSAa.AQX", "RDSAa.NXT",
                                                             "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM", "RDSAl.EDM",
                                                             "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU", "RDSAas.IGDM",
                                                             "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv", "RDSAa.EDv",
                                                             "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "R6Cd.AQXd", "RDSAas.TBEM",
                                                             "0LN9.L^A20", "RDSa.EU^E08", "RDSa.VX^B09", "RDSa.PAp^C18", "RDSa.SMp^J17",
                                                             "RDSAas1.TRE^A20", "RDSaGBP.PAp^B17", "RDSaEUR.PAp^B17", "RDSaEUR.DEp^A18",
                                                             "RDSaGBP.DEp^A18", "RDSa.S^K08", "RDSaEUR.Sp^J18", "RDSa.SGp^L17",
                                                             "RDSas.INS^H07", "RDSaGBP.SGp^L17", "RDSaEUR.SGp^L17", "RDSaEUR.CHIp^E12",
                                                             "RDSaGBP.CHIp^E12", "RDSaEUR.MIp^L17", "RDSaGBX.Sp^J18", "RDSaGBP.xt^I11",
                                                             "RDSaEUR.OLp^E10", "RDSaEUR.STp^J18", "RDSa999.STp^J18", "RDSaGBP.OLp^E10",
                                                             "RDSa.VI^F20", "R6Cd.BCO^A14", "1ERDSANA.PIPB^C18", "1ESRDSANA.PIPB^C18",
                                                             "RDSa.PO^L08", "RDSade.CHI^J08", "RDSa.VIf^F20", "RDSaEUR.VIp^J18",
                                                             "RDSAd.NXT^F19", "RDSa.rEUR^J09", "RDSa.rGBP^J09", "RDSa.MB^L17",
                                                             "RDSa.mGBP^L14", "RDSaEUR.Ip^G19", "RDSaGBP.Ip^G19", "RDSaGBP.PZp^B09"
  ), c("AAPL.O", "AAPLEUR.xbo", "0R2V.L", "AAPL.OQ", "AAPL.Z",
       "AAPL.DG", "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL.BE", "AAPL.MU",
       "AAPL.D", "AAPL.SG", "AAPLE.MI", "AAPL.HA", "AAPL.H", "AAPLUSD.S",
       "AAPL.S", "AAPLEUR.S", "AAPL.MX", "AAPL.DY", "AAPL.ZY", "AAPL.PH",
       "AAPL.DF", "AAPL.MW", "AAPL.SN", "AAPL.BN", "AAPL.VI", "AAPL.LM",
       "AAPL.UAX", "US_AAPL.KZ", "AAPL.xt", "APCde.TRE", "AAPLn.TQ",
       "AAPLEtah.MI", "AAPL.ARC", "AAPL.NB", "AAPL.CN", "AAPL.BIV",
       "AAPL.CE", "AAPL.TI", "AAPL.PFTQ", "AAPL.PFT", "AAPL.BT1", "AAPL.N",
       "AAPL.TG", "AAPL.P", "AAPL.ITC", "AAPL.EI", "AAPL.A", "AAPL.C",
       "AAPL.BAT", "AAPL.BYX", "AAPL.DEU", "AAPL.VIf", "APCde.DAp",
       "APCde.ICEM", "AAPL.PFTP", "AAPL.PFTR", "0R2Vl.BCU", "APCd.BCU",
       "1ASPAAPL.PIPB", "1AAAPL.PIPB", "APC.QTX", "AAPL.MCO", "US0378331005.GTX",
       "AAPLq.L^K07", "AAPL.T^L04", "AAPLEUR.Lp^H16", "AAPLz.F^D94",
       "AAPLq.L^A00", "AAPL.CD^K00", "AAPLqEUR.PAp^K07", "AAPLqGBP.PAp^K07",
       "AAPLEUR.DEp^A10", "AAPLGBP.DEp^A10", "AAPLEUR.PAp^B17", "AAPLGBP.PAp^F11",
       "0HDZ.L^A08", "AAPLEUR.Lp^F08", "0HDZ.L^L08", "0JQ4.L^D10", "AAPLUSD.DEp^D13",
       "AAPLEUR.DEp^D13", "AAPL.DEU^A04", "AAPLUSD.PAp^B17", "AAPL.S^K08",
       "AAPL.SI^D02", "AAPL.B^J07", "AAPLde.INS^H07", "AAPLk.SI^B97",
       "AAPLGBP.SGp^A10", "AAPLEUR.SGp^A10", "AAPLUSD.SGp^D13", "AAPL.HA^B08",
       "AAPLEUR.SGp^D13", "AAPLEUR.CHIp^E12", "AAPL.BM^D03", "AAPLc.MX^J07"
  ), NaN)
  , bestMatch = list(list(RIC = "RDSa.AS"), list(RIC = "AAPL.O"), list(error = "No best match available"))
  , symbol = c("GB00B03MLX29", "US0378331005", "WRONGISIN")
  , error = list(NaN, NaN, "Unknown symbol"))
  , class = "data.frame", row.names = c("GB00B03MLX29", "US0378331005", "WRONGISIN")))




  ProcessSymbology_outcome <- ProcessSymbology(EikonSymbologyResult = ProcessSymbologyInput, from_symbol_type = "ISIN", to_symbol_type = "RIC")

  ExpectecOutcome <- structure(list(RIC = c("RDSa.AS", "RDSaEUR.xbo", "RDSaGBP.xbo",
                                            "RDSa.L", "RDSa.F", "RDSa.DE", "RDSAa.CHI", "RDSAa.BS", "RDSAl.BS",
                                            "RDSAl.CHI", "RDSAa.TQ", "RDSa.BE", "RDSAl.TQ", "RDSa.S", "RDSa.MU",
                                            "RDSa.D", "RDSa.SG", "R6Cd.BS", "R6Cd.CHI", "RYDAF.PK", "RDSa.H",
                                            "RDSa.HA", "RDSAa.ED", "RDSAa.SIG", "RDSAl.ED", "RDSa.BN", "RDSA.PR",
                                            "RDSa.xt", "RDSa.AS1", "RDSAas.TRE", "RYDAF.PQ", "RDSa.TG", "RDSAl.AQX",
                                            "R6Cd.AQX", "RDSAa.DXE", "R6Cd.DXE", "RDSAa.AQX", "RDSAa.NXT",
                                            "RDSa.DEU", "RDSAas.DAp", "RDSAas.ICEM", "RDSAa.EDM", "RDSAl.EDM",
                                            "R6Cd.BCU", "RDSAa.BCU", "RDSAl.BCU", "RYDAFn.BCU", "RDSAas.IGDM",
                                            "R6C.QTX", "RDSAa.MSF", "GB00B03MLX29.GTX", "RDSAl.EDv", "RDSAa.EDv",
                                            "RDSAl.NXT", "RDSAa.AQXd", "RDSAl.AQXd", "R6Cd.AQXd", "RDSAas.TBEM",
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
                                            "AAPL.O", "AAPLEUR.xbo", "0R2V.L", "AAPL.OQ", "AAPL.Z", "AAPL.DG",
                                            "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL.BE", "AAPL.MU", "AAPL.D",
                                            "AAPL.SG", "AAPLE.MI", "AAPL.HA", "AAPL.H", "AAPLUSD.S", "AAPL.S",
                                            "AAPLEUR.S", "AAPL.MX", "AAPL.DY", "AAPL.ZY", "AAPL.PH", "AAPL.DF",
                                            "AAPL.MW", "AAPL.SN", "AAPL.BN", "AAPL.VI", "AAPL.LM", "AAPL.UAX",
                                            "US_AAPL.KZ", "AAPL.xt", "APCde.TRE", "AAPLn.TQ", "AAPLEtah.MI",
                                            "AAPL.ARC", "AAPL.NB", "AAPL.CN", "AAPL.BIV", "AAPL.CE", "AAPL.TI",
                                            "AAPL.PFTQ", "AAPL.PFT", "AAPL.BT1", "AAPL.N", "AAPL.TG", "AAPL.P",
                                            "AAPL.ITC", "AAPL.EI", "AAPL.A", "AAPL.C", "AAPL.BAT", "AAPL.BYX",
                                            "AAPL.DEU", "AAPL.VIf", "APCde.DAp", "APCde.ICEM", "AAPL.PFTP",
                                            "AAPL.PFTR", "0R2Vl.BCU", "APCd.BCU", "1ASPAAPL.PIPB", "1AAAPL.PIPB",
                                            "APC.QTX", "AAPL.MCO", "US0378331005.GTX", "AAPLq.L^K07", "AAPL.T^L04",
                                            "AAPLEUR.Lp^H16", "AAPLz.F^D94", "AAPLq.L^A00", "AAPL.CD^K00",
                                            "AAPLqEUR.PAp^K07", "AAPLqGBP.PAp^K07", "AAPLEUR.DEp^A10", "AAPLGBP.DEp^A10",
                                            "AAPLEUR.PAp^B17", "AAPLGBP.PAp^F11", "0HDZ.L^A08", "AAPLEUR.Lp^F08",
                                            "0HDZ.L^L08", "0JQ4.L^D10", "AAPLUSD.DEp^D13", "AAPLEUR.DEp^D13",
                                            "AAPL.DEU^A04", "AAPLUSD.PAp^B17", "AAPL.S^K08", "AAPL.SI^D02",
                                            "AAPL.B^J07", "AAPLde.INS^H07", "AAPLk.SI^B97", "AAPLGBP.SGp^A10",
                                            "AAPLEUR.SGp^A10", "AAPLUSD.SGp^D13", "AAPL.HA^B08", "AAPLEUR.SGp^D13",
                                            "AAPLEUR.CHIp^E12", "AAPL.BM^D03", "AAPLc.MX^J07", "NaN")
                                    , ISIN = c("GB00B03MLX29",
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
                                               "US0378331005", "WRONGISIN")
                                    , BestMatch = c("RDSa.AS", "RDSa.AS",
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
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O", "AAPL.O",
                                                    "AAPL.O", "AAPL.O", "AAPL.O", "No best match available")
                                    , error = c("NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN",
                                                "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", "Unknown symbol"
                                    )), row.names = c(NA, -199L), class = "data.frame")

  expect_equal(ProcessSymbology_outcome, ExpectecOutcome )

})
