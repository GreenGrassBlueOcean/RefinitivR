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

  Correct_output <- list(list(mappedSymbols = list(list(RICs = list("NVDA.O", "NVDA.OQ",
                                                                    "NVDA.DG", "NVDA.Z", "NVDA.F", "NVDA.DE", "NVDA.B", "0R1I.L",
                                                                    "NVDA-RM.MM", "NVDA.BE", "NVDA-RMTQBD.MMV", "NVDA-RMPSRD.MMV",
                                                                    "NVDA-RMPSRP.MMV", "NVDA-RMEQRP.MMV", "NVDA.MU", "NNVDA.MI",
                                                                    "NVDA.SG", "NVDA.D", "NVDA.H", "NVDA.HA", "NVDd.BS", "NVDd.CHI",
                                                                    "NVDA.MX", "NVDA.DY", "NVDA.ZY", "NVDA.PH", "NVDA.DF", "NVDA.BN",
                                                                    "NVDd.ED", "NVDA.VI", "NVDA.LM", "NVDA_KZ.KZ", "NVDde.TRE",
                                                                    "NVDA-RMFTEQ.MMV", "NVDA-RMPSRE.MMV", "NVDA-RMEQRD.MMV",
                                                                    "NVDA-RMPTSD.MMV", "NVDA-RMEQRE.MMV", "NVDA-RMPSSD.MMV",
                                                                    "NVDA.S", "NNVDAtah.MI", "NVDAn.TQ", "NVDA.ARC", "NVDA.NB",
                                                                    "NVDA.N", "NVDA.TG", "NVDA.A", "NVDA.P", "NVDA.C", "NVDA.LTS",
                                                                    "NVDA.MEM", "NVDA.MW", "NVDA.EI", "NVDde.TRU", "NVDA.TX",
                                                                    "NVDA.BIV", "NVDA.MP", "NVDA_KZEUOM.KZ", "NVDA.PFT", "NVDA_KZEUSW.KZ",
                                                                    "NVDA.BAT", "NVDd.DXE", "NVDA.UAX", "NVDA.PFTQ", "NVDd.SIG",
                                                                    "NVDA.BT1", "NVDA.DEU", "NVDA.PFTR", "NVDd.BCU", "NVDA.SBX",
                                                                    "0R1Il.BCU", "NVDA.VIf", "NVDd.EDM", "1NVDAm.ED", "NVDA.PFTP",
                                                                    "1NVDAm.EDM", "NVD.QTX", "NVDA.AOI", "NVDde.TBEA", "NVDde.TPRM",
                                                                    "NVDde.ICEM", "NVDA.ITC", "NVDA.MCO", "NVDd.EDv", "NVDd.SIU",
                                                                    "NVDA.NOI", "1NVDAm.EDv", "NVDde.TXEA", "NVDA.GTX", "NVDA.BYX",
                                                                    "NVDA.NQT", "NVDAq.L^K07", "NVDA.DE^E11", "NVDAEUR.DEp^A18",
                                                                    "NVDACHF.Lp^K14", "NVDA.B^J07", "NVDAEUR.SGp^L17", "NVDA.HA^G08",
                                                                    "NVDA.BM^D03"), bestMatch = list(RIC = "NVDA.O"), symbol = "US67066G1040"),
                                                   list(RICs = list("AAPL.O", "AAPL.OQ", "0R2V.L", "AAPL.Z",
                                                                    "AAPL.DG", "AAPL.F", "AAPL.DE", "AAPL.B", "AAPL-RM.MM",
                                                                    "AAPL.BE", "AAPL-RMPSRD.MMV", "AAPL-RMPSRP.MMV", "AAPL-RMEQRP.MMV",
                                                                    "AAPL-RMTQBD.MMV", "AAPL.MU", "AAPLE.MI", "AAPL.D", "AAPL.SG",
                                                                    "AAPL.H", "AAPL.HA", "APCd.BS", "AAPL.MX", "AAPL.DY",
                                                                    "AAPL.ZY", "AAPL.PH", "AAPL.DF", "AAPL.UAX", "AAPL.MW",
                                                                    "AAPL.LM", "1AAPLm.ED", "AAPL.CN", "AAPL.SN", "APCd.ED",
                                                                    "AAPL.BN", "AAPL.CE", "AAPL.VI", "AAPLCL.SN", "AAPLCL.CE",
                                                                    "AAPL_KZ.KZ", "AAPL.xt", "APCde.TRE", "AAPL-RMEQRE.MMV",
                                                                    "AAPL-RMFTEQ.MMV", "AAPL-RMPSSD.MMV", "AAPL-RMEQRD.MMV",
                                                                    "AAPL-RMPTSD.MMV", "AAPL-RMPSRE.MMV", "AAPLEUR.S", "AAPLUSD.S",
                                                                    "AAPLEtah.MI", "AAPL.S", "APCd.CHI", "AAPLn.TQ", "AAPL.ARC",
                                                                    "AAPL.NB", "AAPL.N", "AAPL.TG", "AAPL.P", "AAPL.BIV",
                                                                    "APCde.TRU", "AAPL.A", "AAPL.MEM", "AAPL.BAT", "AAPL_KZEUSW.KZ",
                                                                    "AAPL.TX", "AAPL_KZEUOM.KZ", "AAPL.EI", "AAPL.LTS", "APCd.DXE",
                                                                    "AAPL.C", "AAPL.MP", "APCd.SIG", "AAPL.PFT", "AAPL.PFTQ",
                                                                    "AAPL.BT1", "AAPL.DEU", "APC.QTX", "AAPL.SBX", "AAPL.EPQ",
                                                                    "AAPL.EP", "0R2Vl.BCU", "AAPL.VIf", "AAPL.EPR", "APCd.EDM",
                                                                    "AAPL.EPI", "AAPL.PFTP", "AAPL.PFTR", "AAPL.EPA", "1AAPLm.EDM",
                                                                    "APCd.BCU", "APCde.TBEA", "AAPL.AOI", "APCde.ICEM", "APCde.TPRM",
                                                                    "AAPL.ITC", "AAPL.MCO", "AAPL.BYX", "APCde.TXEA", "AAPL.GTX"),
                                                        bestMatch = list(RIC = "AAPL.O"), symbol = "US0378331005"))))

  expect_equal(ex7_raw,  Correct_output)


})
