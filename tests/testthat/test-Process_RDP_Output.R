test_that("Process_RDP_output works",{

  path = "PY_get_search_metadata_input.py"
  PY_get_search_metadata_input <- reticulate::r_to_py(reticulate::py_load_object(file = path))
  r_df <- Process_RDP_output(PY_get_search_metadata_input)

  expect_equal(head(r_df, n=10)
               , structure(list(Refinitiv_index = c("AAACurrencyBondBenchmarkChain", "AACurrencyBondBenchmarkChain", "ABSMBSBondsRIC", "ActiveEstimatesExist",
                                                    "ActivityDate", "ACurrencyBondBenchmarkChain", "AdmissionToTrading", "ADRType", "ADRTypeName", "Adxr14D")
                                , Type = c("String", "String", "String", "Boolean", "Date", "String", "Date", "String", "String", "Double")
                                , Searchable = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
                                , Sortable = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
                                , Navigable = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
                                , Groupable = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
                                , Exact = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
                                , Symbol = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
                           , row.names = c(NA, 10L), class = "data.frame")
  )
})


test_that("Process_RDP_output works",{
  path = "rd_gethistorytest.py"

  rd_gethistorytest_input <- reticulate::r_to_py(reticulate::py_load_object(file = path))

    r_df <- Process_RDP_output(rd_gethistorytest_input)

  expect_equal(r_df
            ,structure(list(Date = structure(c(18263, 18264, 18267, 18268, 18269, 18270, 18271), class = "Date")
                            , variable = c("TR.CLOSEPRICE","TR.CLOSEPRICE", "TR.CLOSEPRICE", "TR.CLOSEPRICE", "TR.CLOSEPRICE", "TR.CLOSEPRICE", "TR.CLOSEPRICE")
                            , AAPL.O = c(75.0875, 74.3575, 74.95, 74.5975, 75.7975, 77.4075, 77.5825)
                            , NVDA.O = c(59.9775, 59.0175, 59.265, 59.9825, 60.095, 60.755, 61.08))
                       , row.names = c(NA, -7L), class = "data.frame"))
})


test_that("Process_RDP_output works without removing NA",{

  path = "test_rd_gethistory2.py"
  rd_gethistorytest_input2 <- reticulate::r_to_py(reticulate::py_load_object(file = path))
  r_df <- NULL
  r_df <- Process_RDP_output(rd_gethistorytest_input2, RemoveNA = FALSE)

  CorrectOuput <- structure(list(Date = structure(c(18536, 18536, 18562, 18562,
                                                    18563, 18563, 18564, 18564, 18565, 18565, 18567, 18567, 18568,
                                                    18568, 18569, 18569, 18570, 18570, 18571, 18571, 18572, 18572,
                                                    18575, 18575, 18576, 18576, 18577, 18577, 18578, 18578, 18579,
                                                    18579, 18582, 18582, 18583, 18583, 18584, 18584, 18585, 18585,
                                                    18586, 18586, 18589, 18589, 18590, 18590, 18591, 18591, 18592,
                                                    18593, 18593, 18596, 18596, 18597, 18597), class = "Date")
                                 , Instrument = c("AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L",
                                                  "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L",
                                                  "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L",
                                                  "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L")
                                 , `TR.CLOSEPRICE(ADJUSTED=0)/*CLOSE*/` = c(NA, NA, 116.6, 1.3828274792, 111.2, 1.337816086, 115.32, 1.3378488425,
                                                                            108.86, 1.3329192225, NA, NA, 108.77, 1.3686779346, 110.44, 1.3862446299,
                                                                            114.95, 1.3813431673, 119.03, 1.3943647164, 118.69, 1.3819052506,
                                                                            116.32, 1.4755630438, 115.97, 1.5484658669, 119.49, 1.5481840898,
                                                                            119.21, 1.5508694626, 119.26, 1.575990928, 120.3, 1.6855518474,
                                                                            119.39, 1.6225520702, 118.03, 1.6320789767, 118.64, 1.6133352783,
                                                                            117.34, 1.6360738478, 113.85, 1.6251432226, 115.17, 1.6737675798,
                                                                            116.03, 1.6641909885, 1.6784636342, 116.59, 1.6629165613, 119.05,
                                                                            1.6475509198, 122.72, 1.6827871162)
                                 , `TR.FREEFLOATPCT()/100/*FREEFLOATWEIGHT*/` = c(0.999315,
                                                                                  0.997444, NA, NA, NA, NA, NA, NA, NA, NA, 0.999313, 0.997442,
                                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.999208, 0.998256)
                                 , `TR.ISSUEMARKETCAP(SCALE=6,SHTYPE=FFL)` = c(NA, NA, 1981052.854803, 37009.2633343445, 1889305.981596, 35804.6022119214,
                                                                               1959305.44782059, 35805.4788891395, 1849549.0032063, 35673.545147514,
                                                                               NA, NA, 1848016.00382353, 36633.1884618125, 1876389.51422516,
                                                                               37103.3677805816, 1953014.98243555, 36972.1782599914, 2022334.69647067,
                                                                               37320.7050025456, 2016558.05363441, 36987.2226333778, 1976291.45504048,
                                                                               39494.0092950691, 1970344.91094433, 41445.2812411556, 2030150.15442561,
                                                                               41437.7393701587, 2025392.91914869, 41509.6144000369, 2026242.42544814,
                                                                               42182.0000320662, 2043912.1564767, 45114.4399480571, 2028451.14182671,
                                                                               43428.2268134631, 2005344.57048167, 43683.2181093227, 2015708.54733496,
                                                                               43181.5358513999, 1993621.38354926, 43790.1423621206, 1934325.84384765,
                                                                               43497.5799971177, 1956752.81015313, 44799.0295170547, 1971364.31850367,
                                                                               44542.7083893923, 44924.7212096551, 1980878.78905751, 44508.5978560931,
                                                                               2022674.49899045, 44097.3305852291, 2062915.83508192, 45077.2214542736)
                                 , `TR.ISSUESHARESOUTSTANDING(SCALE=3)/*SHARES OUTSTANDING*/` = c(NA, NA, 17001802, 26831892.312, 17001802, 26831892.312, 17001802,
                                                                                                  26831892.312, 17001802, 26831892.312, NA, NA, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26834018.032, 17001802, 26834018.032
                                                                                                  )), row.names = c(NA, -55L), class = "data.frame")

 expect_equal(r_df, CorrectOuput)
})

test_that("Process_RDP_output works with removing NA",{
  path = "test_rd_gethistory2.py"
  rd_gethistorytest_input2 <- reticulate::r_to_py(reticulate::py_load_object(file = path))


  r_df <- Process_RDP_output(rd_gethistorytest_input2, RemoveNA = TRUE)


  CorrectOuput <- structure(list(Date = structure(c(18536, 18536, 18562, 18562,
                                                    18563, 18563, 18564, 18564, 18565, 18565, 18567, 18567, 18568,
                                                    18568, 18569, 18569, 18570, 18570, 18571, 18571, 18572, 18572,
                                                    18575, 18575, 18576, 18576, 18577, 18577, 18578, 18578, 18579,
                                                    18579, 18582, 18582, 18583, 18583, 18584, 18584, 18585, 18585,
                                                    18586, 18586, 18589, 18589, 18590, 18590, 18591, 18591, 18592,
                                                    18593, 18593, 18596, 18596, 18597, 18597), class = "Date")
                                 , Instrument = c("AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L",
                                                  "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L",
                                                  "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L",
                                                  "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L", "VOD.L", "AAPL.O",
                                                  "VOD.L", "AAPL.O", "VOD.L", "AAPL.O", "VOD.L")
                                 , `TR.CLOSEPRICE(ADJUSTED=0)/*CLOSE*/` = c(NA, NA, 116.6, 1.3828274792, 111.2, 1.337816086, 115.32, 1.3378488425,
                                                                            108.86, 1.3329192225, NA, NA, 108.77, 1.3686779346, 110.44, 1.3862446299,
                                                                            114.95, 1.3813431673, 119.03, 1.3943647164, 118.69, 1.3819052506,
                                                                            116.32, 1.4755630438, 115.97, 1.5484658669, 119.49, 1.5481840898,
                                                                            119.21, 1.5508694626, 119.26, 1.575990928, 120.3, 1.6855518474,
                                                                            119.39, 1.6225520702, 118.03, 1.6320789767, 118.64, 1.6133352783,
                                                                            117.34, 1.6360738478, 113.85, 1.6251432226, 115.17, 1.6737675798,
                                                                            116.03, 1.6641909885, 1.6784636342, 116.59, 1.6629165613, 119.05,
                                                                            1.6475509198, 122.72, 1.6827871162)
                                 , `TR.FREEFLOATPCT()/100/*FREEFLOATWEIGHT*/` = c(0.999315, 0.997444, NA, NA, NA, NA, NA, NA, NA, NA, 0.999313, 0.997442,
                                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.999208, 0.998256)
                                 , `TR.ISSUEMARKETCAP(SCALE=6,SHTYPE=FFL)` = c(NA, NA, 1981052.854803, 37009.2633343445, 1889305.981596, 35804.6022119214,
                                                                               1959305.44782059, 35805.4788891395, 1849549.0032063, 35673.545147514,
                                                                               NA, NA, 1848016.00382353, 36633.1884618125, 1876389.51422516,
                                                                               37103.3677805816, 1953014.98243555, 36972.1782599914, 2022334.69647067,
                                                                               37320.7050025456, 2016558.05363441, 36987.2226333778, 1976291.45504048,
                                                                               39494.0092950691, 1970344.91094433, 41445.2812411556, 2030150.15442561,
                                                                               41437.7393701587, 2025392.91914869, 41509.6144000369, 2026242.42544814,
                                                                               42182.0000320662, 2043912.1564767, 45114.4399480571, 2028451.14182671,
                                                                               43428.2268134631, 2005344.57048167, 43683.2181093227, 2015708.54733496,
                                                                               43181.5358513999, 1993621.38354926, 43790.1423621206, 1934325.84384765,
                                                                               43497.5799971177, 1956752.81015313, 44799.0295170547, 1971364.31850367,
                                                                               44542.7083893923, 44924.7212096551, 1980878.78905751, 44508.5978560931,
                                                                               2022674.49899045, 44097.3305852291, 2062915.83508192, 45077.2214542736)
                                 , `TR.ISSUESHARESOUTSTANDING(SCALE=3)/*SHARES OUTSTANDING*/` = c(NA, NA, 17001802, 26831892.312, 17001802, 26831892.312, 17001802,
                                                                                                  26831892.312, 17001802, 26831892.312, NA, NA, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 17001802, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26832067.065, 26832067.065,
                                                                                                  17001802, 26832067.065, 17001802, 26834018.032, 17001802, 26834018.032
                                                                                                  )), row.names = c(NA, -55L), class = "data.frame")
  expect_equal(r_df, CorrectOuput)
})



test_that("Process_RDP_output works with nested responses",{

  path = "nestedpythonjson.py"
  nestedpythonjson_input <- reticulate::r_to_py(reticulate::py_load_object(file = path))

  r_df <- Process_RDP_output(nestedpythonjson_input, RemoveNA = FALSE, SpaceConvertor = NULL)


  correct_r_df <- structure(list(DTSubjectName = c("iShares Core S&P 500 UCITS ETF USD (Acc)",
                                                   "Invesco S&P 500 UCITS ETF Acc", "iShares Core MSCI World UCITS ETF USD (Acc)",
                                                   "iShares MSCI ACWI UCITS ETF USD (Acc)", "iShares MSCI EM Small Cap UCITS ETF USD (Dist)",
                                                   "iShares S&P 500 Info Technolg Sctr UCITS ETF USD A", "iShares Core MSCI Japan IMI UCITS ETF USD Acc",
                                                   "iShares Core MSCI EM IMI UCITS ETF USD Acc", "iShares MSCI China A UCITS ETF USD (Acc)",
                                                   "iShares MSCI World ESG Screened UCITS ETF USD Acc", "iShares S&P 500 Health Care Sctr UCITS ETF USD A",
                                                   "Xtrackers MSCI USA UCITS ETF 1C", "Invesco Nasdaq Biotech UCITS ETF Acc",
                                                   "WisdomTree Cloud Computing UCITS ETF USD Acc", "UBS MSCI ACWI SF UCITS ETF HUSD A acc",
                                                   "SPDR S&P 400 US Mid Cap UCITS ETF Acc", "iShares Edge MSCI Wld Val Fctr UCITS ETF USD A",
                                                   "KraneShares CSI China Internet UCITS ETF USD", "iShares MSCI EM Islamic UCITS ETF USD (Dist)",
                                                   "iShares MSCI EM IMI ESG Screened UCITS ETF USD A", "iShares MSCI Global Semiconductors UCITS ETF USD A",
                                                   "UBS MSCI USA SR UCITS ETF USD Aacc", "iShares MSCI EM ex-China UCITS ETF USD Acc",
                                                   "Amundi MSCI Emerging Ex China UCITS ETF Acc", "Guardian Ultra-Short U.S. T-Bill Fund ETF")
                                 , RIC = c("CSPX.L", "SPXS.L", "IWDA.L", "ISACI.L", "IEMS.L",
                                           "IUIT.L", "ISIJPA.L", "EIMI.L", "CNYA.L", "SAWD.L", "IUHC.L",
                                           "XD9U.L", "SBIO.L", "WCLD.L", "ACWIU.S", "SPY4.L", "IWVL.L",
                                           "KWEB.L", "ISDE.L", "SAEM.L", "SEMI.AS", "USSRI.S", "EXCH.AS",
                                           "LYEMXC.L", "GUTBu.TO")
                                 , BusinessEntity = c("QUOTExFUND", "QUOTExFUND",
                                                      "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND",
                                                      "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND",
                                                      "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND",
                                                      "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND", "QUOTExFUND",
                                                      "QUOTExFUND", "QUOTExFUND", "QUOTExFUND")
                                 , PI = c("75441532", "71623797", "63918368", "95981782", "57817022", "207657901",
                                          "63918057", "154611946", "170144917", "325113611", "207657897",
                                          "160669237", "171493596", "380426282", "201867038", "102303890",
                                          "162239537", "337081657", "41804748", "325113617", "532635872",
                                          "434128593", "503325602", "375753971", "711965020")
                                 , SearchAllCategoryv3 = c("Funds","Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                           "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                           "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                           "Funds", "Funds", "Funds")
                                 , SearchAllCategoryv2 = c("Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                           "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                           "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                           "Funds", "Funds", "Funds")
                                 , SearchAllCategory = c("Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                         "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                         "Funds", "Funds", "Funds", "Funds", "Funds", "Funds", "Funds",
                                                         "Funds", "Funds")
                                 , IssueISIN = c("IE00B5BMR087", "IE00B3YCGJ38", "IE00B4L5Y983", "IE00B6R52259", "IE00B3F81G20", "IE00B3WJKG14",
                                                 "IE00B4L5YX21", "IE00BKM4GZ66", "IE00BQT3WG13", "IE00BFNM3J75",
                                                 "IE00B43HR379", "IE00BJ0KDR00", "IE00BQ70R696", "IE00BJGWQN72",
                                                 "IE00BYM11J43", "IE00B4YBJ215", "IE00BP3QZB59", "IE00BFXR7892",
                                                 "IE00B27YCP72", "IE00BFNM3P36", "IE000I8KRLL9", "IE00BJXT3C94",
                                                 "IE00BMG6Z448", "LU2009202107", "CA40145B1076")
                                 , IssueLipperGlobalSchemeName = c("Equity US","Equity US", "Equity Global", "Equity Global", "Equity Emerging Mkts Global",
                                                                   "Equity Sector Information Tech", "Equity Japan", "Equity Emerging Mkts Global",
                                                                   "Equity China", "Equity Global", "Equity Sector Healthcare",
                                                                   "Equity US", "Equity Sector Biotechnology", "Equity Global",
                                                                   "Equity Global", "Equity US Sm&Mid Cap", "Equity Global", "Equity China",
                                                                   "Equity Emerging Mkts Global", "Equity Global", "Equity Sector Information Tech",
                                                                   "Equity US", "Equity Emerging Mkts Global", "Equity Emerging Mkts Global",
                                                                   NA)
                                 , RCSAssetCategoryLeaf = c("Equity ETF", "Equity ETF", "Equity ETF",
                                                            "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF",
                                                            "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF",
                                                            "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF",
                                                            "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF", "Equity ETF",
                                                            "Equity ETF", "Equity ETF")
                                 , RCSIssuerDomicileCountryLeaf = c("Ireland", "Ireland", "Ireland", "Ireland", "Ireland", "Ireland", "Ireland",
                                                                    "Ireland", "Ireland", "Ireland", "Ireland", "Ireland", "Ireland",
                                                                    "Ireland", "Ireland", "Ireland", "Ireland", "Ireland", "Ireland",
                                                                    "Ireland", "Ireland", "Ireland", "Ireland", "Luxembourg", "Canada"
                                                                    )
                                 , RCSIssueCountryRegisteredForSale1 = c("G:A4", "G:6V", "G:6V", "G:7J", "G:92", "G:A3", "G:7M", "G:3S", "G:7J", "G:55", "G:A3",
                                                                          "G:1F", "G:1F", "G:6V", "G:1F", "G:1F", "G:1F", "G:3D", "G:A4",
                                                                          "G:3D", "G:7K", "G:5J", "G:5M", "G:1F", NA)
                                 , RCSIssueCountryRegisteredForSale2 = c("G:1F", "G:7D", "G:2I", "G:6X", "G:7D", "G:92", "G:2V", "G:5J", "G:6X",
                                                                          "G:90", "G:92", "G:30", "G:30", "G:1F", "G:30", "G:30", "G:30",
                                                                          "G:55", "G:9Y", "G:19", "G:3N", "G:1F", "G:7J", "G:30", NA)
                                 , RCSIssueCountryRegisteredForSale3 = c("G:30", "G:1F", "G:92",
                                                                         "G:3S", "G:6V", "G:7D", "G:7K", "G:A9", "G:3S", "G:5M", "G:7D",
                                                                         "G:3D", "G:3D", "G:30", "G:3D", "G:3D", "G:3D", "G:90", "G:30",
                                                                         "G:55", "G:92", "G:30", "G:6X", "G:3D", NA)
                                 , RCSIssueCountryRegisteredForSale4 = c("G:4M",
                                                                         "G:30", "G:7D", "G:5J", "G:1F", "G:6V", "G:3N", "G:7M", "G:5J",
                                                                         "G:7J", "G:6V", "G:19", "G:19", "G:3D", "G:19", "G:19", "G:19",
                                                                         "G:7J", "G:3D", "G:90", "G:6V", "G:4M", "G:5J", "G:19", NA)
                                 , RCSIssueCountryRegisteredForSale5 = c("G:2E", "G:4M",
                                                                          "G:1C", "G:A9", "G:9Y", "G:1F", "G:5Y", "G:7K", "G:A9", "G:6X",
                                                                          "G:1F", "G:55", "G:55", "G:7K", "G:55", "G:55", "G:55", "G:6X",
                                                                          "G:55", "G:5M", "G:1F", "G:3D", "G:7M", "G:55", NA)
                                 , RCSIssueCountryRegisteredForSale6 = c("G:3D", "G:3D", "G:1F", "G:7M", "G:30", "G:30", "G:A3", "G:3N", "G:7M",
                                                                         "G:6I", "G:30", "G:90", "G:90", "G:7M", "G:90", "G:90", "G:90",
                                                                         "G:5J", "G:5M", "G:7J", "G:30", "G:19", "G:7K", "G:90", NA)
                                 , RCSIssueCountryRegisteredForSale7 = c("G:19", "G:19", "G:30", "G:7K", "G:3D", "G:3D", "G:92", "G:3T", "G:7K", "G:5J",
                                                                         "G:3D", "G:5M", "G:5M", "G:5J", "G:5M", "G:5M", "G:5M", "G:7K",
                                                                         "G:7J", "G:6X", "G:3D", "G:55", "G:3N", "G:5M", NA)
                                 , RCSIssueCountryRegisteredForSale8 = c("G:55", "G:55", "G:4M", "G:3N", "G:19", "G:19", "G:7D", "G:5Y", "G:3N",
                                                                         "G:A9", "G:19", "G:7J", "G:7J", "G:6X", "G:7J", "G:7J", "G:7J",
                                                                         "G:A3", "G:6X", "G:5J", "G:19", "G:90", "G:92", "G:7J", NA)
                                 , RCSIssueCountryRegisteredForSale9 = c("G:90", "G:90", "G:2E", "G:3T", "G:55", "G:5Y", "G:1C", "G:A3", "G:A3", "G:7M",
                                                                         "G:55", "G:6X", "G:6X", "G:7J", "G:6X", "G:6X", "G:6X", NA,
                                                                         "G:5J", "G:A9", "G:55", "G:5M", "G:6V", "G:5J", NA)
                                 , RCSIssueCountryRegisteredForSale10 = c("G:2I", "G:5M", "G:3D", "G:5Y", "G:90", "G:3N", "G:6V", "G:92", "G:92",
                                                                          "G:2V", "G:90", "G:5J", "G:6V", "G:5M", "G:5J", "G:5J", "G:5J",
                                                                          NA, "G:7M", "G:7M", "G:7M", "G:7J", "G:1F", "G:7M", NA)
                                 , RCSIssueCountryRegisteredForSale11 = c("G:6V", "G:7J", "G:19",
                                                                          "G:A3", "G:5M", "G:7K", "G:2I", "G:7D", "G:7D", "G:7K", "G:5M",
                                                                          "G:7M", "G:7D", "G:90", "G:41", "G:7M", "G:A9", NA, "G:7K",
                                                                          "G:7K", "G:A9", "G:6X", "G:30", "G:3N", NA)
                                 , RCSIssueCountryRegisteredForSale12 = c("G:1C", "G:6X", "G:55", "G:92", "G:7J", "G:7M", "G:1F", "G:1C", "G:6V",
                                                                          "G:3N", "G:7J", "G:6V", "G:A3", "G:55", "G:A9", "G:7K", "G:7M",
                                                                          NA, "G:A3", "G:3N", "G:5J", "G:6V", "G:3D", "G:6V", NA)
                                 , RCSIssueCountryRegisteredForSale13 = c("G:7D", "G:A3", "G:90",
                                                                          "G:7D", "G:6X", "G:A9", "G:30", "G:6V", "G:1F", "G:A3", "G:5Y",
                                                                          "G:A3", "G:3N", "G:19", "G:3N", "G:6V", "G:6V", NA, "G:92",
                                                                          "G:5Y", "G:6X", "G:7D", "G:19", NA, NA)
                                 , RCSIssueCountryRegisteredForSale14 = c("G:A3", "G:3N", "G:5M", "G:1C", "G:5J", "G:5J", "G:4M", "G:2I", "G:30",
                                                                          "G:92", "G:3N", "G:3N", "G:7K", NA, "G:6V", "G:3N", "G:7D",
                                                                          NA, "G:7D", "G:A3", "G:7J", "G:A3", "G:55", NA, NA)
                                 , RCSIssueCountryRegisteredForSale15 = c("G:3T", "G:7K", "G:7J", "G:6V", "G:7M", "G:3S", "G:2E", "G:1F", "G:4M",
                                                                          "G:6V", "G:7K", "G:7K", "G:7M", NA, "G:7D", NA, "G:92", NA,
                                                                          NA, "G:92", "G:5M", "G:3N", "G:90", NA, NA)
                                 , RCSIssueCountryRegisteredForSale16 = c("G:3N", "G:7M", "G:6X", "G:1F", "G:7K", "G:6X", "G:3D", "G:30", "G:3D",
                                                                          "G:1F", "G:7M", NA, "G:A9", NA, NA, NA, "G:A3", NA, NA, "G:1C",
                                                                          "G:90", "G:7K", NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale17 = c("G:7K", "G:A9", "G:5J", "G:9Y", "G:3N", "G:7J", "G:19", "G:4M", "G:19",
                                                                          "G:30", "G:5J", NA, "G:5J", NA, NA, NA, "G:5Y", NA, NA, "G:6V",
                                                                          NA, "G:7M", NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale18 = c("G:2V", "G:5J", "G:A9", "G:30", "G:5Y", "G:5M", "G:55", "G:2E", "G:55",
                                                                          "G:3D", "G:3S", NA, "G:3S", NA, NA, NA, "G:3N", NA, NA, "G:1F",
                                                                          NA, "G:A9", NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale19 = c("G:7M", "G:3S", "G:7M", "G:19", "G:A3", "G:90", "G:90", "G:3D", "G:90",
                                                                          "G:19", "G:6X", NA, NA, NA, NA, NA, "G:7K", NA, NA, "G:30",
                                                                          NA, NA, NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale20 = c("G:A9",NA, "G:2V", "G:55", NA, "G:55", "G:5M", "G:19", "G:5M", NA,
                                                                          NA, NA, NA, NA, NA, NA, NA, NA, NA, "G:2E", NA, NA, NA, NA,
                                                                          NA)
                                 , RCSIssueCountryRegisteredForSale21 = c("G:5J", NA, "G:A3","G:90", NA, NA, "G:7J", "G:55", NA, NA, NA, NA, NA, NA, NA,
                                                                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale22 = c("G:3S", NA, "G:5Y", "G:5M", NA, NA, "G:6X", "G:90", NA, NA, NA, NA,
                                                                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale23 = c("G:6X",NA, "G:3N", NA, NA, NA, "G:5J", "G:5M", NA, NA, NA, NA, NA,
                                                                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale24 = c("G:7J", NA, "G:7K", NA, NA, NA, "G:A9", "G:7J", NA, NA, NA, NA, NA,
                                                                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                 , RCSIssueCountryRegisteredForSale25 = c("G:5M",NA, NA, NA, NA, NA, NA, "G:6X", NA, NA, NA, NA, NA, NA, NA,
                                                                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                 , RCSCurrencyLeaf = c("US Dollar", "US Dollar", "US Dollar", "US Dollar", "US Dollar", "US Dollar",
                                                       "US Dollar", "US Dollar", "US Dollar", "US Dollar", "US Dollar",
                                                       "US Dollar", "US Dollar", "US Dollar", "US Dollar", "US Dollar",
                                                       "US Dollar", "US Dollar", "US Dollar", "US Dollar", "US Dollar",
                                                       "US Dollar", "US Dollar", "US Dollar", "US Dollar")
                                 , ExchangeName = c("London Stock Exchange", "London Stock Exchange", "London Stock Exchange", "London Stock Exchange",
                                                    "London Stock Exchange", "London Stock Exchange", "London Stock Exchange",
                                                    "London Stock Exchange", "London Stock Exchange", "London Stock Exchange",
                                                    "London Stock Exchange", "London Stock Exchange", "London Stock Exchange",
                                                    "London Stock Exchange", "SIX Swiss Exchange", "London Stock Exchange",
                                                    "London Stock Exchange", "London Stock Exchange", "London Stock Exchange",
                                                    "London Stock Exchange", "Euronext Amsterdam", "SIX Swiss Exchange",
                                                    "Euronext Amsterdam", "London Stock Exchange", "The Toronto Stock Exchange")
                                 , iNAVRIC = c("CSSPXUSD=INAV", NA, "IWDAUSD=INAV", "ISACUSD=INAV", "EMSCUSD=INAV", "421DINAV.DE^B23", "IJPAUSD=INAV", "X2EUUSD=INAV",
                                               "0J2NUSD=INAV", "19JFUSD=INAV", "3094INAV.DE^B23", "X2HHINAV.DE",
                                                "0J1KINAV.DE", NA, NA, NA, "X2JHINAV.DE^B23", "KWEBINAV.PA^F23",
                                                "ISDEUSDINAV.DE^B23", "19J1USD=INAV", "CXKFUSD=INAV", NA,
                                                "4J81USD=INAV", NA, NA)
                                 , RCSIssuerDomicileCountry = c("G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X",
                                                                "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X",
                                                                "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:6X", "G:7M", "G:8W"
                                                                )
                                 , RCSCurrency = c("C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6",
                                                   "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6", "C:6",
                                                   "C:6")
                                 , RCSAssetCategoryGenealogy1 = c("A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ",
                                                                  "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ", "A:5\\A:2X\\A:KZ")
                                 , RCSAssetCategoryGenealogy2 = c("A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ",
                                                                  "A:7\\A:GL\\A:2X\\A:KZ", "A:7\\A:GL\\A:2X\\A:KZ")), row.names = c(NA,-25L), class = "data.frame")
  expect_equal(r_df, correct_r_df)

  })
