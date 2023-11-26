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
  # path = "test_rd_gethistory2.py"

  #test_python <-  rd_GetHistory(RD = RDConnect(PythonModule = "RD"),  universe= c("AAPL.O", "VOD.L")
  #                              , fields = c("TR.IssueMarketCap(Scale=6,ShType=FFL)","TR.FreeFloatPct()/100/*FreefloatWeight*/"
  #                                           ,"TR.IssueSharesOutstanding(Scale=3)/*shares outstanding*/","TR.CLOSEPRICE(Adjusted=0)/*close*/")
  #                             , parameters = list("Curn" = "USD", "SDate" = "2020-10-27", "EDate" = "2020-12-01"))
  # breakpoint in function
  #reticulate::py_save_object(object = JsonString, filename = "fullpath/test_rd_gethistory2.py")

  path = "test_rd_gethistory2.py"
  rd_gethistorytest_input2 <- reticulate::r_to_py(reticulate::py_load_object(file = path))
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

