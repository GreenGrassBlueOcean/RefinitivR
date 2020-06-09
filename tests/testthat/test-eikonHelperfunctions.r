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


## WeekendDates -----

test_that("WeekendDates returns an error when it should", {
  expect_error(WeekendDates(MinDate = "2020-02-01", MaxDate = "2018-04-06"))
})

test_that("WeekendDates satisfies testcases", {
  expect_equal(WeekendDates(MinDate = "2020-06-12", MaxDate = "2020-06-15"), c(as.Date("2020-06-13"), as.Date("2020-06-14")))
  expect_equal(WeekendDates(MinDate = "2020-06-13", MaxDate = "2020-06-15"), c(as.Date("2020-06-13"), as.Date("2020-06-14")))
  expect_equal(WeekendDates(MinDate = "2020-06-13", MaxDate = "2020-06-14"), c(as.Date("2020-06-13"), as.Date("2020-06-14")))
  expect_equal(WeekendDates(MinDate = "2020-06-14", MaxDate = "2020-06-14"), c(as.Date("2020-06-14")))
})



