test_that("CheckandReportEmptyDF works with a simple data.frame", {

  expect_false(CheckandReportEmptyDF(data.frame(), functionname = "test"))
  expect_true(CheckandReportEmptyDF(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test"))

})


test_that("CheckandReportEmptyDF with complex eikon list structure",{


  expect_true(CheckandReportEmptyDF(df =   list(structure(list(Instrument = "MMM", `Company Name` = "3M Co")
                                                , class = "data.frame", row.names = c(NA, -1L)), NULL)
                                    , functionname = 'test'
              ))


})


test_that("CheckandReportEmptyDF can work with NA",{


  expect_false(CheckandReportEmptyDF(df = NA, functionname = 'test'))


})
