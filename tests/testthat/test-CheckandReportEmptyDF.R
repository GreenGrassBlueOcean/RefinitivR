# test_that("InspectRequest works with a simple data.frame", {
#
#   expect_false(InspectRequest(data.frame(), functionname = "test"))
#   expect_true(InspectRequest(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test"))
#
# })
#
#
# test_that("InspectRequest with complex eikon list structure",{
#
#
#   expect_true(InspectRequest(df =   list(structure(list(Instrument = "MMM", `Company Name` = "3M Co")
#                                                 , class = "data.frame", row.names = c(NA, -1L)), NULL)
#                                     , functionname = 'test'
#               ))
#
#
# })
#
#
# test_that("InspectRequest can work with NA",{
#
#
#   expect_false(InspectRequest(df = NA, functionname = 'test'))
#
#
# })
