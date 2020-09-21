test_that("InspectRequest works with a simple data.frame", {

    expect_message(InspectRequest(data.frame(), functionname = "test"), "test request returned with length 0")
    expect_message(InspectRequest(data.frame(test = c(1,2),test2 = c("a","b")), functionname = "test"), "test request returned with length 2")

})


test_that("InspectRequest returns NULL when not verbose", {

  expect_identical(InspectRequest(data.frame(), functionname = "test", verbose = FALSE), NULL)

})


test_that("InspectRequest with error structure",{

  df <- list( columnHeadersCount = 1L
            , data = list(list("WRONRIC", NULL,NULL))
            , error = list( list(code = 251658243L, col = 1L, message = "'The record could not be found' for the instrument 'WRONRIC'", row = 0L)
                          , list(code = 416L, col = 2L, message = "Unable to collect data for the field 'TR.CompanyName' and some specific identifier(s).",row = 0L))
            , headerOrientation = "horizontal", headers = list(list( list(displayName = "Instrument")
                                                                    , list(displayName = "RDN_EXCHD2", field = "RDN_EXCHD2")
                                                                    , list(displayName = "Company Name", field = "TR.COMPANYNAME")))
            , rowHeadersCount = 1L
            , totalColumnsCount = 3L
            , totalRowsCount = 2L
            )

    expect_message(InspectRequest(df = df, functionname = 'test', verbose = TRUE))



})
