test_that("DataStream connection stop without password", {


  expect_error( DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = "real password")
              , "Please supply Datastream Username"
               )


  expect_error( DataStreamConnect(DatastreamUserName = "real username", DatastreamPassword = NA)
                , "Please supply Datastream Password"
  )



})
