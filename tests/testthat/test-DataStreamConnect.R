test_that("DataStream connection stop without password", {

  if(is.null(getOption("Datastream.Username")) && is.null(getOption("Datastream.Password")) ){

  print(DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = "real password"))
  expect_error( DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = "real password")
              , "Please supply Datastream Username"
               )

  print(DataStreamConnect(DatastreamUserName = "real username", DatastreamPassword = NA))
  expect_error( DataStreamConnect(DatastreamUserName = "real username", DatastreamPassword = NA)
                , "Please supply Datastream Password"
  )

  }



})
