test_that("DataStream connection stop without password", {

  if(is.null(getOption("Datastream.Username")) && is.null(getOption("Datastream.Password")) ){


  expect_error( DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = "real password")
              , "Please supply Datastream Username"
               )


  expect_error( DataStreamConnect(DatastreamUserName = "real username", DatastreamPassword = NA)
                , "Please supply Datastream Password"
  )

  }



})
