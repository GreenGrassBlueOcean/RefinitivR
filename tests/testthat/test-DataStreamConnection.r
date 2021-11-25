test_that("TestDataStreamCredentials fails when it should", {
  expect_error(TestDataStreamCredentials(), "Please Supply Datastream Username")
  expect_error(TestDataStreamCredentials(DatastreamUsername = "someuser"), "Please Supply Datastream Password")
})




test_that("TestDataStreamCredentials satisfies test cases", {

  is.bad <- function(code) {
    isTRUE(tryCatch(code,
                    error = function(c) TRUE,
                    warning = function(c) TRUE
    ))
  }

  expect_true(is.bad(TestDataStreamCredentials(DatastreamUsername = "wrong username", DatastreamPassword = "wrong Password")))

})



