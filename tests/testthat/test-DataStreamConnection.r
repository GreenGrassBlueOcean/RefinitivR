test_that("TestDataStreamCredentials fails when it should", {
  expect_error(TestDataStreamCredentials(), "Please Supply Datastream Username")
  expect_error(TestDataStreamCredentials(DatastreamUsername = "someuser"), "Please Supply Datastream Password")
})


test_that("TestDataStreamCredentials satisfies test cases", {
  expect_error(expect_false(isTRUE(TestDataStreamCredentials(DatastreamUsername = "wrong username", DatastreamPassword = "wrong Password"))))
})



