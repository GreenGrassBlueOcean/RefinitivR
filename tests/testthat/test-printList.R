library(testthat)

test_that("printList outputs correct messages for a named list", {
  # Create a named list to test.
  test_list <- list(
    error1 = list(code = 218L, col = 1L,
                  message = "The formula must contain at least one field or function.", row = 0L),
    error2 = list(code = 218L, col = 1L,
                  message = "The formula must contain at least one field or function.", row = 1L)
  )

  # Capture the messages produced by printList.
  msg_output <- capture_messages(printList(list = test_list))

  # Check that the output contains the names of the list elements.
  expect_true(any(grepl("error1", msg_output)))
  expect_true(any(grepl("error2", msg_output)))

  # Check that parts of the inner lists are printed (e.g. "218" and the warning message).
  expect_true(any(grepl("218", msg_output)))
  expect_true(any(grepl("The formula must contain at least one field or function", msg_output)))
})

test_that("printList respects the hn argument", {
  # Create a list whose inner elements have more than three items.
  test_list <- list(
    warning1 = list(a = 1, b = 2, c = 3, d = 4, e = 5),
    warning2 = list(x = "alpha", y = "beta", z = "gamma", w = "delta")
  )

  # Capture messages using hn = 3.
  msg_output <- capture_messages(printList(list = test_list, hn = 3))

  # The function calls message twice per list element (once for the name, once for the head output)
  # so for 2 elements we expect 4 message lines.
  expect_equal(length(msg_output), 4)

  # Verify that the printed head output for the first element does not include the 4th item.
  expect_false(grepl("4", msg_output[2]))
  # And similarly for the second element, the head output should not include "delta" (the 4th element).
  expect_false(grepl("delta", msg_output[4]))
})

test_that("printList handles an unnamed list gracefully", {
  # Create an unnamed list.
  test_list <- list(
    list(a = 10, b = 20),
    list(x = "foo", y = "bar")
  )

  # Capture the messages.
  msg_output <- capture_messages(printList(list = test_list))

  # Since the list is unnamed, the name printing will be empty,
  # but the inner list contents should still appear.
  expect_true(any(grepl("10", msg_output)))
  expect_true(any(grepl("foo", msg_output)))
})

test_that("printList handles an empty list by throwing an error", {
  # An empty list will cause the loop to attempt to index an element that doesn't exist.
  test_list <- list()
  expect_error(printList(list = test_list), "subscript out of bounds")
})
