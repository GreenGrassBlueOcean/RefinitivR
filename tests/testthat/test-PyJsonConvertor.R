test_that("PyJsonConvertor works", {
  x_py <- reticulate::py_run_string('obj = [2435823760, 123, ["abc", 1234567890987654321]];', convert = FALSE)
  expect_equal(PyJsonConvertor(x_py$obj), list(2435823760, 123L, list("abc", 1234567890987654400)))
})
