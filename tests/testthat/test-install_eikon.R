test_that("install_eikon works and libraries can be successfully loaded", {
  expect_error(install_eikon(), NA)

  expect_error({PythonEK <- reticulate::import(module = "eikon")}
               , NA)

  expect_error({PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon")}
               , NA)


})
