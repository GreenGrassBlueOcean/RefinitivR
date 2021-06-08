requireNamespace("reticulate")

test_that("eikon can be imported in python", {
  expect_error({PythonEK <- reticulate::import(module = "eikon")}
              , NA)

})

test_that("Refinitiv dataplatform can be imported in python", {
  expect_error({PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon")}
               , NA)


})
