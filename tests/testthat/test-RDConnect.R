library(testthat)
library(mockery)

# Test 1: application_id missing and no option ".EikonApiKey" is set.
test_that("RDConnect errors when application_id is not supplied", {
  oldEikon <- getOption(".EikonApiKey")
  options(.EikonApiKey = NULL)
  expect_error(
    RDConnect(application_id = NA, PythonModule = "JSON"),
    "Please supply application_id"
  )
  options(.EikonApiKey = oldEikon)
})

# Test 2: Invalid PythonModule value.
test_that("RDConnect errors with invalid PythonModule", {
  expect_error(
    RDConnect(application_id = "dummy_key", PythonModule = "FOO"),
    "RDConnect parameter PythonModule can only be RD \\(python\\) or JSON \\(direct JSON message\\) but is FOO"
  )
})

# Test 3: When PythonModule is "JSON", it should call RefinitivJsonConnect().
test_that("RDConnect returns RefinitivJsonConnect output when PythonModule is JSON", {
  dummy_obj <- list(dummy = "json")
  stub(RDConnect, "RefinitivJsonConnect", function() dummy_obj)
  result <- RDConnect(application_id = "dummy_key", PythonModule = "JSON")
  expect_equal(result, dummy_obj)
})

# Test 4: When PythonModule is "RD" and CondaExists() returns FALSE, it errors.
test_that("RDConnect errors when PythonModule is RD and CondaExists returns FALSE", {
  stub(RDConnect, "CondaExists", function() FALSE)
  expect_error(
    RDConnect(application_id = "dummy_key", PythonModule = "RD"),
    "Conda/reticulate does not seem to be available please run install_eikon or change parameter PythonModule to 'JSON'"
  )
})

# Test 5: When PythonModule is "RD" and CondaExists() returns TRUE, it should import and return the RD object.
test_that("RDConnect returns RD object when PythonModule is RD and CondaExists returns TRUE", {
  # Stub CondaExists to return TRUE.
  stub(RDConnect, "CondaExists", function() TRUE)
  # Stub reticulate::use_miniconda to do nothing.
  stub(RDConnect, "reticulate::use_miniconda", function(condaenv) TRUE)

  # Create a dummy RD object simulating the imported module.
  dummy_rd <- list(
    open_session = function() { },
    dummy_property = "rd_object"
  )
  # Stub reticulate::import to return dummy_rd.
  stub(RDConnect, "reticulate::import", function(module, convert, delay_load) dummy_rd)
  # Stub GetandSetPyModuleNameandVersion to do nothing.
  stub(RDConnect, "GetandSetPyModuleNameandVersion", function(rd) NULL)

  result <- RDConnect(application_id = "dummy_key", PythonModule = "RD")
  expect_equal(result, dummy_rd)
})
