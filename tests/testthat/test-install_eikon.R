test_that("install_eikon works and libraries can be successfully loaded", {

  #some special handling for windows
  if (Sys.info()["sysname"] == "Windows") {

    # avoid DLL in use errors
    if (reticulate::py_available()) {
      skip()
    } else {
      expect_error(install_eikon(), NA)
    }
  } else {
    expect_error(install_eikon(), NA)
  }

  #expect_error({PythonEK <- reticulate::import(module = "eikon")}
  #             , NA)

  #expect_error({PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon")}
  #             , NA)


})

test_that("CheckInstallationResult fails with incorrect python package", {


  expect_warning(CheckInstallationResult(PyhtonModuleName = "wrongmodule")
                 , "Installation of python module wrongmodule failed")

})
