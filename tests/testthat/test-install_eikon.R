test_that("install_eikon works and libraries can be successfully loaded", {

  my_packages <- library()$results[,1]

  if("Refinitiv" %in% my_packages){
        print("Refinitiv Package loaded so install should not work")
        expect_error( install_eikon())
  } else {
    expect_error(install_eikon(), NA)
  }

  #expect_error({PythonEK <- reticulate::import(module = "eikon")}
  #             , NA)

  #expect_error({PythonEK <- reticulate::import(module = "refinitiv.dataplatform.eikon")}
  #             , NA)


})
