test_that("GetandSetPyModuleNameandVersion works", {

  OldModuleName <- getOption(".RefinitivPyModuleName")
  OldVersion <- getOption(".RefinitivPyModuleVersion")
  OldType <- getOption(".RefinitivPyModuleType")

  test <- reticulate::import(module = "numpy", delay_load = T)
  GetandSetPyModuleNameandVersion(test)

  TestModuleName <- getOption(".RefinitivPyModuleName")
  TestVersion <- getOption(".RefinitivPyModuleVersion")
  TestType <- getOption(".RefinitivPyModuleType")


  expect_equal(TestModuleName ,"numpy")
  expect_true(is.character(TestVersion))
  expect_equal(TestType, c("python.builtin.module", "python.builtin.object"))

  test_python_object <- PropertiesActiveRefinitivObject(verbose = FALSE)

  expect_equal(object = test_python_object
              , expected = list("name" = TestModuleName, "version" = TestVersion, "Type" = TestType))


  options(.RefinitivPyModuleName = OldModuleName)
  options(.RefinitivPyModuleVersion = OldVersion)
  options(.RefinitivPyModuleType = OldType)
})
