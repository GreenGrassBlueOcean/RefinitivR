test_that("EikonConnect uses DEFAULT_WORKSPACE_APP_KEY when no key supplied", {
  original_key <- getOption(".EikonApiKey")
  original_port <- getOption("eikon_port")
  on.exit({
    options(.EikonApiKey = original_key, eikon_port = original_port)
    refinitiv_vault_clear(keys = "api_key")
    if (!is.null(original_key)) refinitiv_vault_set("api_key", original_key)
  }, add = TRUE)

  options(.EikonApiKey = NULL, eikon_port = 9000L)
  refinitiv_vault_clear(keys = "api_key")

  # Stub CheckTerminalType so we don't need a live terminal

  mockery::stub(EikonConnect, "CheckTerminalType", NULL)

  conn <- EikonConnect()
  expect_equal(refinitiv_vault_get("api_key"), "DEFAULT_WORKSPACE_APP_KEY")
})

test_that("EikonConnect warns on deprecated PythonModule values", {
  original_key <- getOption(".EikonApiKey")
  original_api <- getOption(".RefinitivAPI")
  original_vault_key <- refinitiv_vault_get("api_key")
  on.exit({
    options(.EikonApiKey = original_key, .RefinitivAPI = original_api)
    refinitiv_vault_clear(keys = "api_key")
    if (!is.null(original_vault_key)) refinitiv_vault_set("api_key", original_vault_key)
  }, add = TRUE)

  options(.EikonApiKey = "testing_key")

  expect_warning(RDConnect(PythonModule = "NA"),
                 "PythonModule parameter is deprecated")

  expect_warning(EikonConnect(PythonModule = NA),
                 "PythonModule parameter is deprecated")
})

test_that("EikonConnect errors when TestConnection is not logical", {
  original_key <- getOption(".EikonApiKey")
  on.exit(options(.EikonApiKey = original_key), add = TRUE)
  options(.EikonApiKey = "testing_key")

  expect_error(EikonConnect(TestConnection = "notalogical"),
               "TestConnection should be TRUE or FALSE",
               fixed = TRUE)
})

test_that("EikonConnect works with default PythonModule", {
  original_key <- getOption(".EikonApiKey")
  original_vault_key <- refinitiv_vault_get("api_key")
  on.exit({
    options(.EikonApiKey = original_key)
    refinitiv_vault_clear(keys = "api_key")
    if (!is.null(original_vault_key)) refinitiv_vault_set("api_key", original_vault_key)
  }, add = TRUE)
  options(.EikonApiKey = "testing_key")

  expect_no_error(EikonConnect(PythonModule = "JSON"))
})

test_that("RDConnect uses DEFAULT_WORKSPACE_APP_KEY when no key supplied", {
  original_key <- getOption(".EikonApiKey")
  original_port <- getOption("eikon_port")
  on.exit({
    options(.EikonApiKey = original_key, eikon_port = original_port)
    refinitiv_vault_clear(keys = "api_key")
    if (!is.null(original_key)) refinitiv_vault_set("api_key", original_key)
  }, add = TRUE)

  options(.EikonApiKey = NULL, eikon_port = 9000L)
  refinitiv_vault_clear(keys = "api_key")

  # Stub CheckTerminalType so we don't need a live terminal
  mockery::stub(RDConnect, "RefinitivJsonConnect", function(...) rlang::env())
  mockery::stub(RDConnect, "CheckTerminalType", NULL)

  conn <- RDConnect()
  expect_equal(refinitiv_vault_get("api_key"), "DEFAULT_WORKSPACE_APP_KEY")
})

test_that("EikonConnect uses explicit key when supplied", {
  original_key <- getOption(".EikonApiKey")
  original_port <- getOption("eikon_port")
  on.exit({
    options(.EikonApiKey = original_key, eikon_port = original_port)
    refinitiv_vault_clear(keys = "api_key")
    if (!is.null(original_key)) refinitiv_vault_set("api_key", original_key)
  }, add = TRUE)

  options(eikon_port = 9000L)
  mockery::stub(EikonConnect, "CheckTerminalType", NULL)

  conn <- EikonConnect(Eikonapplication_id = "my_custom_key")
  expect_equal(refinitiv_vault_get("api_key"), "my_custom_key")
})

dump_refinitiv_options("test-EikonConnect")
