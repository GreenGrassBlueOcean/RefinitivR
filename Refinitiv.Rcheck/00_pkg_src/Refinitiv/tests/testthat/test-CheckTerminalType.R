test_that("CheckTerminalType sets eikon_port on success", {
  original_eikon <- getOption("eikon_port")
  on.exit(options(eikon_port = original_eikon), add = TRUE)
  options(eikon_port = NULL)

  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, "rd_check_proxy_url", mock_rd_check_proxy_url)

  CheckTerminalType(verbose = FALSE)

  expect_equal(getOption("eikon_port"), 9000L)
  mockery::expect_called(mock_rd_check_proxy_url, 1)
})

test_that("CheckTerminalType warns when proxy is unreachable", {
  original_eikon <- getOption("eikon_port")
  on.exit(options(eikon_port = original_eikon), add = TRUE)
  options(eikon_port = NULL)

  mock_rd_check_proxy_url <- mockery::mock(stop("connection refused"))
  mockery::stub(CheckTerminalType, "rd_check_proxy_url", mock_rd_check_proxy_url)

  expect_warning(
    CheckTerminalType(),
    "No terminal connection.*LSEG Workspace"
  )
})

test_that("CheckTerminalType does not recheck when eikon_port is set and force = FALSE", {
  original_eikon <- getOption("eikon_port")
  on.exit(options(eikon_port = original_eikon), add = TRUE)
  options(eikon_port = 9000L)

  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, "rd_check_proxy_url", mock_rd_check_proxy_url)

  CheckTerminalType(verbose = FALSE, force = FALSE)

  mockery::expect_called(mock_rd_check_proxy_url, 0)
  expect_equal(getOption("eikon_port"), 9000L)
})

test_that("CheckTerminalType rechecks when force = TRUE", {
  original_eikon <- getOption("eikon_port")
  on.exit(options(eikon_port = original_eikon), add = TRUE)
  options(eikon_port = 9000L)

  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, "rd_check_proxy_url", mock_rd_check_proxy_url)

  CheckTerminalType(verbose = FALSE, force = TRUE)

  mockery::expect_called(mock_rd_check_proxy_url, 1)
  expect_equal(getOption("eikon_port"), 9000L)
})

test_that("CheckTerminalType prints verbose message on success", {
  original_eikon <- getOption("eikon_port")
  on.exit(options(eikon_port = original_eikon), add = TRUE)
  options(eikon_port = NULL)

  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, "rd_check_proxy_url", mock_rd_check_proxy_url)

  expect_message(
    CheckTerminalType(verbose = TRUE),
    "LSEG Workspace detected on port 9000"
  )
})

dump_refinitiv_options("test-CheckTerminalType")
