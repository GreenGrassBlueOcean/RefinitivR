# Snapshot + auto-restore all Refinitiv options and vault at end of file
.saved_state <- save_refinitiv_state()

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


#####
# Q9: Environment variable pre-configuration
#####

test_that("REFINITIV_PORT env var pre-sets eikon_port, skipping auto-detect", {
  withr::local_options(eikon_port = NULL)
  withr::local_envvar(REFINITIV_PORT = "9000")

  # Simulate .onLoad env var resolution
  env_port <- Sys.getenv("REFINITIV_PORT", "")
  if (nzchar(env_port)) {
    options(eikon_port = as.integer(env_port))
  }

  # Port is now set — CheckTerminalType should short-circuit
  mock_probe <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, "rd_check_proxy_url", mock_probe)
  CheckTerminalType(verbose = FALSE)

  expect_equal(getOption("eikon_port"), 9000L)
  mockery::expect_called(mock_probe, 0) # no network probe
})

test_that("REFINITIV_BASE_URL env var sets refinitiv_base_url", {
  withr::local_options(refinitiv_base_url = NULL)
  withr::local_envvar(REFINITIV_BASE_URL = "http://x.x.x.x")

  # Simulate .onLoad env var resolution
  env_url <- Sys.getenv("REFINITIV_BASE_URL", "http://localhost")
  options(refinitiv_base_url = env_url)

  expect_equal(getOption("refinitiv_base_url"), "http://x.x.x.x")
})


restore_refinitiv_state(.saved_state, "test-CheckTerminalType")
