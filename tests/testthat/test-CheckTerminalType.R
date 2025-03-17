test_that("CheckTerminalType correctly sets eikon_port for Eikon", {
  # Save original options
  original_eikon <- getOption("eikon_port")
  original_rdp <- getOption("rdp_port")
  on.exit(options(eikon_port = original_eikon, rdp_port = original_rdp), add = TRUE)

  # Ensure both options are unset so defaults are used
  options(eikon_port = NULL, rdp_port = NULL)

  # Simulate failure on the Workspace port (9000) and success on the Eikon port (9060)
  mock_rd_check_proxy_url <- mockery::mock(
    # First call: workspace (9000) fails
    throws("Error on port 9000"),
    # Second call: Eikon (9060) succeeds
    TRUE
  )
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Run the function
  CheckTerminalType(verbose = FALSE)

  # Since Workspace connectivity failed, Eikon connectivity should succeed (9060)
  expect_equal(getOption("eikon_port"), 9060L)
})

test_that("CheckTerminalType correctly sets eikon_port for Workspace", {
  original_eikon <- getOption("eikon_port")
  original_rdp <- getOption("rdp_port")
  on.exit(options(eikon_port = original_eikon, rdp_port = original_rdp), add = TRUE)

  options(eikon_port = NULL, rdp_port = NULL)

  # Simulate successful connectivity on the Workspace port (9000)
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  CheckTerminalType(verbose = FALSE)

  # Expect that the first check passes and the port is set to 9000
  expect_equal(getOption("eikon_port"), 9000L)
})

test_that("CheckTerminalType handles no terminal connection", {
  original_eikon <- getOption("eikon_port")
  original_rdp <- getOption("rdp_port")
  on.exit(options(eikon_port = original_eikon, rdp_port = original_rdp), add = TRUE)

  options(eikon_port = NULL, rdp_port = NULL)

  # Simulate failure on both ports: first call (9000) fails, second call (9060) fails
  mock_rd_check_proxy_url <- mockery::mock(
    throws("Error on port 9000"),
    throws("Error on port 9060")
  )
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  expect_warning(CheckTerminalType(),
                 "There is no terminal connection. Please make sure Eikon or Workspace Desktop is running.")
})

test_that("CheckTerminalType does not recheck if eikon_port is set and force is FALSE", {
  original_eikon <- getOption("eikon_port")
  original_rdp <- getOption("rdp_port")
  on.exit(options(eikon_port = original_eikon, rdp_port = original_rdp), add = TRUE)

  # Pre-set eikon_port (simulate a previously detected state)
  options(eikon_port = 9060L, rdp_port = NULL)

  # Stub rd_check_proxy_url but expect it not to be called
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  CheckTerminalType(verbose = FALSE, force = FALSE)

  # Ensure the stub was not called
  mockery::expect_called(mock_rd_check_proxy_url, 0)
  expect_equal(getOption("eikon_port"), 9060L)
})

test_that("CheckTerminalType rechecks when force is TRUE", {
  original_eikon <- getOption("eikon_port")
  original_rdp <- getOption("rdp_port")
  on.exit(options(eikon_port = original_eikon, rdp_port = original_rdp), add = TRUE)

  # Start with a preset value (simulate a prior state)
  options(eikon_port = 9060L, rdp_port = NULL)

  # Simulate successful workspace connectivity (port 9000) on recheck
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  CheckTerminalType(verbose = FALSE, force = TRUE)

  # With force=TRUE and the workspace check succeeding, the port should now be 9000
  expect_equal(getOption("eikon_port"), 9000L)
})

test_that("CheckTerminalType prints verbose messages", {
  original_eikon <- getOption("eikon_port")
  original_rdp <- getOption("rdp_port")
  on.exit(options(eikon_port = original_eikon, rdp_port = original_rdp), add = TRUE)

  options(eikon_port = NULL, rdp_port = NULL)

  # Simulate workspace connectivity on port 9000
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  expect_message(CheckTerminalType(verbose = TRUE),
                 "Workspace detected, setting port 9000 for Eikon/UDF use.")
})
