test_that("CheckTerminalType correctly sets eikon_port for Eikon", {
  # Save the original option
  original_port <- getOption("eikon_port")
  on.exit(options(eikon_port = original_port), add = TRUE)

  # Set the option to NULL before the test
  options(eikon_port = NULL)

  # Mock the rd_check_proxy_url function to simulate Eikon running on port 9060
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Run the function
  CheckTerminalType(verbose = FALSE)

  # Check that eikon_port is set to 9060
  expect_equal(getOption("eikon_port"), 9060L)
})

test_that("CheckTerminalType correctly sets eikon_port for Workspace", {
  # Save the original option
  original_port <- getOption("eikon_port")
  on.exit(options(eikon_port = original_port), add = TRUE)

  # Set the option to NULL before the test
  options(eikon_port = NULL)

  # Simulate failure on port 9060 and success on port 9000
  mock_rd_check_proxy_url <- mockery::mock(throws(error("Error on port 9060")), TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Run the function
  CheckTerminalType(verbose = FALSE)

  # Check that eikon_port is set to 9000
  expect_equal(getOption("eikon_port"), 9000L)
})

test_that("CheckTerminalType handles no terminal connection", {
  # Save the original option
  original_port <- getOption("eikon_port")
  on.exit(options(eikon_port = original_port), add = TRUE)

  # Set the option to NULL before the test
  options(eikon_port = NULL)

  # Mock rd_check_proxy_url to simulate failure on both ports
  mock_rd_check_proxy_url <- mockery::mock(throws(error("Error on port 9060")), throws(error("Error on port 9000")))
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Expect the function to throw an error
  expect_error(CheckTerminalType(),
               "There is no terminal connection. Please make sure Eikon or Workspace Desktop is running.")
})

test_that("CheckTerminalType does not recheck if eikon_port is set and force is FALSE", {
  # Save the original option
  original_port <- getOption("eikon_port")
  on.exit(options(eikon_port = original_port), add = TRUE)

  # Manually set eikon_port before the test
  options(eikon_port = 9060L)

  # Mock the rd_check_proxy_url function but don't expect it to be called
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Run the function
  CheckTerminalType(verbose = FALSE, force = FALSE)

  # Check that rd_check_proxy_url was not called
  mockery::expect_called(mock_rd_check_proxy_url, 0)
  expect_equal(getOption("eikon_port"), 9060L)
})

test_that("CheckTerminalType rechecks when force is TRUE", {
  # Save the original option
  original_port <- getOption("eikon_port")
  on.exit(options(eikon_port = original_port), add = TRUE)

  # Manually set eikon_port before the test
  options(eikon_port = 9000L)

  # Mock the rd_check_proxy_url function to simulate Eikon running on port 9060
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Run the function with force = TRUE
  CheckTerminalType(verbose = FALSE, force = TRUE)

  # Check that eikon_port is reset to 9060
  expect_equal(getOption("eikon_port"), 9060L)
})

test_that("CheckTerminalType prints verbose messages", {
  # Save the original option
  original_port <- getOption("eikon_port")
  on.exit(options(eikon_port = original_port), add = TRUE)

  # Set the option to NULL before the test
  options(eikon_port = NULL)

  # Mock the rd_check_proxy_url function to simulate Eikon running on port 9060
  mock_rd_check_proxy_url <- mockery::mock(TRUE)
  mockery::stub(CheckTerminalType, 'rd_check_proxy_url', mock_rd_check_proxy_url)

  # Capture the output from verbose messages
  expect_message(CheckTerminalType(verbose = TRUE), "Eikon detected, setting port 9060 for Eikon/UDF use.")
})
