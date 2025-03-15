library(testthat)
library(mockery)
library(data.table)

context("Testing rd_ManageCustomInstruments")

### Helper dummy functions and objects

# Dummy CorrectCustomInstrument: simply appends "_corrected" to the symbol.
dummy_CorrectCustomInstrument <- function(symbol, UUID) {
  paste0(symbol, "_corrected")
}

# Create dummy objects for basket, udc, holidays with correct classes.
dummy_basket_valid <- structure(list(basket = "dummy_basket"), class = "Refinitiv_basket")
dummy_udc_valid <- structure(list(udc = "dummy_udc"), class = "Refinitiv_udc")
dummy_holidays_valid <- structure(list(holidays = "dummy_holidays"), class = "Refinitiv_holidays")

# Create a dummy RD connection object with stubbed methods.
# get_exists: dummy object returned by manage_custom_instrument(GET).
# If get_exists$state$code is NOT NULL, instrument does NOT exist.
# If get_exists$state$code is NULL, instrument exists.
create_dummy_RD <- function(get_exists, create_result = NULL, update_result = NULL, delete_result = NULL) {
  dummy_RD <- list(
    manage_custom_instrument = function(operation, symbol, ...) {
      if (toupper(operation) == "GET") {
        return(get_exists)
      }
      if (toupper(operation) %in% c("UPDATE", "DELETE")) {
        return(list(result = paste0("Operation ", toupper(operation), " executed on ", symbol)))
      }
      stop("Unexpected operation in manage_custom_instrument")
    },
    create_custom_instrument = function(symbol, formula, type, basket, udc, currency,
                                        instrumentName, exchangeName, holidays, timeZone, description, debug) {
      return(create_result)
    }
  )
  return(dummy_RD)
}

### Test Group 1: Input Validation

test_that("errors with invalid operation", {
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = NULL, symbol = "SYM"),
    "Parameter operation can not be null"
  )
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = "FOO", symbol = "SYM"),
    "operation should be one of 'CREATE','GET', 'UPDATE', 'DELETE'"
  )
})

test_that("errors when UUID is missing", {
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = "CREATE", symbol = "SYM", formula = "AAPL.O", UUID = NULL),
    "Parameter UUID has to be set"
  )
})

test_that("errors if more than one type is supplied", {
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = "CREATE", symbol = "SYM",
                               formula = "AAPL.O", basket = dummy_basket_valid, UUID = "dummy-uuid"),
    regexp = "Only ONE single type of formula, basket, or user defin.*continuation \\(udc\\) has to be supplied"
  )

})

test_that("errors when basket is provided but not valid", {
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = "CREATE", symbol = "SYM",
                               basket = list(basket = "notvalid"), currency = "USD", UUID = "dummy-uuid"),
    "Parameter basket is not defined correctly"
  )
})

test_that("errors when holidays is provided but not valid", {
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = "CREATE", symbol = "SYM",
                               formula = "AAPL.O", holidays = list(holidays = "notvalid"), UUID = "dummy-uuid"),
    "Parameter holidays is not defined correctly"
  )
})

test_that("errors when udc is provided but not valid", {
  expect_error(
    rd_ManageCustomInstruments(RDObject = list(), operation = "CREATE", symbol = "SYM",
                               formula = "AAPL.O", udc = list(udc = "notvalid"), UUID = "dummy-uuid"),
    "Parameter udc is not defined correctly"
  )
})

### Test Group 2: CREATE branch

test_that("CREATE branch calls create_custom_instrument when instrument does not exist", {
  # Simulate that instrument does NOT exist (state code is not NULL).
  get_exists <- list(state = list(code = "dummy_code"))
  dummy_RD <- create_dummy_RD(get_exists, create_result = list(result = "Created"))

  # Stub CorrectCustomInstrument.
  stub(rd_ManageCustomInstruments, "CorrectCustomInstrument", dummy_CorrectCustomInstrument)

  res <- rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "CREATE",
                                    symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid")
  expect_equal(res$result, "Created")
})

test_that("CREATE branch errors if instrument already exists", {
  # Simulate that instrument exists (state code is NULL).
  get_exists <- list(state = list(code = NULL))
  dummy_RD <- create_dummy_RD(get_exists)
  stub(rd_ManageCustomInstruments, "CorrectCustomInstrument", dummy_CorrectCustomInstrument)

  expect_error(
    rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "CREATE",
                               symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid"),
    "Instrument is already be created and should be deleted or updated first if you want to make changes"
  )
})

### Test Group 3: GET branch

test_that("GET branch returns instrument details if exists", {
  # Simulate that instrument exists (state code is NULL).
  get_exists <- list(state = list(code = NULL), details = "existing details")
  dummy_RD <- create_dummy_RD(get_exists)

  res <- rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "GET",
                                    symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid")
  expect_equal(res, get_exists)
})

test_that("GET branch errors if instrument does not exist", {
  # Simulate that instrument does NOT exist (state code is not NULL).
  get_exists <- list(state = list(code = "dummy_code"))
  dummy_RD <- create_dummy_RD(get_exists)

  expect_error(
    rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "GET",
                               symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid"),
    "Instrument did not exist and can therefore not be obtained"
  )
})

### Test Group 4: UPDATE branch

test_that("UPDATE branch calls update when instrument exists", {
  # Simulate that instrument exists (state code is NULL).
  get_exists <- list(state = list(code = NULL))
  dummy_RD <- create_dummy_RD(get_exists, update_result = list(result = "Updated"))

  res <- rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "UPDATE",
                                    symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid")
  # We use regex matching to allow for variation in the corrected symbol.
  expect_match(res$result, "^Operation UPDATE executed on .*")
})

test_that("UPDATE branch errors if instrument does not exist", {
  # Simulate that instrument does NOT exist (state code is not NULL).
  get_exists <- list(state = list(code = "dummy_code"))
  dummy_RD <- create_dummy_RD(get_exists)

  expect_error(
    rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "UPDATE",
                               symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid"),
    "Instrument did not exist and can therefore not be updated"
  )
})

### Test Group 5: DELETE branch

test_that("DELETE branch calls delete when instrument exists", {
  # Simulate that instrument exists (state code is NULL).
  get_exists <- list(state = list(code = NULL))
  dummy_RD <- create_dummy_RD(get_exists, delete_result = list(result = "Deleted"))

  res <- rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "DELETE",
                                    symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid")
  expect_equal(res, NULL)  # DELETE branch returns NULL after deletion.
})

test_that("DELETE branch errors if instrument does not exist", {
  # Simulate that instrument does NOT exist (state code is not NULL).
  get_exists <- list(state = list(code = "dummy_code"))
  dummy_RD <- create_dummy_RD(get_exists)

  expect_error(
    rd_ManageCustomInstruments(RDObject = dummy_RD, operation = "DELETE",
                               symbol = "SYM", formula = "AAPL.O", UUID = "dummy-uuid"),
    "Instrument did not exist and can therefore not be deleteted"
  )
})
