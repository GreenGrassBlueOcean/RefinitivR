# Coverage tests for rd_custominstruments.r
# Targets 13 missed lines: 78-79, 81, 217, 266, 413, 425, 441-443, 446, 544-545
library(testthat)
library(mockery)

# ── CustomInstrumentUDCBuilder (lines 78-79, 81) ────────────────────────────

test_that("CustomInstrumentUDCBuilder returns not-implemented message", {
  result <- CustomInstrumentUDCBuilder(
    root = "CC", rollover = "volume", spreadAdjustment = "arithmetic"
  )
  expect_equal(result, "This function is not implemented yet")
})

# ── CheckifCustomInstrument with UUID = NULL (line 217) ─────────────────────

test_that("CheckifCustomInstrument returns NA when UUID is NULL", {
  result <- CheckifCustomInstrument(symbol = "test", UUID = NULL)
  expect_true(is.na(result))

  result2 <- CheckifCustomInstrument(symbol = c("A", "B", "C"), UUID = NULL)
  expect_equal(result2, c(NA, NA, NA))
  expect_length(result2, 3)
})

# ── CustomInstrumentBasketBuilder leveraged/short warning (line 266) ─────────

test_that("CustomInstrumentBasketBuilder warns for leveraged/short weights", {
  # Condition: !all(Weights > 0) & !(all(Weights < 1))
  # Needs at least one weight <= 0 AND at least one weight >= 1
  expect_warning(
    CustomInstrumentBasketBuilder(
      RICs = c("AAPL.O", "AMZN.O"),
      Weights = c(-0.5, 1.5)
    ),
    "leveraged or short elements"
  )
})

# ── rd_ManageCustomInstruments: valid holidays/basket/udc branches ───────────

# Helper: mock RDObject where GET says instrument does NOT exist (state$code != NULL)
mock_rd_not_exists <- function(create_return = list(result = "created")) {
  list(
    manage_custom_instrument = function(operation, symbol, ...) {
      list(state = list(code = "not_found"))
    },
    create_custom_instrument = function(symbol, formula, type, basket, udc,
                                        currency, instrumentName, exchangeName,
                                        holidays, timeZone, description, debug) {
      create_return
    }
  )
}

test_that("rd_ManageCustomInstruments strips holidays class for CREATE (line 413)", {
  holidays <- CustomInstrumentHolidayBuilder(
    dates = c("2023-12-25"),
    reasons = c("Christmas")
  )
  rd <- mock_rd_not_exists()
  stub(rd_ManageCustomInstruments, "CorrectCustomInstrument",
       function(symbol, UUID) paste0("S)", symbol, ".", UUID))

  result <- rd_ManageCustomInstruments(
    RDObject = rd, operation = "CREATE", symbol = "test",
    formula = "AAPL.O", holidays = holidays, UUID = "TEST-UUID"
  )
  expect_equal(result$result, "created")
})

test_that("rd_ManageCustomInstruments strips udc class for CREATE (lines 425, 446)", {
  udc <- structure(list(root = "CC"), class = "Refinitiv_udc")
  rd <- mock_rd_not_exists()
  stub(rd_ManageCustomInstruments, "CorrectCustomInstrument",
       function(symbol, UUID) paste0("S)", symbol, ".", UUID))

  result <- rd_ManageCustomInstruments(
    RDObject = rd, operation = "CREATE", symbol = "test",
    udc = udc, UUID = "TEST-UUID"
  )
  expect_equal(result$result, "created")
})

test_that("rd_ManageCustomInstruments with valid basket sets type and requires currency (lines 441-443)", {
  basket <- CustomInstrumentBasketBuilder(
    RICs = c("AAPL.O", "AMZN.O"),
    Weights = c(0.5, 0.5)
  )

  # Basket without currency → error at line 443
  stub(rd_ManageCustomInstruments, "CorrectCustomInstrument",
       function(symbol, UUID) paste0("S)", symbol, ".", UUID))
  expect_error(
    rd_ManageCustomInstruments(
      RDObject = mock_rd_not_exists(), operation = "CREATE", symbol = "test",
      basket = basket, currency = NULL, UUID = "TEST-UUID"
    ),
    "Parameter currency is required for baskets"
  )

  # Basket with currency → success (lines 441-442)
  rd <- mock_rd_not_exists()
  stub(rd_ManageCustomInstruments, "CorrectCustomInstrument",
       function(symbol, UUID) paste0("S)", symbol, ".", UUID))
  result <- rd_ManageCustomInstruments(
    RDObject = rd, operation = "CREATE", symbol = "test",
    basket = basket, currency = "USD", UUID = "TEST-UUID"
  )
  expect_equal(result$result, "created")
})

# ── rd_SearchCustomInstruments (lines 544-545) ──────────────────────────────

test_that("rd_SearchCustomInstruments calls search_custom_instrument", {
  mock_rd <- list(
    search_custom_instrument = function(debug) {
      list(instruments = list("S)test.UUID"))
    }
  )
  result <- rd_SearchCustomInstruments(RDObject = mock_rd, debug = FALSE)
  expect_equal(result$instruments, list("S)test.UUID"))
})
