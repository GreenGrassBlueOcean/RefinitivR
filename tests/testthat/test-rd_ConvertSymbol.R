library(testthat)
library(mockery)
library(data.table)

# --- Helpers ---
mock_symbology_success <- function(mapped) {
  function(symbol, ...) data.table(Input = symbol, bestMatch = mapped, RICs = mapped)
}

mock_eikon_active <- function(ric, active = TRUE) {
  list(PostProcessedEikonGetData = data.frame(
    Instrument = ric, Instrument.Is.Active.Flag = active,
    stringsAsFactors = FALSE
  ))
}

mock_eikon_primary <- function(input, primary, active = TRUE) {
  list(PostProcessedEikonGetData = data.frame(
    Instrument = input, Primary.Instrument.RIC = primary,
    Instrument.Is.Active.Flag = active, stringsAsFactors = FALSE
  ))
}

mock_history <- function(input, chain) {
  df <- data.frame(Date = paste0("2020-01-0", seq_along(chain)),
                   stringsAsFactors = FALSE)
  df[[input]] <- chain
  df
}

# --- Tier 1: Standard Symbology ---

test_that("Tier 1: resolves active RIC via symbology", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("AAPL.O"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol(symbols = "AAPL.O")
  expect_equal(result$MappedSymbol, "AAPL.O")
  expect_equal(result$ResolutionTier, "symbology")
  expect_true(result$IsActive)
})

test_that("Tier 1: ISIN to RIC conversion", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...)
    data.table(Input = symbol, bestMatch = "AAPL.O", RICs = "AAPL.O"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol("US0378331005",
                             from_symbol_type = "ISIN", to_symbol_type = "RIC")
  expect_equal(result$MappedSymbol, "AAPL.O")
  expect_equal(result$ResolutionTier, "symbology")
})

test_that("Tier 1: inactive symbology match still returned", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("OLD.L^K25"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))

  result <- rd_ConvertSymbol("OLD.L")
  expect_equal(result$MappedSymbol, "OLD.L^K25")
  expect_false(result$IsActive)
})

# --- Tier 2: Bare RIC / PrimaryInstrument ---

test_that("Tier 2: active bare RIC A -> A.N", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas)
      mock_eikon_primary("A", "A.N", TRUE)
    else mock_eikon_active(rics, TRUE)
  })

  result <- rd_ConvertSymbol("A")
  expect_equal(result$MappedSymbol, "A.N")
  expect_equal(result$ResolutionTier, "primary_instrument")
  expect_true(result$IsActive)
})

test_that("Tier 2: delisted bare RIC HES -> HES.N^G25", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas)
      mock_eikon_primary("HES", "HES.N^G25", FALSE)
    else mock_eikon_active(rics, FALSE)
  })

  result <- rd_ConvertSymbol("HES")
  expect_equal(result$MappedSymbol, "HES.N^G25")
  expect_equal(result$ResolutionTier, "primary_instrument")
  expect_false(result$IsActive)
})

test_that("Tier 2: skipped when handle_bare_rics = FALSE", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(...) stop("should not be called"))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) stop("should not be called"))

  result <- rd_ConvertSymbol("HES",
    handle_bare_rics = FALSE, canonical_history_fallback = FALSE)
  expect_true(is.na(result$MappedSymbol))
  expect_equal(result$ResolutionTier, "none")
})

test_that("Tier 2: skipped for dotted RICs", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  eikon_calls <- list()
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    eikon_calls[[length(eikon_calls) + 1L]] <<- Eikonformulas
    mock_eikon_active(rics, FALSE)
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    mock_history("OLD.DE", "OLD.DE^L25"))

  rd_ConvertSymbol("OLD.DE")
  pi_calls <- Filter(function(f) "TR.PrimaryInstrument" %in% f, eikon_calls)
  expect_equal(length(pi_calls), 0L)
})

test_that("Tier 2: PrimaryInstrument returns NA falls through to Tier 3", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas)
      mock_eikon_primary("BARE", NA_character_, NA)
    else mock_eikon_active(rics, FALSE)
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    mock_history("BARE", c("BARE.N", "BARE.N^X25")))

  result <- rd_ConvertSymbol("BARE")
  expect_equal(result$ResolutionTier, "history_canonical")
  expect_equal(result$MappedSymbol, "BARE.N^X25")
})

# --- Tier 3: Canonical History Fallback ---

test_that("Tier 3: case-insensitive 1cOv.De -> 1COv.DE^L25", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    mock_history("1cOv.De", c("1COv.DE", "1COv.DE^L25")))

  result <- rd_ConvertSymbol("1cOv.De")
  expect_equal(result$MappedSymbol, "1COv.DE^L25")
  expect_equal(result$ResolutionTier, "history_canonical")
  expect_false(result$IsActive)
})

test_that("Tier 3: HES.N full chain picks last entry", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    mock_history("HES.N", c("AHC.N", "HES.N", "HES.N^G25")))

  result <- rd_ConvertSymbol("HES.N")
  expect_equal(result$MappedSymbol, "HES.N^G25")
})

test_that("Tier 3: same canonical as input -> unresolved", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    mock_history("SAME.L", c("SAME.L")))

  result <- rd_ConvertSymbol("SAME.L")
  expect_true(is.na(result$MappedSymbol))
  expect_equal(result$ResolutionTier, "none")
})

test_that("Tier 3: skipped when canonical_history_fallback = FALSE", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas)
      mock_eikon_primary("HES", NA_character_, NA)
    else mock_eikon_active(rics, FALSE)
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    stop("should not be called"))

  result <- rd_ConvertSymbol("HES", canonical_history_fallback = FALSE)
  expect_true(is.na(result$MappedSymbol))
})

test_that("Tier 3: all-NA history -> unresolved", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) {
    df <- data.frame(Date = "2020-01-01", stringsAsFactors = FALSE)
    df[["X"]] <- NA_character_
    df
  })

  result <- rd_ConvertSymbol("X")
  expect_true(is.na(result$MappedSymbol))
})

test_that("Tier 3: empty history dataframe -> unresolved", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    data.frame(Date = character(0), stringsAsFactors = FALSE))

  result <- rd_ConvertSymbol("EMPTY.L")
  expect_true(is.na(result$MappedSymbol))
})

test_that("Tier 3: rd_GetHistory API error -> unresolved, not crash", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) stop("HTTP 503"))

  result <- rd_ConvertSymbol("DEAD.DE")
  expect_true(is.na(result$MappedSymbol))
  expect_equal(result$ResolutionTier, "none")
})

# --- Batch ---

test_that("Batch: mixed scenarios resolved correctly", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    if (length(symbol) == 3 && "AAPL.O" %in% symbol) {
      dt <- data.table(Input = symbol, bestMatch = NA_character_, RICs = symbol)
      dt$bestMatch[symbol == "AAPL.O"] <- "AAPL.O"
      dt
    } else stop("not found")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas)
      mock_eikon_primary(rics, "HES.N^G25", FALSE)
    else mock_eikon_active(rics, grepl("AAPL", rics))
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...) {
    if (universe == "1cOv.De") mock_history(universe, c("1COv.DE^L25"))
    else mock_history(universe, universe)
  })

  result <- rd_ConvertSymbol(c("AAPL.O", "HES", "1cOv.De"))
  expect_equal(nrow(result), 3L)
  expect_equal(result$ResolutionTier,
    c("symbology", "primary_instrument", "history_canonical"))
})

# --- Edge Cases ---

test_that("NULL symbols raises error", {
  expect_error(rd_ConvertSymbol(NULL), "cannot be NULL")
})

test_that("Empty string symbol -> unresolved", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    data.frame(Date = character(0), stringsAsFactors = FALSE))

  result <- rd_ConvertSymbol("")
  expect_true(is.na(result$MappedSymbol))
})

test_that("Output has correct columns", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("VOD.L"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol("VOD.L")
  expect_true(all(c("OriginalSymbol", "MappedSymbol",
                     "ResolutionTier", "IsActive") %in% names(result)))
})

test_that("Original input order is preserved in batch", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(RICs = symbol, bestMatch = symbol, RIC = symbol)
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  syms <- c("ZZZ.L", "AAA.O", "MMM.DE")
  result <- rd_ConvertSymbol(syms)
  expect_equal(result$OriginalSymbol, syms)
})

# --- Tier 4: Identity check ---

test_that("Tier 4: active RIC maps to itself when symbology returns no best match", {
  # Symbology returns "No best match available" for a valid RIC-to-RIC no-op
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = "No best match available", RICs = symbol)
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  result <- rd_ConvertSymbol("AAPL.O")
  expect_equal(result$MappedSymbol, "AAPL.O")
  expect_equal(result$ResolutionTier, "identity")
  expect_true(result$IsActive)
})

test_that("Tier 4: inactive RIC stays unresolved when symbology returns no match", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = "No best match available", RICs = symbol)
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...)
    mock_history("DEAD.RIC", c("DEAD.RIC")))

  result <- rd_ConvertSymbol("DEAD.RIC")
  expect_true(is.na(result$MappedSymbol))
  expect_equal(result$ResolutionTier, "none")
})

test_that("Tier 4: identity skipped for non-RIC target types", {
  # ISIN-to-ISIN should NOT trigger identity check
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = "No best match available")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol("US0378331005",
    from_symbol_type = "ISIN", to_symbol_type = "ISIN")
  expect_true(is.na(result$MappedSymbol))
  expect_equal(result$ResolutionTier, "none")
})

test_that("Tier 4: identity works in batch with mixed results", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    # AAPL.O returns no best match, HES returns no best match
    data.table(
      Input = symbol,
      bestMatch = rep("No best match available", length(symbol)),
      RICs = symbol
    )
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas) {
      mock_eikon_primary("HES", "HES.N^G25", FALSE)
    } else {
      # AAPL.O is active, HES.N^G25 is not
      active_flags <- ifelse(grepl("AAPL", rics), TRUE, FALSE)
      list(PostProcessedEikonGetData = data.frame(
        Instrument = rics,
        Instrument.Is.Active.Flag = active_flags,
        stringsAsFactors = FALSE
      ))
    }
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  result <- rd_ConvertSymbol(c("AAPL.O", "HES"))
  expect_equal(result$MappedSymbol, c("AAPL.O", "HES.N^G25"))
  expect_equal(result$ResolutionTier, c("identity", "primary_instrument"))
  expect_equal(result$IsActive, c(TRUE, FALSE))
})

# --- Parity with EikonGetSymbology tests ---
# These verify that rd_ConvertSymbol handles the same scenarios that
# EikonGetSymbology tests cover (chunking, ISIN, mixed results).

test_that("ISIN to RIC: symbology succeeds and returns correct mapping", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = "AAPL.O", RICs = "AAPL.O")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol("US0378331005",
    from_symbol_type = "ISIN", to_symbol_type = "RIC")
  expect_equal(result$OriginalSymbol, "US0378331005")
  expect_equal(result$MappedSymbol, "AAPL.O")
  expect_equal(result$ResolutionTier, "symbology")
  expect_true(result$IsActive)
})

test_that("Mixed valid/invalid symbols: valid ones resolve, invalid stay NA", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    bm <- ifelse(symbol == "AAPL.O", "AAPL.O", "No best match available")
    data.table(Input = symbol, bestMatch = bm, RICs = symbol)
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...) {
    active <- ifelse(grepl("AAPL", rics), TRUE, FALSE)
    list(PostProcessedEikonGetData = data.frame(
      Instrument = rics, Instrument.Is.Active.Flag = active,
      stringsAsFactors = FALSE
    ))
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...) {
    df <- data.frame(Date = "2020-01-01", stringsAsFactors = FALSE)
    df[[universe]] <- universe  # maps to itself -> unresolved
    df
  })

  result <- rd_ConvertSymbol(c("AAPL.O", "NONEXISTENT"))
  expect_equal(nrow(result), 2L)
  expect_equal(result$MappedSymbol[1], "AAPL.O")
  expect_true(is.na(result$MappedSymbol[2]))
})

test_that("Symbology API total failure does not crash rd_ConvertSymbol", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("HTTP 500"))
  stub(rd_ConvertSymbol, "EikonGetData", function(...) stop("HTTP 500"))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) stop("HTTP 503"))

  result <- rd_ConvertSymbol(c("FAIL1.O", "FAIL2.O"))
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2L)
  expect_true(all(is.na(result$MappedSymbol)))
  expect_true(all(result$ResolutionTier == "none"))
})

test_that("No deprecation warning emitted from internal EikonGetSymbology call", {
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("VOD.L"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  expect_no_warning(rd_ConvertSymbol("VOD.L"))
})

