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


# ===================================================================
# Coverage Gap Tests — get_active_and_date helper
# ===================================================================

test_that("get_active_and_date: retire date POSIXt path", {
  # Covers L165-169: retire column present with POSIXt value
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("DEL.L^K25"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    list(PostProcessedEikonGetData = data.frame(
      Instrument = rics,
      Instrument.Is.Active.Flag = FALSE,
      Retire.Date = as.POSIXct("2025-11-15 00:00:00", tz = "UTC"),
      stringsAsFactors = FALSE
    ))
  })

  result <- rd_ConvertSymbol("DEL.L")
  expect_equal(result$MappedSymbol, "DEL.L^K25")
  expect_false(result$IsActive)
  expect_equal(result$DelistingDate, "2025-11-15")
})

test_that("get_active_and_date: retire date character/string path", {
  # Covers L170-171: retire column with plain character string
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("OLD.O^F25"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    list(PostProcessedEikonGetData = data.frame(
      Instrument = rics,
      Instrument.Is.Active.Flag = FALSE,
      Retire.Date = "2025-06-30T00:00:00Z",
      stringsAsFactors = FALSE
    ))
  })

  result <- rd_ConvertSymbol("OLD.O")
  expect_equal(result$DelistingDate, "2025-06-30")
})

test_that("get_active_and_date: retire date Date class path", {
  # Covers L168-169: retire column with Date value (not POSIXt)
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("DEL2.L^J25"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    list(PostProcessedEikonGetData = data.frame(
      Instrument = rics,
      Instrument.Is.Active.Flag = FALSE,
      stringsAsFactors = FALSE
    ))
  })
  # Inject a Date column manually since data.frame doesn't preserve Date well
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    df <- data.frame(
      Instrument = rics,
      Instrument.Is.Active.Flag = FALSE,
      stringsAsFactors = FALSE
    )
    df$Retire.Date <- as.Date("2025-10-01")
    list(PostProcessedEikonGetData = df)
  })

  result <- rd_ConvertSymbol("DEL2.L")
  expect_equal(result$DelistingDate, "2025-10-01")
})

test_that("get_active_and_date: non-dataframe return from EikonGetData", {
  # Covers L153: early return when df is not a data.frame
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("TEST.O"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.InstrumentIsActive" %in% Eikonformulas) {
      # Return something that's NOT a data.frame and NOT wrapped in list
      "unexpected string result"
    } else {
      mock_eikon_active(rics, TRUE)
    }
  })

  result <- rd_ConvertSymbol("TEST.O")
  # Should still resolve from symbology, but IsActive stays NA
  expect_equal(result$MappedSymbol, "TEST.O")
  expect_true(is.na(result$IsActive))
})

test_that("get_active_and_date: empty rics vector returns empty table", {
  # Covers L145: early return for length(rics)==0
  # This happens when all symbols resolve in tiers with IsActive already set
  # and mapped_idx at the bottom is empty. Test via identity tier which sets IsActive.
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = "No best match available", RICs = symbol)
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...) {
    mock_eikon_active(rics, TRUE)
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  # AAPL.O -> identity (sets IsActive=TRUE), so mapped_idx will be empty
  result <- rd_ConvertSymbol("AAPL.O")
  expect_equal(result$ResolutionTier, "identity")
  expect_true(result$IsActive)
})

test_that("get_active_and_date: error handler with verbose message", {
  # Covers L177: error branch in get_active_and_date
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("ERR.L"))
  call_count <- 0L
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    call_count <<- call_count + 1L
    if ("TR.InstrumentIsActive" %in% Eikonformulas && !"TR.PrimaryInstrument" %in% Eikonformulas) {
      stop("API connection lost")
    }
    mock_eikon_active(rics, TRUE)
  })

  result <- expect_message(
    rd_ConvertSymbol("ERR.L", verbose = TRUE),
    "get_active_and_date error"
  )
  # Should still return a valid table, just with NA for IsActive
  expect_s3_class(result, "data.table")
  expect_equal(result$MappedSymbol, "ERR.L")
  expect_true(is.na(result$IsActive))
})

# ===================================================================
# Coverage Gap Tests — Tier 1: bestMatch=FALSE path
# ===================================================================

test_that("Tier 1: bestMatch=FALSE resolves via RICs column", {
  # Covers L208-222: the !bestMatch && "RICs" branch
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, RICs = "AAPL.O, AAPL.OQ")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol("US0378331005",
                             from_symbol_type = "ISIN",
                             to_symbol_type = "RIC",
                             bestMatch = FALSE)
  expect_equal(result$MappedSymbol, "AAPL.O, AAPL.OQ")
  expect_equal(result$ResolutionTier, "symbology")
})

test_that("Tier 1: bestMatch=FALSE uses last column for non-RIC target", {
  # Covers L217: target_col picks last column when to_symbol_type != "RIC"
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, RICs = "AAPL.O", ISINs = "US0378331005")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))

  result <- rd_ConvertSymbol("AAPL.O",
                             from_symbol_type = "RIC",
                             to_symbol_type = "ISIN",
                             bestMatch = FALSE)
  expect_equal(result$MappedSymbol, "US0378331005")
  expect_equal(result$ResolutionTier, "symbology")
})

test_that("Tier 1: bestMatch=FALSE skips NA/empty values", {
  # Covers L220: guard for NA and empty-string in RICs column
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, RICs = NA_character_)
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  result <- rd_ConvertSymbol("JUNK.L",
                             from_symbol_type = "RIC",
                             to_symbol_type = "RIC",
                             bestMatch = FALSE)
  expect_true(is.na(result$MappedSymbol))
})

# ===================================================================
# Coverage Gap Tests — Tier 1: Delisted alt-RIC matching
# ===================================================================

test_that("Tier 1: comma-separated alt RICs finds delisted match", {
  # Covers L242, L246-248: comma-separated RICs string with delisted pattern
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = NA_character_,
               RICs = "OLD.L, OLD.L^K25, OLD.L^M24")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))

  result <- rd_ConvertSymbol("OLD.L")
  expect_equal(result$MappedSymbol, "OLD.L^K25")
  expect_equal(result$ResolutionTier, "symbology")
})

test_that("Tier 1: alt RICs no delisted pattern falls through", {
  # Covers L243 match_alt == 0 path: no caret pattern found
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = NA_character_,
               RICs = "UNRELATED.O, DIFFERENT.L")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  result <- rd_ConvertSymbol("MYSTERY.L",
                             canonical_history_fallback = FALSE)
  # No match from delisted pattern, stays unresolved
  expect_true(is.na(result$MappedSymbol))
})

# ===================================================================
# Coverage Gap Tests — Verbose error branches
# ===================================================================

test_that("Tier 1: verbose symbology error message is emitted", {
  # Covers L257: verbose message in symbology error handler
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("HTTP 502"))
  stub(rd_ConvertSymbol, "EikonGetData", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) stop("fail"))

  expect_message(
    rd_ConvertSymbol("FAIL.O", verbose = TRUE,
                     handle_bare_rics = FALSE,
                     canonical_history_fallback = FALSE),
    "Symbology API error"
  )
})

test_that("Tier 2: verbose bare RIC error message is emitted", {
  # Covers L294: verbose message in bare RIC error handler
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas) stop("Bare RIC timeout")
    mock_eikon_active(rics, FALSE)
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) stop("fail"))

  expect_message(
    rd_ConvertSymbol("BARE", verbose = TRUE,
                     canonical_history_fallback = FALSE),
    "Bare RIC Resolution error"
  )
})

test_that("Tier 3: verbose history error message is emitted", {
  # Covers L347: verbose message in history error handler
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    if ("TR.PrimaryInstrument" %in% Eikonformulas)
      mock_eikon_primary(rics, NA_character_, NA)
    else mock_eikon_active(rics, FALSE)
  })
  stub(rd_ConvertSymbol, "rd_GetHistory", function(...) stop("History timeout"))

  expect_message(
    rd_ConvertSymbol("DEAD.DE", verbose = TRUE),
    "History Fallback error"
  )
})

# ===================================================================
# Coverage Gap Tests — Tier 3: TR.RIC column and POSIXt dates
# ===================================================================

test_that("Tier 3: finds canonical via TR.RIC column name", {
  # Covers L318: the else-if "TR.RIC" branch
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...) {
    data.frame(
      Date = c("2020-01-01", "2025-06-01"),
      TR.RIC = c("CASE.DE", "CASE.DE^L25"),
      stringsAsFactors = FALSE
    )
  })

  result <- rd_ConvertSymbol("CaSe.De")
  expect_equal(result$MappedSymbol, "CASE.DE^L25")
  expect_equal(result$ResolutionTier, "history_canonical")
})

test_that("Tier 3: history Date column with POSIXt extracts delisting date", {
  # Covers L335-336: POSIXt date in history fallback
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...) {
    dates <- as.POSIXct(c("2020-01-01", "2025-12-08"), tz = "UTC")
    data.frame(Date = dates,
               RIC = c("1COv.DE", "1COv.DE^L25"),
               stringsAsFactors = FALSE)
  })

  result <- rd_ConvertSymbol("1cOv.De")
  expect_equal(result$MappedSymbol, "1COv.DE^L25")
  expect_equal(result$DelistingDate, "2025-12-08")
})

test_that("Tier 3: RIC column found by sym name variant", {
  # Covers L317 FALSE -> L318 FALSE -> L319 TRUE: sym %in% names(hist_df)
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(...) stop("fail"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, FALSE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...) {
    # Column named exactly as input symbol (neither "RIC" nor "TR.RIC")
    df <- data.frame(Date = c("2020-01-01", "2025-12-08"),
                     stringsAsFactors = FALSE)
    df[[universe]] <- c("WEIRD.L", "WEIRD.L^X25")
    df
  })

  result <- rd_ConvertSymbol("WEIRD.L")
  expect_equal(result$MappedSymbol, "WEIRD.L^X25")
  expect_equal(result$ResolutionTier, "history_canonical")
})

# ===================================================================
# Coverage Gap Tests — Final DelistingDate propagation
# ===================================================================

test_that("Final: DelistingDate propagated from get_active_and_date for symbology-resolved symbol", {
  # Covers L391: DelistingDate assignment at bottom when tier already set it
  # but also for when it was NA and needs filling from get_active_and_date
  stub(rd_ConvertSymbol, "EikonGetSymbology", mock_symbology_success("DEL.L^K25"))
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, Eikonformulas, ...) {
    list(PostProcessedEikonGetData = data.frame(
      Instrument = rics,
      Instrument.Is.Active.Flag = FALSE,
      Retire.Date = "2025-11-15",
      stringsAsFactors = FALSE
    ))
  })

  result <- rd_ConvertSymbol("DEL.L")
  expect_equal(result$MappedSymbol, "DEL.L^K25")
  expect_false(result$IsActive)
  expect_equal(result$DelistingDate, "2025-11-15")
})

test_that("NA symbols in batch are skipped correctly", {
  # Covers valid_idx filtering for NA symbols
  stub(rd_ConvertSymbol, "EikonGetSymbology", function(symbol, ...) {
    data.table(Input = symbol, bestMatch = "VOD.L", RICs = "VOD.L")
  })
  stub(rd_ConvertSymbol, "EikonGetData", function(rics, ...)
    mock_eikon_active(rics, TRUE))
  stub(rd_ConvertSymbol, "rd_GetHistory", function(universe, ...)
    mock_history(universe, universe))

  result <- rd_ConvertSymbol(c("VOD.L", NA_character_, ""))
  expect_equal(nrow(result), 3L)
  expect_equal(result$MappedSymbol[1], "VOD.L")
  expect_true(is.na(result$MappedSymbol[2]))
  expect_true(is.na(result$MappedSymbol[3]))
  expect_equal(result$ResolutionTier[2], "none")
  expect_equal(result$ResolutionTier[3], "none")
})

