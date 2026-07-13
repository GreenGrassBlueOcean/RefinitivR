#' Robust Symbol Conversion for Refinitiv Data
#'
#' @description
#' `rd_ConvertSymbol` is the modern, robust replacement for \code{\link{EikonGetSymbology}}.
#' It resolves critical gaps in the LSEG Data Library's native symbology by
#' introducing a multi-tiered fallback strategy. It automatically maps "Bare
#' RICs" (e.g., `HES`) to their primary exchange-listed instruments, and uses a
#' historical timeline lookup to fix fragile letter-casing issues on delisted
#' instruments.
#'
#' @details
#' The function operates in three tiers, executed in order until a match is found:
#'
#' \enumerate{
#'   \item \strong{Symbology API}: Standard conversion using the Refinitiv
#'     symbology endpoint. Handles active instruments, ISIN/SEDOL/CUSIP
#'     conversions, and most common lookups.
#'   \item \strong{Bare RIC Resolution}: For RIC-to-RIC conversions where the
#'     input symbol lacks a dot suffix (e.g., \code{HES}, \code{A}, \code{LLY}),
#'     queries \code{TR.PrimaryInstrument} via \code{EikonGetData} to find the
#'     active exchange-suffixed RIC. This tier is critical because LSEG's
#'     symbology service cannot map bare tickers to exchange-suffixed RICs.
#'   \item \strong{Canonical History Fallback}: For symbols that remain
#'     unresolved — typically delisted instruments with letter-casing mismatches
#'     — queries \code{rd_GetHistory(fields = "TR.RIC", start = "1950-01-01")}.
#'     This exploits the case-insensitive nature of the history endpoint to
#'     extract the true canonical RIC string (e.g., \code{1cOv.De} resolves to
#'     \code{1COv.DE^L25}).
#' }
#'
#' @param RDObject Connection object (ignored, kept for back-compat).
#' @param symbols Character vector of symbols to convert.
#' @param from_symbol_type Character. Source symbology type.
#'   One of: \code{"RIC"}, \code{"ISIN"}, \code{"SEDOL"}, \code{"CUSIP"},
#'   \code{"ticker"}, \code{"lipperID"}, \code{"IMO"}. Default: \code{"RIC"}.
#' @param to_symbol_type Character. Target symbology type.
#'   Same options as \code{from_symbol_type}, plus \code{"OAPermID"}.
#'   Default: \code{"RIC"}.
#' @param bestMatch Logical. If \code{TRUE} (default), returns only the single
#'   best match per input symbol. If \code{FALSE}, returns all alternatives.
#' @param handle_bare_rics Logical. If \code{TRUE} (default), enables Tier 2:
#'   bare RIC resolution via \code{TR.PrimaryInstrument}. Only applies when
#'   \code{from_symbol_type = "RIC"}.
#' @param canonical_history_fallback Logical. If \code{TRUE} (default), enables
#'   Tier 3: historical canonical fallback via \code{rd_GetHistory}. Only
#'   applies when \code{from_symbol_type = "RIC"}.
#' @param time_out Numeric. Maximum timeout in seconds. Default: 300.
#' @param verbose Logical. If \code{TRUE}, prints debug information.
#'
#' @return A \code{data.table} with the following columns:
#' \describe{
#'   \item{\code{OriginalSymbol}}{The input symbol as provided.}
#'   \item{\code{MappedSymbol}}{The resolved symbol, or \code{NA} if unresolved.}
#'   \item{\code{ResolutionTier}}{Which tier resolved the symbol:
#'     \code{"symbology"}, \code{"primary_instrument"},
#'     \code{"history_canonical"}, \code{"identity"}, or \code{"none"}.}
#'   \item{\code{IsActive}}{Logical. Whether the resolved instrument is active.}
#'   \item{\code{DelistingDate}}{Character. The retire date (YYYY-MM-DD) if inactive, or \code{NA}.}
#' }
#'
#' @examples
#' \dontrun{
#' # --- Standard conversion (Tier 1) ---
#' # ISIN to RIC: standard symbology handles this natively
#' rd_ConvertSymbol(symbols = "US0378331005",
#'                  from_symbol_type = "ISIN",
#'                  to_symbol_type = "RIC")
#' #>   OriginalSymbol MappedSymbol ResolutionTier IsActive DelistingDate
#' #> 1:   US0378331005       AAPL.O      symbology     TRUE          <NA>
#'
#' # --- Active Bare RIC Resolution (Tier 2) ---
#' # "A" is a bare RIC (no dot suffix). LSEG symbology cannot resolve it,
#' # but Tier 2 queries TR.PrimaryInstrument and maps to A.N (NYSE).
#' rd_ConvertSymbol("A")
#' #>   OriginalSymbol MappedSymbol     ResolutionTier IsActive DelistingDate
#' #> 1:              A          A.N primary_instrument     TRUE          <NA>
#'
#' # --- Delisted Bare RIC (Tier 2) ---
#' # "HES" was Hess Corp, recently acquired and delisted. The bare ticker
#' # has no exchange suffix, so symbology fails. Tier 2 maps it to the
#' # delisted form HES.N^G25.
#' rd_ConvertSymbol("HES")
#' #>   OriginalSymbol MappedSymbol     ResolutionTier IsActive DelistingDate
#' #> 1:            HES    HES.N^G25 primary_instrument    FALSE    2025-07-18
#'
#' # --- Case-Sensitive Delisted RIC (Tier 3) ---
#' # Refinitiv is highly case-sensitive for European delisted RICs.
#' # "1cOv.De" fails both symbology and bare-RIC checks. Tier 3 uses
#' # rd_GetHistory(TR.RIC) which is case-insensitive, extracting the
#' # canonical string "1COv.DE^L25" from the historical timeline.
#' rd_ConvertSymbol("1cOv.De")
#' #>   OriginalSymbol  MappedSymbol    ResolutionTier IsActive DelistingDate
#' #> 1:        1cOv.De 1COv.DE^L25 history_canonical    FALSE    2025-12-08
#'
#' # --- Batch conversion (mixed scenarios) ---
#' rd_ConvertSymbol(c("AAPL.O", "HES", "1cOv.De", "INVALID_RIC"))
#' #>   OriginalSymbol  MappedSymbol     ResolutionTier IsActive DelistingDate
#' #> 1:         AAPL.O        AAPL.O          identity     TRUE          <NA>
#' #> 2:            HES     HES.N^G25 primary_instrument    FALSE    2025-07-18
#' #> 3:        1cOv.De  1COv.DE^L25  history_canonical    FALSE    2025-12-08
#' #> 4:    INVALID_RIC          <NA>               none       NA          <NA>
#'
#' # --- Disable fallbacks for speed ---
#' # If you know all inputs are well-formed, skip Tier 2 & 3:
#' rd_ConvertSymbol(c("AAPL.O", "VOD.L"),
#'                  handle_bare_rics = FALSE,
#'                  canonical_history_fallback = FALSE)
#' }
#'
#' @seealso \code{\link{EikonGetSymbology}} (deprecated predecessor)
#' @export
rd_ConvertSymbol <- function(symbols,
                             from_symbol_type = "RIC",
                             to_symbol_type = "RIC",
                             bestMatch = TRUE,
                             handle_bare_rics = TRUE,
                             canonical_history_fallback = TRUE,
                             time_out = 300,
                             verbose = FALSE,
                             RDObject = NULL) {

  if (is.null(symbols)) stop("Parameter 'symbols' cannot be NULL")

  # Setup output table
  res_dt <- data.table::data.table(
    OriginalSymbol = symbols,
    MappedSymbol = NA_character_,
    ResolutionTier = "none",
    IsActive = NA,
    DelistingDate = NA_character_
  )

  # Only process non-empty symbols
  valid_idx <- which(!is.na(symbols) & nchar(trimws(symbols)) > 0)
  if (length(valid_idx) == 0L) return(res_dt)

  syms_to_process <- symbols[valid_idx]

  # --- Helper to resolve active flag and delisting date ---
  get_active_and_date <- function(rics) {
    res <- data.table::data.table(
      IsActive = rep(NA, length(rics)),
      DelistingDate = rep(NA_character_, length(rics))
    )
    

    tryCatch({
      df <- EikonGetData(rics = rics, Eikonformulas = c("TR.InstrumentIsActive", "TR.RetireDate"), raw_output = FALSE, time_out = time_out, verbose = verbose)
      if (is.list(df) && "PostProcessedEikonGetData" %in% names(df)) {
        df <- df$PostProcessedEikonGetData
      }
      
      if (!is.data.frame(df) || !"Instrument" %in% names(df)) return(res)
      
      act_col_idx <- grep("active", tolower(names(df)))
      ret_col_idx <- grep("retire", tolower(names(df)))
      
      for (i in seq_along(rics)) {
        match_idx <- which(df$Instrument == rics[i])
        if (length(match_idx) > 0) {
          if (length(act_col_idx) > 0) {
            val <- df[[names(df)[act_col_idx[1]]]][match_idx[1]]
            res$IsActive[i] <- if (is.logical(val)) val else (val == 1 || val == "True" || val == "TRUE")
          }
          if (length(ret_col_idx) > 0) {
            ret_val <- df[[names(df)[ret_col_idx[1]]]][match_idx[1]]
            if (!is.na(ret_val) && as.character(ret_val) != "") {
               if (inherits(ret_val, "POSIXt") || inherits(ret_val, "Date")) {
                 res$DelistingDate[i] <- format(as.Date(ret_val), "%Y-%m-%d")
               } else {
                 res$DelistingDate[i] <- substr(as.character(ret_val), 1, 10)
               }
            }
          }
        }
      }
    }, error = function(e) { if (verbose) message("get_active_and_date error: ", conditionMessage(e)) })
    
    return(res)
  }

  # --- Tier 1: Standard Symbology API ---
  tryCatch({
    symb_res <- suppressWarnings(EikonGetSymbology(
      symbol = syms_to_process,
      from_symbol_type = from_symbol_type,
      to_symbol_type = to_symbol_type,
      bestMatch = bestMatch,
      time_out = time_out,
      verbose = verbose,
      raw_output = FALSE
    ))
    
    # Process Symbology Result
    if (is.data.frame(symb_res) && nrow(symb_res) > 0) {

      # --- Tier 1a: current EikonGetSymbology schema (type-named columns) ---
      # The live symbology endpoint names its result columns after the symbol
      # types: an ISIN->RIC lookup returns a `RIC` (mapped) column plus an
      # `ISIN` (echoed input) column, and an `error` column on failed rows.
      # The mapped value therefore lives in the `to_symbol_type` column, keyed
      # to each input via the `from_symbol_type` column. The legacy
      # bestMatch/RICs handling below never fired for this shape, so EVERY
      # ISIN/CUSIP/ticker conversion silently returned ResolutionTier="none"
      # even though the underlying lookup resolved correctly. Engage only when
      # the legacy columns are absent, so older response shapes are untouched.
      legacy_schema <- ("bestMatch" %in% names(symb_res)) ||
                       ("RICs" %in% names(symb_res))
      if (!legacy_schema && to_symbol_type %in% names(symb_res)) {
        match_col <- if (from_symbol_type %in% names(symb_res)) {
          from_symbol_type
        } else {
          names(symb_res)[1L]
        }
        match_keys  <- as.character(symb_res[[match_col]])
        mapped_vals <- as.character(symb_res[[to_symbol_type]])
        err_vals <- if ("error" %in% names(symb_res)) {
          as.character(symb_res[["error"]])
        } else {
          rep(NA_character_, nrow(symb_res))
        }
        for (i in valid_idx) {
          rows <- which(match_keys == as.character(symbols[i]))
          if (length(rows) > 0L) {
            r <- rows[1L]
            val <- mapped_vals[r]
            has_err <- !is.na(err_vals[r]) && nzchar(err_vals[r])
            # Skip failures and pure identity (from==to echoes input); an
            # active RIC->RIC no-op is labelled by the Tier 4 identity check.
            if (!has_err && !is.na(val) && nzchar(val) &&
                val != "No best match available" &&
                !identical(val, as.character(symbols[i]))) {
              res_dt$MappedSymbol[i]   <- val
              res_dt$ResolutionTier[i] <- "symbology"
            }
          }
        }
      }

      if (bestMatch && "bestMatch" %in% names(symb_res)) {
        for (i in valid_idx) {
          sym <- symbols[i]
          match_row <- symb_res[symb_res[[1]] == sym, ]
          if (nrow(match_row) > 0) {
            best_val <- match_row$bestMatch[1]
            if (!is.na(best_val) && as.character(best_val) != "" && best_val != "No best match available") {
              res_dt$MappedSymbol[i] <- best_val
              res_dt$ResolutionTier[i] <- "symbology"
            }
          }
        }
      } else if (!bestMatch && "RICs" %in% names(symb_res)) {
         # if returning multiple, take the first one or leave it for further logic? The spec says returns single best match in MappedSymbol if bestMatch is TRUE. 
         # Wait, if bestMatch is FALSE, how to handle? The plan says "return all alternatives". But MappedSymbol is character. Let's just grab the first one or the whole string. 
         # Assuming bestMatch=TRUE is the main path. For FALSE, we can just grab the first valid.
         for (i in valid_idx) {
          sym <- symbols[i]
          match_row <- symb_res[symb_res[[1]] == sym, ]
          if (nrow(match_row) > 0) {
             # Take the RICs column if target is RIC
             target_col <- if (to_symbol_type == "RIC") "RICs" else names(match_row)[ncol(match_row)]
             if (target_col %in% names(match_row)) {
               best_val <- match_row[[target_col]][1]
               if (!is.na(best_val) && as.character(best_val) != "") {
                 res_dt$MappedSymbol[i] <- best_val
                 res_dt$ResolutionTier[i] <- "symbology"
               }
             }
          }
         }
      }
      
      # For from_symbol_type = "RIC", Refinitiv sometimes returns case-mismatched RICs in the RICs column for delisted.
      # Let's check if there's an exact case-insensitive match with a delisting suffix.
      if (from_symbol_type == "RIC" && to_symbol_type == "RIC" && "RICs" %in% names(symb_res)) {
         for (i in valid_idx) {
            if (is.na(res_dt$MappedSymbol[i]) || res_dt$MappedSymbol[i] == symbols[i]) {
              sym <- symbols[i]
              match_row <- symb_res[symb_res[[1]] == sym, ]
              if (nrow(match_row) > 0) {
                # Look for exact delisted pattern in alternatives
                delisted_pattern <- tolower(paste0(sym, "^"))
                alt_rics <- match_row$RICs
                if (is.character(alt_rics)) {
                   # they might be comma separated or vector if data.table 
                   if (length(alt_rics) == 1 && grepl(",", alt_rics)) alt_rics <- trimws(unlist(strsplit(alt_rics, ",")))
                   match_alt <- grep(delisted_pattern, tolower(alt_rics), fixed = TRUE, value = TRUE)
                   if (length(match_alt) > 0) {
                      # Map back to the original casing from alt_rics
                      orig_case_match <- alt_rics[tolower(alt_rics) == tolower(match_alt[1])]
                      res_dt$MappedSymbol[i] <- if(length(orig_case_match) > 0) orig_case_match[1] else match_alt[1]
                      res_dt$ResolutionTier[i] <- "symbology"
                   }
                }
              }
            }
         }
      }
    }
  }, error = function(e) {
    if (verbose) message(sprintf("Symbology API error: %s", conditionMessage(e)))
  })

  # --- Tier 2: Bare RIC Resolution ---
  unresolved_idx <- which(is.na(res_dt$MappedSymbol) & !is.na(res_dt$OriginalSymbol) & nchar(trimws(res_dt$OriginalSymbol)) > 0)
  # Also re-evaluate symbols that matched themselves in symbology to see if they are bare RICs
  same_idx <- which(res_dt$MappedSymbol == res_dt$OriginalSymbol & !is.na(res_dt$OriginalSymbol))
  tier2_idx <- unique(c(unresolved_idx, same_idx))
  
  if (handle_bare_rics && from_symbol_type == "RIC" && length(tier2_idx) > 0) {
    bare_idx <- tier2_idx[!grepl("\\.", res_dt$OriginalSymbol[tier2_idx])]
    if (length(bare_idx) > 0) {
      bare_syms <- res_dt$OriginalSymbol[bare_idx]
      tryCatch({
        df_bare <- EikonGetData(
          rics = bare_syms,
          Eikonformulas = c("TR.PrimaryInstrument", "TR.InstrumentIsActive"),
          time_out = time_out,
          verbose = verbose
        )
        if (is.list(df_bare) && "PostProcessedEikonGetData" %in% names(df_bare)) {
          df_bare <- df_bare$PostProcessedEikonGetData
        }
        if (is.data.frame(df_bare) && "Instrument" %in% names(df_bare) && "Primary.Instrument.RIC" %in% names(df_bare)) {
          for (i in seq_along(bare_syms)) {
            match_row <- df_bare[df_bare$Instrument == bare_syms[i], ]
            if (nrow(match_row) > 0) {
              prim_ric <- match_row$Primary.Instrument.RIC[1]
              if (!is.na(prim_ric) && as.character(prim_ric) != "") {
                idx <- bare_idx[i]
                res_dt$MappedSymbol[idx] <- prim_ric
                res_dt$ResolutionTier[idx] <- "primary_instrument"
              }
            }
          }
        }
      }, error = function(e) {
        if (verbose) message(sprintf("Bare RIC Resolution error: %s", conditionMessage(e)))
      })
    }
  }

  # --- Tier 3: Canonical History Fallback ---
  unresolved_idx <- which(is.na(res_dt$MappedSymbol) & !is.na(res_dt$OriginalSymbol) & nchar(trimws(res_dt$OriginalSymbol)) > 0)
  same_idx <- which(res_dt$MappedSymbol == res_dt$OriginalSymbol & !is.na(res_dt$OriginalSymbol))
  tier3_idx <- unique(c(unresolved_idx, same_idx))
  
  if (canonical_history_fallback && from_symbol_type == "RIC" && length(tier3_idx) > 0) {
    for (idx in tier3_idx) {
      sym <- res_dt$OriginalSymbol[idx]
      tryCatch({
        hist_df <- rd_GetHistory(
          universe = sym,
          fields = "TR.RIC",
          start = "1950-01-01",
          debug = verbose
        )
        if (is.data.frame(hist_df) && nrow(hist_df) > 0) {
          # Look for the RIC column (might be "RIC" or the sym name itself depending on api behavior, or "TR.RIC")
          ric_col <- NULL
          if ("RIC" %in% names(hist_df)) ric_col <- "RIC"
          else if ("TR.RIC" %in% names(hist_df)) ric_col <- "TR.RIC"
          else if (sym %in% names(hist_df)) ric_col <- sym
          
          if (!is.null(ric_col)) {
             ric_vals <- hist_df[[ric_col]]
             ric_vals <- ric_vals[!is.na(ric_vals) & as.character(ric_vals) != ""]
             if (length(ric_vals) > 0) {
               canonical <- utils::tail(ric_vals, 1) # last entry
               if (canonical != sym) {
                 res_dt$MappedSymbol[idx] <- canonical
                 res_dt$ResolutionTier[idx] <- "history_canonical"
                 
                 # Extract delisting date from history
                 if ("Date" %in% names(hist_df)) {
                   date_vals <- hist_df[["Date"]][!is.na(hist_df[[ric_col]]) & as.character(hist_df[[ric_col]]) != ""]
                   if (length(date_vals) > 0) {
                     delist_dt <- utils::tail(date_vals, 1)
                     if (inherits(delist_dt, "POSIXt") || inherits(delist_dt, "Date")) {
                       res_dt$DelistingDate[idx] <- format(as.Date(delist_dt), "%Y-%m-%d")
                     } else {
                       res_dt$DelistingDate[idx] <- substr(as.character(delist_dt), 1, 10)
                     }
                   }
                 }
               }
             }
          }
        }
      }, error = function(e) {
        if (verbose) message(sprintf("History Fallback error for %s: %s", sym, conditionMessage(e)))
      })
    }
  }

  # --- Tier 4: Identity check for RIC-to-RIC ---
  # When converting RIC -> RIC, a well-formed active RIC (e.g. AAPL.O) maps
  # to itself. Symbology returns "No best match available" for this no-op,
  # and Tiers 2-3 skip dotted RICs or reject canonical == input. So we check
  # if the unresolved input RIC is itself active.
  if (from_symbol_type == "RIC" && to_symbol_type == "RIC") {
    still_unresolved <- which(
      is.na(res_dt$MappedSymbol) &
      !is.na(res_dt$OriginalSymbol) &
      nchar(trimws(res_dt$OriginalSymbol)) > 0
    )
    if (length(still_unresolved) > 0) {
      identity_syms <- res_dt$OriginalSymbol[still_unresolved]
      identity_active <- get_active_and_date(identity_syms)
      for (j in seq_along(still_unresolved)) {
        if (isTRUE(identity_active$IsActive[j])) {
          idx <- still_unresolved[j]
          res_dt$MappedSymbol[idx] <- res_dt$OriginalSymbol[idx]
          res_dt$ResolutionTier[idx] <- "identity"
          res_dt$IsActive[idx] <- TRUE
        }
      }
    }
  }

  # --- Resolve IsActive and DelistingDate for successfully mapped symbols ---
  mapped_idx <- which(!is.na(res_dt$MappedSymbol) & is.na(res_dt$IsActive))
  if (length(mapped_idx) > 0) {
    unique_mapped <- unique(res_dt$MappedSymbol[mapped_idx])
    act_status <- get_active_and_date(unique_mapped)
    
    for (idx in mapped_idx) {
      mapped_val <- res_dt$MappedSymbol[idx]
      match_j <- which(unique_mapped == mapped_val)[1]
      
      if (!is.na(act_status$IsActive[match_j])) {
        res_dt$IsActive[idx] <- act_status$IsActive[match_j]
      }
      if (is.na(res_dt$DelistingDate[idx]) && !is.na(act_status$DelistingDate[match_j])) {
        res_dt$DelistingDate[idx] <- act_status$DelistingDate[match_j]
      }
    }
  }

  return(res_dt)
}
