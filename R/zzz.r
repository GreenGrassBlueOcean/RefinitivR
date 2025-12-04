# zzz.R

# Hidden package environment
.pkgglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # reticulate::configure_environment(pkgname) # Uncomment if needed

  # Set reticulate options
  if (is.null(getOption("reticulate.long_as_bit64"))) {
    options(reticulate.long_as_bit64 = TRUE)
  }

  if (is.null(getOption("reticulate.ulong_as_bit64"))) {
    options(reticulate.ulong_as_bit64 = TRUE)
  }

  # Set Refinitiv-related options with a unique prefix to avoid conflicts
  if (is.null(getOption("refinitiv_base_url"))) {
    options(refinitiv_base_url = 'http://localhost')
  }

  if (is.null(getOption("eikon_port"))) { # Renamed for clarity
    options(eikon_port  = NULL) # 9060L for Eikon, 9000L for Workstation
  }

  if (is.null(getOption("eikon_api"))) { # Renamed for clarity
    options(eikon_api = '/api/udf/') # Old: '/api/v1/data'
  }

  if (is.null(getOption("rdp_api"))) { # Renamed for clarity
    options(rdp_api = '/api/rdp/')
  }

  if (is.null(getOption("rdp_port"))) { # Renamed for clarity
    options(rdp_port = 9000L)
  }

  if (is.null(getOption("streaming_port"))) { # Port for streaming WebSocket connections
    # Default to eikon_port if available, otherwise rdp_port, otherwise 9060
    eikon_port <- getOption("eikon_port")
    if (!is.null(eikon_port)) {
      options(streaming_port = eikon_port)
    } else {
      options(streaming_port = getOption("rdp_port", 9060L))
    }
  }

  if (is.null(getOption("HistoricalPricingFields"))) {
    options(HistoricalPricingFields  = c(
      "HIGH_1", "LOW_1", "OPEN_PRC", "TRDPRC_1",
      "NUM_MOVES", "ACVOL_UNS", "HIGH_YLD", "LOW_YLD",
      "OPEN_YLD", "YIELD", "BID_HIGH_1", "BID_LOW_1",
      "OPEN_BID", "BID", "BID_NUMMOV", "ASK_HIGH_1",
      "ASK_LOW_1", "OPEN_ASK", "ASK", "ASK_NUMMOV",
      "MID_HIGH", "MID_LOW", "MID_OPEN", "MID_PRICE",
      "TRNOVR_UNS", "VWAP", "BLKCOUNT", "BLKVOLUM",
      "TRD_STATUS", "SALTIM", "NAVALUE"
    ))
  }
}

