# zzz.R

# Hidden package environment
.pkgglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Default options — resolution order: existing option > env var > default.
  # Setting REFINITIV_PORT / REFINITIV_BASE_URL in .Renviron skips
  # terminal auto-detection entirely (CheckTerminalType short-circuits
  # when eikon_port is non-NULL).
  if (is.null(getOption("refinitiv_base_url"))) {
    env_url <- Sys.getenv("REFINITIV_BASE_URL", "http://localhost")
    options(refinitiv_base_url = env_url)
  }

  if (is.null(getOption("eikon_port"))) {
    env_port <- Sys.getenv("REFINITIV_PORT", "")
    if (nzchar(env_port)) {
      options(eikon_port = as.integer(env_port))
    }
  }

  if (is.null(getOption("eikon_api"))) {
    options(eikon_api = "/api/udf/")
  }

  if (is.null(getOption("rdp_api"))) {
    options(rdp_api = "/api/rdp/")
  }

  if (is.null(getOption("streaming_port"))) {
    eikon_port <- getOption("eikon_port")
    options(streaming_port = if (!is.null(eikon_port)) eikon_port else 9000L)
  }

  if (is.null(getOption("HistoricalPricingFields"))) {
    options(HistoricalPricingFields = c(
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

  # === C3 Secure Credential Vault — backward-compatibility migration ===
  # Resolution order: existing option (.EikonApiKey) > env var > nothing.
  # If the user has .EikonApiKey in .Rprofile / options(), migrate it into
  # the vault so internal code finds it there.  The write-through in
  # refinitiv_vault_set() keeps the option in sync for external callers.
  legacy_key <- getOption(".EikonApiKey")
  if (!is.null(legacy_key) && !refinitiv_vault_has("api_key")) {
    refinitiv_vault_set("api_key", legacy_key)
  }
  # Also check REFINITIV_APP_KEY env var (e.g., in .Renviron)
  if (!refinitiv_vault_has("api_key")) {
    env_key <- Sys.getenv("REFINITIV_APP_KEY", "")
    if (nzchar(env_key)) {
      refinitiv_vault_set("api_key", env_key)
    }
  }

  # Parse REFINITIV_PROGRESS env var: TRUE / FALSE / VERBOSE
  if (is.null(getOption("refinitiv_progress"))) {
    env_progress <- toupper(Sys.getenv("REFINITIV_PROGRESS", ""))
    if (nzchar(env_progress)) {
      options(refinitiv_progress = switch(env_progress,
        "TRUE"    = TRUE,
        "FALSE"   = FALSE,
        "VERBOSE" = "verbose",
        TRUE  # fallback
      ))
    }
    # When unset, getOption("refinitiv_progress") remains NULL and
    # progress_msg() defaults to TRUE
  }

  # Cache package version for fast cache key generation (avoids repeated
  # DESCRIPTION file reads — see cache.R)
  pkg <- if (is.null(pkgname)) "Refinitiv" else pkgname
  .pkgglobalenv$pkg_version <- tryCatch(
    as.character(utils::packageVersion(pkg)),
    error = function(e) NULL
  )

  # Clear any stale token options from previous sessions / older package
  # versions.  Tokens now live exclusively in the vault.
  options(
    refinitiv_access_token = NULL,
    refinitiv_token_expiration = NULL,
    refinitiv_token_type = NULL
  )
}
