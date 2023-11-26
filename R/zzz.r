# Hidden package environment
.pkgglobalenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  #reticulate::configure_environment(pkgname)
  options(reticulate.long_as_bit64=TRUE)
  options(reticulate.ulong_as_bit64=TRUE)
  options(refinitiv_base_url = 'http://localhost')
  options(eikon_port = 9060L)
  options(eikon_api = '/api/udf/') #old --> '/api/v1/data'
  options(rdp_api = '/api/rdp/')
  options(rdp_port=9000L)
  options(HistoricalPricingFields = c("HIGH_1", "LOW_1", "OPEN_PRC", "TRDPRC_1",
                                     "NUM_MOVES", "ACVOL_UNS", "HIGH_YLD", "LOW_YLD", "OPEN_YLD",
                                     "YIELD", "BID_HIGH_1", "BID_LOW_1", "OPEN_BID", "BID", "BID_NUMMOV",
                                     "ASK_HIGH_1", "ASK_LOW_1", "OPEN_ASK", "ASK", "ASK_NUMMOV", "MID_HIGH",
                                      "MID_LOW", "MID_OPEN", "MID_PRICE","TRNOVR_UNS", "VWAP", "BLKCOUNT", "BLKVOLUM", "TRD_STATUS",
                                                                            "SALTIM", "NAVALUE"))
}
