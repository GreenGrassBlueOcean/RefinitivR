## code to prepare `OperatingMicLookup` dataset goes here

OperatingMicLookup <- data.frame( "RDN_EXCHD2" = c("AEX","ASX","BRU","CPH","GER","HEX","HKG","ISE","LIS","LSE","MCE","MIL","NAQ","NMQ","NSQ","NYQ","NZC","OSL","PAR","SES","STO","SWX","TOR","TYO","VIE","VTX","WSE")
                                , "repairedMIC" = c("XAMS","XASX","XBRU","XCSE","XETR","XHEL","XHKG","XDUB","XLIS","XLON","BMEX","XMIL","XNAS","XNAS","XNAS","XNYS","XNZE","XOSL","XPAR","XSES","XSTO","XSWX","XTSE","XJPX","XWBO","XSWX","XWAR")
                                , stringsAsFactors = FALSE
                                )

usethis::use_data(OperatingMicLookup, overwrite = TRUE)
