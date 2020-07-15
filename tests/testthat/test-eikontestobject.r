#Build an EIkontestobject
# when using covr::report() or using online coverage checks like e.g. codecov.io an connection with Eikon is not available.
# However we can make a test connection that behaves exactly the same and can mimic this connection


TestEikon <- list( "DacsParams" = NA
                  , "DesktopSession" = NA
                  , "EikonError" = NA
                  , "Profile" = NA
                  , "Session" = NA
                  , "Stream" = NA
                  , "StreamConnection" = NA
                  , "StreamConnectionState" = NA
                  , "StreamState" = NA
                  , "StreamingPrice" = NA
                  , "StreamingPrices" = NA
                  , "TR_Field" = NA
                  , "__builtins__" = NA
                  , "__cached__" = NA
                  , "__doc__" = NA
                  , "__file__" = NA
                  , "__loader__" = NA
                  , "__name__" = NA
                  , "__package__" = NA
                  , "__path__" = NA
                  , "__spec__" = NA
                  , "__version__" = NA
                  , "cache" = NA
                  , "data_grid" = NA
                  , "desktop_session" = NA
                  , "eikonError" = NA
                  , "get_app_key" = NA
                  , "get_data" = NA
                  , "get_desktop_session" = NA
                  , "get_news_headlines" = NA
                  , "get_news_story" = NA
                  , "get_port_number" = NA
                  , "get_symbology" = NA
                  , "get_timeout" = NA
                  , "get_timeseries" = NA
                  , "istream_callback" = NA
                  , "itemstream" = NA
                  , "json_requests" = NA
                  , "news_request" = NA
                  , "send_json_request" = NA
                  , "session" = NA
                  , "set_app_key" = function(app_key){return(app_key)}
                  , "set_log_level" = NA
                  , "set_log_path" = NA
                  , "set_on_event_callback" = NA
                  , "set_on_state_callback" = NA
                  , "set_port_number" = NA
                  , "set_timeout" = NA
                  , "stream" = NA
                  , "stream_connection" = NA
                  , "streaming_session" = NA
                  , "streamingprice" = NA
                  , "streamingprice_callback" = NA
                  , "streamingprices" = NA
                  , "symbology" = NA
                  , "time_series" = NA
                  , "tools" = NA
                  )




# test <- EikonGetSymbology( EikonObject = TestEikon, symbol = "RDSa.L", from_symbol_type = "RIC", to_symbol_type = "ISIN"
#                           , bestMatch = TRUE, time_out = 60, verbose = FALSE, raw_output = FALSE)

