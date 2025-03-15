library(testthat)
library(mockery)

test_that("get_rdp_streaming_url returns value from RDObject$get_rdp_streaming_url", {
  # Create a dummy RD object with a get_rdp_streaming_url method.
  dummy_RD <- list(
    get_rdp_streaming_url = function(debug) {
      return("dummy_stream_url")
    }
  )

  # Stub out external dependencies so that the handshake and send_json_request
  # do not actually perform network calls.
  stub(get_rdp_streaming_url, "rd_handshake", function(debug = FALSE, force = TRUE) NULL)
  stub(get_rdp_streaming_url, "send_json_request", function(payload, service, EndPoint, request_type, debug) NULL)

  result <- get_rdp_streaming_url(RDObject = dummy_RD, debug = TRUE)
  expect_equal(result, "dummy_stream_url")
})

test_that("get_rdp_streaming_url passes the debug parameter to RDObject$get_rdp_streaming_url", {
  # Create a variable to capture the debug flag passed to get_rdp_streaming_url.
  captured_debug <- NA
  dummy_RD <- list(
    get_rdp_streaming_url = function(debug) {
      captured_debug <<- debug
      return("dummy_stream_url")
    }
  )

  stub(get_rdp_streaming_url, "rd_handshake", function(...) NULL)
  stub(get_rdp_streaming_url, "send_json_request", function(...) NULL)

  result <- get_rdp_streaming_url(RDObject = dummy_RD, debug = FALSE)
  expect_equal(captured_debug, FALSE)
  expect_equal(result, "dummy_stream_url")
})

test_that("get_rdp_streaming_url calls handshake and send_json_request", {
  handshake_called <- FALSE
  send_called <- FALSE

  dummy_RD <- list(
    get_rdp_streaming_url = function(debug) {
      return("dummy_stream_url")
    }
  )

  # Stub rd_handshake so that we record its call.
  stub(get_rdp_streaming_url, "rd_handshake", function(debug = FALSE, force = TRUE) {
    handshake_called <<- TRUE
  })

  # Stub send_json_request to record its call.
  stub(get_rdp_streaming_url, "send_json_request", function(payload, service, EndPoint, request_type, debug) {
    send_called <<- TRUE
    return(NULL)
  })

  result <- get_rdp_streaming_url(RDObject = dummy_RD, debug = TRUE)
  expect_true(handshake_called)
  expect_true(send_called)
  expect_equal(result, "dummy_stream_url")
})
