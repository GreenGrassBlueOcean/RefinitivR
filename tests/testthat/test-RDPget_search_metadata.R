# tests/testthat/test-RDPget_search_metadata.R

test_that("RDPget_search_metadata works against real environment (optional)", {

  Eikon <- check_Eikonapi()

  for (i in c("JSON")) {
    Eikon <- check_Eikonapi(ExecutionMode = i)

    if (i == "JSON") {
      searchView <- "EquityQuotes"
    } else {
      searchView <- "EQUITY_QUOTES"
    }

    test <- RDPget_search_metadata(RDP = Eikon, searchView = searchView)
    expect_error(test, NA)
    expect_equal(class(test), "data.frame")

    expect_equal(
      lapply(test, class),
      list(
        Refinitiv_index = "character",
        Type       = "character",
        Searchable = "logical",
        Sortable   = "logical",
        Navigable  = "logical",
        Groupable  = "logical",
        Exact      = "logical",
        Symbol     = "logical"
      )
    )

    test2 <- RDPget_search_metadata(RDP = Eikon)
    expect_error(test2, NA)
    expect_equal(class(test2), "data.frame")

    expect_equal(
      lapply(test2, class),
      list(
        Refinitiv_index = "character",
        Type       = "character",
        Searchable = "logical",
        Sortable   = "logical",
        Navigable  = "logical",
        Groupable  = "logical",
        Exact      = "logical",
        Symbol     = "logical"
      )
    )
  }
})


library(testthat)
library(mockery)
library(data.table)
library(jsonlite)

# We create dummy metadata for testing; everything is in memory (no real calls).
dummy_metadata <- list(
  Type       = data.frame(Type = "Equity", stringsAsFactors = FALSE),
  Searchable = data.frame(Searchable = TRUE),
  Sortable   = data.frame(Sortable = FALSE),
  Navigable  = data.frame(Navigable = TRUE),
  Groupable  = data.frame(Groupable = FALSE),
  Exact      = data.frame(Exact = TRUE),
  Symbol     = data.frame(Symbol = FALSE)
)

test_that("RDPget_search_metadata can handle 'refinitiv.data' connections via mocking", {
  # 1) Pretend we have an RD (Python) session by stubbing.
  #    So the function sees ConnectionMetaData$name == "refinitiv.data".
  stub(RDPget_search_metadata, "PropertiesActiveRefinitivObject", function(verbose) {
    list(name = "refinitiv.data")
  })

  # 2) Stub available search views for "RD" so only these two appear valid:
  stub(RDPget_search_metadata, "RDPShowAvailableSearchViews", function(Platform) {
    if (Platform == "RD") c("SEARCH_ALL", "EquityQuotes") else character(0)
  })

  # 3) Stub the function that maps “EquityQuotes” → “DUMMY_VIEW”.
  stub(RDPget_search_metadata, "GetSearchView", function(ConnectionObject, SearchView) {
    "DUMMY_VIEW"
  })

  # 4) Create a dummy definition object whose get_data returns dummy raw JSON.
  dummy_definition <- list(
    get_data = function() {
      list(data = list(
        raw = charToRaw(toJSON(list(Properties = dummy_metadata), auto_unbox = TRUE))
      ))
    }
  )

  # 5) Build a dummy “refinitiv.data” object that returns the above definition.
  dummy_conn_rd <- list(
    content = list(
      search = list(
        metadata = list(
          Definition = function(view) {
            dummy_definition
          }
        )
      )
    )
  )

  # 6) Stub py_to_r so it decodes our JSON from raw → R list.
  stub(RDPget_search_metadata, "reticulate::py_to_r", function(x) {
    fromJSON(rawToChar(x))
  })

  # 7) Call the function. This no longer makes any real network requests.
  result <- RDPget_search_metadata(RDP = dummy_conn_rd, searchView = "EquityQuotes")

  # 8) Check the structure of the returned object
  expect_true(is.data.frame(result))
  expect_equal(
    sort(names(result)),
    sort(c("Refinitiv_index", names(dummy_metadata)))
  )
  # Confirm classes
  expect_identical(class(result$Refinitiv_index), "character")
  expect_identical(class(result$Type),       "character")
  expect_identical(class(result$Searchable), "logical")
  expect_identical(class(result$Sortable),   "logical")
  expect_identical(class(result$Navigable),  "logical")
  expect_identical(class(result$Groupable),  "logical")
  expect_identical(class(result$Exact),      "logical")
  expect_identical(class(result$Symbol),     "logical")
})

test_that("RDPget_search_metadata can handle JSON‐style connections", {
  # Pretend the connection is "JSON" by stubbing.
  stub(RDPget_search_metadata, "PropertiesActiveRefinitivObject", function(verbose) {
    list(name = "JSON")
  })

  # This is the data.frame that the JSON connection would return:
  dummy_df <- data.frame(
    Refinitiv_index = "DummyIndex",
    Type       = "Equity",
    Searchable = TRUE,
    Sortable   = FALSE,
    Navigable  = TRUE,
    Groupable  = FALSE,
    Exact      = TRUE,
    Symbol     = FALSE,
    stringsAsFactors = FALSE
  )

  # Our “JSON” connection object: RDP = list(...).
  dummy_conn_json <- list(
    get_search_metadata = function(searchView) {
      # In real usage, it might do an HTTP POST, but here it just returns dummy_df
      dummy_df
    }
  )

  # Now call the function, verifying it yields dummy_df with no real calls.
  result <- RDPget_search_metadata(RDP = dummy_conn_json, searchView = "EquityQuotes")
  expect_true(is.data.frame(result))
  expect_identical(result, dummy_df)
})

test_that("RDPget_search_metadata errors for unknown searchView in RD mode", {
  # Fake “refinitiv.data” connection
  stub(RDPget_search_metadata, "PropertiesActiveRefinitivObject", function(verbose) {
    list(name = "refinitiv.data")
  })
  # Pretend "RD" can only do "OTHER_VIEW"
  stub(RDPget_search_metadata, "RDPShowAvailableSearchViews", function(Platform) {
    if (Platform == "RD") return("OTHER_VIEW") else return(character(0))
  })

  # Minimal dummy connection object
  dummy_conn <- list(
    content = list(
      search = list(
        metadata = list(
          Definition = function(view) stop("dummy")
        )
      )
    )
  )

  expect_error(
    RDPget_search_metadata(RDP = dummy_conn, searchView = "TEST_VIEW_RD"),
    "SearchView TEST_VIEW_RD not available for RD connection object"
  )
})

test_that("RDPget_search_metadata errors for unknown searchView in JSON mode", {
  # Fake “JSON” connection
  stub(RDPget_search_metadata, "PropertiesActiveRefinitivObject", function(verbose) {
    list(name = "JSON")
  })

  # The JSON approach might throw if the searchView is invalid
  dummy_conn <- list(
    get_search_metadata = function(searchView) {
      stop("SearchView not available for JSON")
    }
  )

  expect_error(
    RDPget_search_metadata(RDP = dummy_conn, searchView = "TEST_VIEW_JSON"),
    "SearchView not available for JSON"
  )
})

test_that("RDPget_search_metadata errors if the connection type is unsupported", {
  stub(RDPget_search_metadata, "PropertiesActiveRefinitivObject", function(verbose) {
    list(name = "other")
  })

  # If “other” is not recognized, the function eventually tries a call that fails:
  expect_error(
    RDPget_search_metadata(RDP = list(), searchView = "ANY"),
    "attempt to apply non-function"
  )
})
