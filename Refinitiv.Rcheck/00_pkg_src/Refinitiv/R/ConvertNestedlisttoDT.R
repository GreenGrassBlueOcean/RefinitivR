#' Convert a Nested List to a Data Table
#'
#' This function takes a nested list where each element is a list and converts it into a `data.table`.
#' Each sublist in the nested list is converted to a `data.table`, and all these `data.table` objects
#' are combined into a single `data.table`. The function also handles automatic type conversion for the
#' columns.
#'
#' @param input A nested list where each element is a list with named elements.
#' @return A `data.table` combining all sublists with appropriate column types.
#' @examples
#' library(data.table)
#' nested_list <- list(
#'   list(a = 1, b = 2.5, c = "hello"),
#'   list(a = 3, b = 4.5, c = "world"),
#'   list(a = 5, b = 6.5, c = "foo", d = TRUE)
#' )
#' ConvertNestedlisttoDT(nested_list)
#' @noRd
ConvertNestedlisttoDT <- function(input) {
  # Convert each element of the input to a data.table
  response_DT <- lapply(input, function(x) {
    # Unlist each element into a flat list
    flat_list <- as.list(unlist(x, recursive = FALSE))

    # Convert the flat list to a data.table
    dt <- data.table::as.data.table(flat_list)

    # Ensure that each column in dt has the same class as in the original list
    for (col in names(x)) {
      dt[[col]] <- switch(class(x[[col]])[1],
                          "character" = as.character(dt[[col]]),
                          "numeric" = as.numeric(dt[[col]]),
                          "integer" = as.integer(dt[[col]]),
                          "logical" = as.logical(dt[[col]]),
                          dt[[col]])  # default case: keep as is
    }


    # Return the data.table
    return(dt)
  })

  # Combine all data.tables into one
  combined_DT <- data.table::rbindlist(response_DT, fill = TRUE, use.names = TRUE)

  # Remove unwanted '.V' prefix and keep numeric suffix
  data.table::setnames(combined_DT, gsub("\\.V([0-9]+)$", "\\1", names(combined_DT)))

  return(combined_DT)
}
