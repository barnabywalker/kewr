#' Format filters for POWO search API.
#'
#' Checks the filters are valid before joining them
#' together with as a comma-separated string.
#'
#' @param filters A character vector of filter names.
#'
#' @noRd
format_filters_ <- function(filters, resource) {
  if (is.null(filters)) {
    return(NULL)
  }

  filter_map <- get_filters_(resource)
  valid_filters <- names(filter_map)
  bad_filters <- setdiff(filters, valid_filters)

  if (length(bad_filters) > 0) {
    stop(
      sprintf(
        "Filters for [%s] must be one of [%s]\n[%s] are not recognised.",
        resource,
        paste(valid_filters, collapse=","),
        paste(bad_filters, collapse=",")
      )
    )
  }

  paste(filter_map[filters], collapse=",")
}
