#' Format filters for search APIs.
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

#' Format queries for search APIs.
#'
#' Checks if query is a keyword or string query,
#' and makes sure any keywords a valid.
#'
#' @param query A string or list specifying the query.
#' @param resource A string specifying the resource being queried.
#'
#' @importFrom glue glue
#'
#' @noRd
format_query_ <- function(query, resource) {

  if (! is.list(query) & length(query) > 1) {
    message <- glue("{resource} search query must be a named list or a string.",
                    "Provided query type: {typeof(query)}",
                    "Provided query length: {query_length}",
                    "",
                    .sep="\n", .trim=FALSE)

    stop(message, call.=FALSE)
  }

  if (is.list(query)) {
    keywords <- names(query)
    keyword_map <- get_keywords_(resource)
    valid_keywords <- names(keyword_map)
    bad_keywords <- setdiff(keywords, valid_keywords)

    if (length(bad_keywords) > 0) {
      stop(
        sprintf(
          "Query keywords for [%s] must be one of [%s]\n[%s] are not recognised.",
          resource,
          paste(valid_keywords, collapse=","),
          paste(bad_keywords, collapse=",")
        )
      )
    }
  }

  if(is.list(query)) {
    names(query) <- keyword_map[keywords]
    query
  } else {
    list(q=query)
  }
}
