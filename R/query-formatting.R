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
#' Checks if query is valid and then formats it correctly.
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
  } else if(resource == "krs") {
    list(query=query)
  } else {
    list(q=query)
  }
}

#' Format query for an Open Refine API.
#'
#' Checks if query is valid, formats the keywords correctly, and makes it
#' a JSON string.
#'
#' @param query A string or list specifying the query.
#' @param resource A string specifying the resource being queried.
#'
#' @importFrom glue glue
#' @importFrom jsonlite toJSON
#' @importFrom purrr map2
#'
#' @noRd
format_refine_query_ <- function(query, resource) {
  query <- format_query_(query, resource)

  properties <- query[names(query) != "query"]
  properties <- map2(names(properties), properties, format_refine_property_)

  q <- query$query
  query <- list(properties=properties)
  if(!is.null(q)) {
    query$query <- q
  }

  query <- toJSON(query, auto_unbox=TRUE)
  list(query=query)
}

#' Format an Open Refine property for an API request.
#'
#' @noRd
format_refine_property_ <- function(name, value) {
  list(p=name, pid=name, v=value)
}


#' Format body for a POST request.
#'
#' The body of a POST request must be a list.
#' So far, only KNMS uses POST requests. Names
#' for matching might be input as a character vector,
#' so we need to ensure the body is a list and coerce it
#' if not.
#'
#' @param body The raw body as input.
#'
#' @noRd
format_body_ <- function(body) {

  if (! is.list(body) & ! is.character(body)) {
    message <- glue("The body of a POST request must be either a list or character vector.",
                    "Provided body type: {typeof(query)}",
                    "",
                    .sep="\n", .trim=FALSE)

    stop(message, call.=FALSE)
  }

  if (! is.list(body)) {
    body <- as.list(body)
  }

  body
}
