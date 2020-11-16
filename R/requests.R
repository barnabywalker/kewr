#' Get the names of valid keywords for querying a resource.
#'
#' @param resource The resource being queried.
#'
#' @return A character vector of filter names.
#'
#' @noRd
get_keywords_ <- function(resource=c("wcvp")) {
  resource <- match.arg(resource)

  switch(
    resource,
    wcvp=c("family", "genus", "species")
  )
}

#' Get the names of valid filters for a resource.
#'
#' @param resource The resource being queried.
#'
#' @return A character vector of filter names.
#'
#' @noRd
get_filters_ <- function(resource=c("wcvp")) {
  resource <- match.arg(resource)

  switch(
    resource,
    wcvp=c("accepted", "generic", "specific", "infraspecific")
  )
}

#' Get the base URL for a particular resource.
#'
#' @param resource Name of a Kew resource.
#' @return The base URL for the requested resource.
#'
#' @noRd
get_url_ <- function(resource=c("wcvp", "powo")) {
  resource <- match.arg(resource)

  switch(resource,
         wcvp="https://wcvp.science.kew.org/api/v1",
         powo="http://www.plantsoftheworldonline.org/api/2")
}

#' Get the package user agent.
#'
#' @noRd
#'
#' @importFrom httr user_agent
get_user_agent_ <- function() {
  user_agent("https://github.com/barnabywalker/kewr")
}

#' Make a request to a Kew resource.
#'
#' @param url The URL for the resource API.
#' @param query A list specifying an optional query.
#'
#' @return A list containing the returned response object and
#'   the response content parsed into a list.
#'
#' @noRd
#'
#' @import httr
#' @importFrom jsonlite fromJSON
make_request_ <- function(url, query) {
  user_agent <- get_user_agent_()
  response <- GET(url, user_agent, query=query)

  if (http_error(response)) {
    status <- http_status(response)
    code <- status_code(response)
    message <- status$message

    stop(
      glue("Request to '{url}' failed with code {code}: {message}"),
      call.=FALSE
    )
  }

  if (http_type(response) != "application/json") {
    stop("API did not return json", call.=FALSE)
  }

  parsed <- fromJSON(content(response, "text"), simplifyVector=FALSE)

  list(response=response, content=parsed)
}
