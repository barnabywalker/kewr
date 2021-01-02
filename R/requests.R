#' Get the names of valid keywords for querying a resource.
#'
#' @param resource The resource being queried.
#'
#' @return A character vector of filter names.
#'
#' @noRd
get_keywords_ <- function(resource=c("wcvp", "powo", "ipni")) {
  resource <- match.arg(resource)

  switch(
    resource,
    wcvp=c(
      family="family",
      genus="genus",
      species="species"
    ),
    powo=c(
      # name
      name="name",
      common_name="common name",
      family="family",
      genus="genus",
      species="species",
      author="author",
      # characteristics
      characteristic="characteristic",
      summary="summary",
      appearance="appearance",
      flower="flower",
      fruit="fruit",
      leaf="leaf",
      inflorescence="inflorescence",
      seed="seed",
      cloning="cloning",
      use="use",
      # geography
      distribution="location"
    ),
    ipni=c(
      # name
      added="added",
      author="name author",
      basionym="basionym",
      basionym_author="basionym author",
      bibliographic_reference="bibliographic reference",
      citation_type="citation type",
      collection_number="collection number",
      collectors="collector team",
      distribution="distribution",
      family="family",
      full_name="full name",
      genus="genus",
      in_powo="in powo",
      infrafamily="infrafamily",
      infragenus="infragenus",
      infraspecies="infraspecies",
      modified="modified",
      name_status="name status",
      published="published",
      published_in="published in",
      publishing_author="publishing author",
      rank="rank",
      scientific_name="scientific name",
      species="species",
      species_author="species author",
      version="version",
      # author
      author_forename="author forename",
      author_full_name="author name",
      author_std_form="author std",
      author_surname="author surname",
      # publication
      pub_std_form="publication std",
      bph_number="bph number",
      pub_date="date",
      isbn="isbn",
      issn="issn",
      lc_number="lc number",
      preceded_by="preceded by",
      superceded_by="superceded by",
      title="publication title",
      tl2_author="tl2 author",
      tl2_number="tl2 number"
    )
  )
}

#' Get the names of valid filters for a resource.
#'
#' @param resource The resource being queried.
#'
#' @return A character vector of filter names.
#'
#' @noRd
get_filters_ <- function(resource=c("wcvp", "powo", "ipni")) {
  resource <- match.arg(resource)

  switch(
    resource,
    wcvp=c(accepted="accepted",
           families="family",
           genera="generic",
           species="specific",
           infraspecies="infraspecific"),
    powo=c(accepted="accepted_names",
           has_images="has_images",
           families="family_f",
           genera="genus_f",
           species="species_f",
           infraspecies="infraspecific_f"),
    ipni=c(families="f_familial",
           genera="f_generic",
           species="f_specific",
           infraspecies="f_infraspecific",
           infragenera="f_infrageneric",
           infrafamilies="f_infrafamilial")
  )
}

#' Get the base URL for a particular resource.
#'
#' @param resource Name of a Kew resource.
#' @return The base URL for the requested resource.
#'
#' @noRd
get_url_ <- function(resource=c("wcvp", "powo", "knms", "ipni")) {
  resource <- match.arg(resource)

  switch(resource,
         wcvp="https://wcvp.science.kew.org/api/v1",
         powo="http://www.plantsoftheworldonline.org/api/2",
         knms="http://namematch.science.kew.org/api/v2/powo/match",
         ipni="https://www.ipni.org/api/1")
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
#' @param query A list specifying a query.
#' @param body A list specifying an optional body. If specified,
#' the function will make a POST request to the resource.
#'
#' @return A list containing the returned response object and
#'   the response content parsed into a list.
#'
#' @noRd
#'
#' @import httr
#' @importFrom jsonlite fromJSON
make_request_ <- function(url, query, body=NULL) {
  user_agent <- get_user_agent_()

  if (! is.null(body)) {
    response <- POST(url, user_agent, body=body, encode="json")
  } else {
    response <- GET(url, user_agent, query=query)
  }

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
