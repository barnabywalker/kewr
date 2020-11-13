WCVP_SEARCH_URL <- "https://wcvp.science.kew.org/api/v1/search"
USER_AGENT <- httr::user_agent("https://github.com/barnabywalker/kewr")
WCVP_FILTERS <- c("accepted", "generic", "specific", "infraspecific")

#' Search WCVP for a taxon.
#'
#' Query the World Checklist of Vascular Plants search API
#' for a taxon string.
#'
#' The [World Checklist of Vascular Plants (WCVP)](https://wcvp.science.kew.org/)
#' is a global consensus view of all known vascular plant species.
#' It has been compiled by staff at RBG Kew in consultation with plant
#' group experts.
#'
#' The search API allows users to query the checklist for plant names.
#' Currently, it does not support partial or fuzzy matching.
#' In order to get a result, the user must supply a valid name string.
#' For example, 'Myrcia' and 'Myrcia guianensis' will return results,
#' but 'M' or 'Myr' will not.
#'
#' The API will return taxonomic information (the family, authority, status, and rank)
#' of all names matching the query. These results can be limited, for example to accepted species,
#' using filters.
#'
#' @param query The taxon string to search WCVP for.
#' @param filters Filter to apply to search results. Can be one
#' or more of `accepted`, `generic`, `specific`, `intraspecific`.
#' Multiple filters must be supplied as a character vector.
#' @param page An integer specifying the page of results to return.
#' @param limit An integer specifying the maximum number of results
#'  to return.
#' @return Returns an object of class `wcvp_search` that is a simple
#' stucture with slots for:
#'
#'  * `total`: the total number of results held in WCVP for the query
#'  * `page`: the page number requested from the API.
#'  * `limit`: the maximum number of results requested from the API.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `filter`: the filter strings submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#' @examples
#' # search for all entries containing a genus name
#' search_wcvp("Myrcia")
#'
#' # search for all accepted species within a genus
#' search_wcvp("Myrcia", filters=c("specific", "accepted"))
#'
#' # search for up to 10,000 species in a genus
#' search_wcvp("Poa", filters=c("specific"), limit=10000)
#'
#' # search for a species name and print the results
#' r <- search_wcvp("Myrcia guianensis", filters=c("specific"))
#' print(r)
#'
#' # simplify search results to a `tibble`
#' r <- search_wcvp("Poa", filters=c("specific"))
#' format(r)
#'
#' # accepted name info is nested inside the records for synonyms
#' # simplify accepted name info to the name ID
#' r <- search_wcvp("Poa", filters=c("specific"))
#' format(r, synonyms="simplify")
#'
#' # expand accepted name info
#' r <- search_wcvp("Poa", filters=c("specific"))
#' format(r, synonyms="expand")
#'
#' @references
#' WCVP (2020). World Checklist of Vascular Plants, version 2.0. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://wcvp.science.kew.org/
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' @export
search_wcvp <- function(query, filters=NULL, page=0, limit=50) {
  url <- WCVP_SEARCH_URL

  query <- list(q=query, page=page, limit=limit)
  query$f <- format_filters(filters)

  response <- GET(url, USER_AGENT, query=query)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call.=FALSE)
  }

  parsed <- fromJSON(content(response, "text"), simplifyVector=FALSE)

  if (http_error(response)) {
    stop(
      sprintf(
        "WCVP search query failed with code [%s]\n<%s: %s>",
        status_code(response),
        parsed$error,
        parsed$message
      ),
      call.=FALSE
    )
  }

  structure(
    list(
      total=parsed$total,
      page=parsed$page,
      limit=parsed$limit,
      results=parsed$results,
      query=query$q,
      filters=query$f,
      response=response
    ),
    class="wcvp_search"
  )
}

#' @import glue
#' @importFrom utils str head
#' @export
print.wcvp_search <- function(x, ...) {
  filters <- ifelse(is.null(x$filters), "none", x$filters)
  message <- glue("<WCVP search: '{x$query}' filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  str(head(x$results, 1))
  invisible()
}

format_filters <- function(filters) {

  if (is.null(filters)) {
    return(NULL)
  }

  bad_filters <- setdiff(filters, WCVP_FILTERS)
  if (length(bad_filters) > 0) {
    stop(
      sprintf(
        "Filters must be one of [%s]\n[%s] are not recognised.",
        paste(WCVP_FILTERS, collapse=","),
        paste(bad_filters, collapse=",")
      )
    )
  }

  paste(filters, collapse=",")
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @export
format.wcvp_search <- function(x, synonyms=c("ignore", "simplify", "expand"), ...) {
  synonyms <- match.arg(synonyms)

  if (synonyms == "ignore") {
    fcn <- as_tibble
  } else if (synonyms == "simplify") {
    fcn <- function(r) {
      synonym_id <- r$synonymOf$id
      r$synonymOf <- NULL
      formatted <- as_tibble(r)
      formatted$synonymOf <- synonym_id

      formatted
    }
  } else if (synonyms == "expand") {
    fcn <- function(r) {
      synonym_col <- as_tibble(
        r$synonymOf,
        .name_repair=~paste0("synonymOf_", .x)
      )
      r$synonymOf <- NULL
      formatted <- as_tibble(r)
      bind_cols(r, synonym_col)
    }
  }

  map_dfr(x$results, fcn)
}
