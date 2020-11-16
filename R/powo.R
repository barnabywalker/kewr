#' Search POWO.
#'
#' Query Plants of the World Online for taxon information.
#'
#' [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/)
#' is a database of information on the world's flora. It curates information from
#' published floras and other sources of floristic information.
#'
#' The search API allows users to query the database using plant names,
#' geographic terms, and floristic characters. These can be queried using
#' keyword arguments. See arguments for all implemented keywords.
#'
#' The API returns taxonomic information as well as species descriptions and
#' image locations if available. These results can be limited, for example to accepted species,
#' using filters. See arguments for all implemented filters.
#'
#' @param query The string to query POWO with.
#' @param limit The maximum number of records to return.
#'
#' @return
#' Returns an object of class `powo_search` that is a simple
#' stucture with slots for:
#'
#'  * `total`: the total number of results held in POWO for the query
#'  * `pages`: the total number of results pages for the query.
#'  * `limit`: the maximum number of results requested from the API, per page.
#'  * `cursor`: a cursor to retrieve the next page of results from the API.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#'
#' @export
search_powo <- function(query, limit=24) {
  url <- powo_search_url_()

  query <- list(q=query, perPage=limit)

  results <- make_request_(url, query)

  structure(
    list(
      total=results$content$totalResults,
      pages=results$content$totalPages,
      limit=results$content$perPage,
      cursor=results$content$cursor,
      results=results$content$results,
      query=query,
      response=results$response
    ),
    class="powo_search"
  )
}

# object print methods ----
#' @export
print.powo_search <- function(x, ...) {
  message <- glue("<POWO search: '{x$query}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  str(head(x$results, 1))
  invisible()
}

# object format methods ----

#' @importFrom purrr map_dfr
#'
#' @export
format.powo_search <- function(x, ...) {
  map_dfr(x$results, as_tibble)
}

# URL utility functions ----
#' @noRd
powo_search_url_ <- function() {
  base <- get_url_("powo")

  paste0(base, "/search")
}
