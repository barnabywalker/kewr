#' Search IPNI.
#'
#' Query the International Plant Names Index for nomenclatural information.
#'
#' The [International Plant Names Index (IPNI)](https://www.ipni.org/)
#' is a service that provides nomenclatural information for vascular plant names.
#'
#' The search API allows users to query the database for plant names,
#' as well as authors and publications. There may be limited support for
#' some sort of fuzzy matching.
#'
#' There is some support for querying using keyword arguments. Documentation for
#' the API is currently available in the [`pykew` package](https://github.com/RBGKew/pykew/blob/master/pykew/ipni_terms.py),
#' so keywords have been copied across from there. There are sets of keywords
#' relating to plants, authors, and publications.
#'
#' The API will return nomenclatural information (publication date, nomenclatural status, author, etc.)
#' of all names matching the query. These results can be limited, for example to only family names,
#' using filters. See arguments for all implemented filters.
#'
#' @param query The string to query IPNI with.
#' @param filters Filter to apply to search results. Can be one
#' or more of `families`, `genera`, `species`, `infrafamilies`,
#' `infragenera`, and `infraspecies`.
#' @param limit The maximum number of records to return.
#'
#' @return
#' Returns an object of class `ipni_search` that is a simple
#' structure with slots for:
#'
#'  * `total`: the total number of results held in POWO for the query
#'  * `pages`: the total number of results pages for the query.
#'  * `limit`: the maximum number of results requested from the API, per page.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#'
#' @examples
#' # search for all names containing Poa annua
#' results <- search_ipni("Poa annua")
#'
#' # format search results in a table
#' format(results)
#'
#' # extract author team information for the search results
#' results_tbl <- format(results)
#' tidyr::unnest(results_tbl, cols=c(authorTeam), names_sep="_")
#'
#' # filter results to only species names
#' species_results <- search_ipni("Poa annua", filters="species")
#' format(species_results)
#'
#' # search for species from Mexico published in 1989
#' q <- list(published="1989", distribution="Mexico")
#' f <- "species"
#' results <- search_ipni(q, filters=f)
#' format(results)
#'
#' # search for an author by surname
#' author_results <- search_ipni(list(author_surname="Gardiner"))
#' format(author_results)
#'
#' @export
search_ipni <- function(query, filters=NULL, limit=50) {
  url <- ipni_search_url_()

  query <- format_query_(query, "ipni")
  # keeping a copy of this to return in the result object
  original_query <- query

  query$limit <- limit
  query$f <- format_filters_(filters, "ipni")

  results <- make_request_(url, query)

  structure(
    list(
      total=results$content$totalResults,
      pages=results$content$totalPages,
      limit=results$content$perPage,
      results=results$content$results,
      query=original_query,
      filters=query$f,
      response=results$response
    ),
    class="ipni_search"
  )
}

#' @noRd
ipni_search_url_ <- function() {
  base <- get_url_("ipni")

  paste0(base, "/search")
}
