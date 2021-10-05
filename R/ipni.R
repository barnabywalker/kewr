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
#' relating to plants, authors, and publications. A full list of keywords can be viewed
#' using the `get_keywords` function.
#'
#' The API will return nomenclatural information (publication date, nomenclatural status, author, etc.)
#' of all names matching the query. These results can be limited, for example to only family names,
#' using filters. Use the `get_filters` function to get a list of all implemented filters.
#'
#' @param query The string to query IPNI with. If using keywords, the query
#'  must be formatted as a list.
#' @param filters Filter to apply to search results.
#' @param cursor A cursor returned by a previous search.
#'  If used, the query and filter must be exactly the same.
#' @param limit The maximum number of records to return. This has a maximum of 1000.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return
#' Returns an object of class `ipni_search` that is a simple
#' structure with slots for:
#'
#'  * `total`: the total number of results held in POWO for the query
#'  * `pages`: the total number of results pages for the query.
#'  * `limit`: the maximum number of results requested from the API, per page.
#'  * `cursor`: a cursor to retrieve the next page of results from the API.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#'
#' @examples
#' # search for all names containing Poa annua
#' results <- search_ipni("Poa annua")
#'
#' # tidy search results in a table
#' tidy(results)
#'
#' # extract author team information for the search results
#' results_tbl <- tidy(results)
#' tidyr::unnest(results_tbl, cols=c(authorTeam), names_sep="_")
#'
#' # filter results to only species names
#' species_results <- search_ipni("Poa annua", filters="species")
#' tidy(species_results)
#'
#' # search for species from Mexico published in 1989
#' q <- list(published="1989", distribution="Mexico")
#' f <- "species"
#' results <- search_ipni(q, filters=f)
#' tidy(results)
#'
#' # search for an author by surname
#' author_results <- search_ipni(list(author_surname="Gardiner"))
#' tidy(author_results)
#'
#' @family IPNI functions
#' @seealso
#'  * [lookup_ipni()] to look up a name using an IPNI ID.
#'
#' @export
search_ipni <- function(query, filters=NULL, cursor="*", limit=50, .wait=0.1) {
  url <- ipni_search_url_()

  # keeping a copy of this to return in the result object
  original_query <- query

  query <- format_query_(query, "ipni")

  query$perPage <- limit
  query$cursor <- cursor
  query$f <- format_filters_(filters, "ipni")

  results <- make_request_(url, query, .wait=.wait)

  structure(
    list(
      total=results$content$totalResults,
      pages=results$content$totalPages,
      cursor=results$content$cursor,
      limit=results$content$perPage,
      results=results$content$results,
      query=original_query,
      filters=filters,
      response=results$response
    ),
    class="ipni_search"
  )
}

#' Look up a name in IPNI.
#'
#' Request the record for a taxon, author, or publication name in IPNI,
#' using the IPNI ID.
#'
#' The [International Plant Names Index (IPNI)](https://www.ipni.org/)
#' is a service that provides nomenclatural information for vascular plant names.
#'
#' The name lookup API allows users to retrieve information for
#' a specific taxon, author, or publication name using the unique IPNI ID. If this is not known,
#' it can be found out using the [IPNI search API][kewr::search_ipni].
#'
#' @param id A string containing a valid IPNI ID.
#' @param type The type of record to look up. Either `taxon`, `author`, or `publication`.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return An `ipni_taxon` object, which is a simple structure with fields
#'   for each of the fields returned by the lookup API, as well as the the [httr response object][httr::response].
#'
#' @examples
#'
#' # retrieve nomenclatural information for a taxon name
#' name <- lookup_ipni("271445-2", "taxon")
#' print(name)
#'
#' # tidy the results in a table
#' tidy(name)
#'
#' # retrieve nomenclatural information for an author
#' name <- lookup_ipni("20028192-1", type="author")
#' tidy(name)
#'
#' # retrieve nomenclatural information for a publication
#' name <- lookup_ipni("987-2", type="publication")
#' tidy(name)
#'
#' @family IPNI functions
#' @seealso
#'  * [search_ipni()] to search IPNI using a taxon name.
#'
#' @export
lookup_ipni <- function(id, type=c("taxon", "author", "publication"), .wait=0.1) {
  type <- match.arg(type)

  url <- ipni_lookup_url_(id, type)

  result <- make_request_(url, query=NULL, .wait=.wait)

  # this might be better if things were explicitly listed
  record <- result$content
  record$response <- result$response
  record$queryId <- id

  structure(
    record,
    class=paste0("ipni_", record$recordType)
  )
}

#' Make the IPNI search URL.
#'
#' @noRd
ipni_search_url_ <- function() {
  base <- get_url_("ipni")

  paste0(base, "/search")
}

#' Make the IPNI name lookup URL.
#'
#' @param id A valid IPNI ID.
#' @param type The type of name to lookup.
#'
#' @noRd
#'
#' @importFrom glue glue
ipni_lookup_url_ <- function(id, type) {
  base <- get_url_("ipni")

  type <- switch(type,
                 taxon="n",
                 author="a",
                 publication="p")

  glue("{base}/{type}/{id}")
}
