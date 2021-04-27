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
#' keyword arguments. Use the `get_keywords` function for a list of all implemented keywords.
#'
#' The API returns taxonomic information as well as species descriptions and
#' image locations if available. These results can be limited, for example to accepted species,
#' using filters. Use the `get_filters` function to get a list of all implemented filters.
#'
#' Distributions in POWO are categorised using the [World Geographical Scheme for
#' Recording Plant Distributions (WGSRP)](https://www.tdwg.org/standards/wgsrpd/).
#' Users can query POWO using distributions listed under WGSRPD levels 1 (continents),
#' 2 (regions), and 3 (botanical countries).
#'
#' @param query The string to query POWO with. If using keywords,
#'  the query must be formatted as a list.
#' @param filters Filter to apply to search results.
#'  Multiple filters must be supplied as a character vector.
#' @param cursor A cursor returned by a previous search.
#'  If used, the query and filter must be exactly the same.
#' @param limit The maximum number of records to return.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return
#' Returns an object of class `powo_search` that is a simple
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
#' # search for all entries containing a genus name
#' search_powo("Myrcia")
#'
#' # search for all accepted species within a genus
#' search_powo("Myrcia", filters=c("species", "accepted"))
#'
#' # search for up to 100 species in a genus
#' search_powo("Poa", filters=c("species"), limit=100)
#'
#' # search for all names in a family
#' search_powo(list(family="Myrtaceae"))
#'
#' # search for all accepted species with blue flowers
#' search_powo(list(flower="blue"), filters=c("accepted", "species"))
#'
#' # search for all accepted genera in Mexico
#' search_powo(list(distribution="Mexico"), filters=c("accepted", "genera"))
#'
#' # search for a species name and print the results
#' r <- search_powo("Myrcia guianensis", filters=c("species"))
#' print(r)
#'
#' # simplify search results to a `tibble`
#' r <- search_powo("Poa", filters=c("species"))
#' tidy(r)
#'
#' @family POWO functions
#' @seealso
#'  * [lookup_powo()] to look up a taxon in POWO using the IPNI ID.
#'
#' @export
search_powo <- function(query, filters=NULL, cursor=NULL, limit=50, .wait=0.2) {
  url <- powo_search_url_()

  # keeping a copy of this to return in the result object
  original_query <- query

  query <- format_query_(query, "powo")

  query$perPage <- limit
  query$cursor <- cursor
  query$f <- format_filters_(filters, "powo")

  results <- make_request_(url, query, .wait=.wait)

  structure(
    list(
      total=results$content$totalResults,
      pages=results$content$totalPages,
      limit=results$content$perPage,
      cursor=results$content$cursor,
      results=results$content$results,
      query=original_query,
      filters=filters,
      response=results$response
    ),
    class="powo_search"
  )
}

#' Look up a taxon in POWO.
#'
#' Request the record for a taxon in Plants of the World Online (POWO)
#' using the IPNI ID.
#'
#' [Plants of the World Online (POWO)](http://www.plantsoftheworldonline.org/)
#' is a database of information on the world's flora. It curates information from
#' published floras and other sources of floristic information.
#'
#' The taxon lookup API allows users to retrieve information about
#' a specific taxon name using the unique IPNI ID. If this is not known,
#' it can be found out using the [POWO search API][kewr::search_powo].
#'
#' @param taxonid A string containing a valid IPNI ID.
#' @param distribution Include distribution in results (default `FALSE`).
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return A `powo_taxon` object, which is a simple structure with fields
#'   for each of the fields returned by the lookup API, as well as the the [httr response object][httr::response].
#'
#' @examples
#' # retrieve information for a taxon name
#' lookup_powo("271445-2")
#'
#' # print a summary of the returned information
#' r <- lookup_powo("271445-2")
#' print(r)
#'
#' # tidy returned record into a tibble
#' r <- lookup_powo("271445-2")
#' tidy(r)
#'
#' # tidy the returned list of synonyms into a tibble
#' r <- lookup_wcvp("60447743-2")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=synonyms, names_sep="_")
#'
#' # tidy the returned list of children into a tibble
#' r <- lookup_wcvp("30000055-2")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=children, names_sep="_")
#'
#' @family POWO functions
#' @seealso
#'  * [search_powo()] to search POWO using a taxon name.
#'
#' @export
lookup_powo <- function(taxonid, distribution=FALSE, .wait=0.2) {
  url <- powo_taxon_url_(taxonid)

  query <- NULL
  if (distribution) {
    query <- list(fields="distribution")
  }

  result <- make_request_(url, query=query, .wait=.wait)

  # this might be better if things were explicitly listed
  record <- result$content
  record$response <- result$response
  record$queryId <- taxonid

  structure(
    record,
    class="powo_taxon"
  )
}

#' @noRd
powo_search_url_ <- function() {
  base <- get_url_("powo")

  paste0(base, "/search")
}

#' @noRd
powo_taxon_url_ <- function(taxonid) {
  base <- get_url_("powo")

  glue("{base}/taxon/urn:lsid:ipni.org:names:{taxonid}")
}
