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
#' @param query The string to query POWO with. If using keywords,
#'  the query must be formatted as a list and keywords must be one or more
#'  of `family`, `genus`, `species`, `name`, `common_name`, `author`, `characteristic`,
#'  `summary`, `appearance`, `flower`, `fruit`, `leaf`, `inflorescence`, `seed`,
#'  `cloning`, `use`, `distribution.`
#' @param filters Filter to apply to search results. Can be one
#' or more of `accepted`, `families`, `genera`, `species`,
#' `infraspecies`, `has_images`.
#' Multiple filters must be supplied as a character vector.
#' @param cursor A cursor returned by a previous search.
#'  If used, the query and filter must be exactly the same.
#' @param limit The maximum number of records to return.
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
#' format(r)
#'
#' @family POWO functions
#' @seealso
#'  * [lookup_powo()] to look up a taxon in POWO using the IPNI ID.
#'
#' @export
search_powo <- function(query, filters=NULL, cursor=NULL, limit=50) {
  url <- powo_search_url_()

  query <- format_query_(query, "powo")
  # keeping a copy of this to return in the result object
  original_query <- query

  query$perPage <- limit
  query$cursor <- cursor
  query$f <- format_filters_(filters, "powo")

  results <- make_request_(url, query)

  structure(
    list(
      total=results$content$totalResults,
      pages=results$content$totalPages,
      limit=results$content$perPage,
      cursor=results$content$cursor,
      results=results$content$results,
      query=original_query,
      filters=query$f,
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
#'
#' @return A `powo_taxon` object, which is a simple structure with fields
#'   for each of the fields returned by the lookup API, as well as the the [httr response object][httr::response].
#'
#' @examples
#'
#' # retrieve information for a taxon name
#' lookup_powo("271445-2")
#'
#' # print a summary of the returned information
#' r <- lookup_powo("271445-2")
#' print(r)
#'
#' # format returned record into a tibble
#' r <- lookup_powo("271445-2")
#' format(r)
#'
#' # format the returned list of synonyms into a tibble
#' r <- lookup_wcvp("60447743-2")
#' formatted <- format(r)
#' tidyr::unnest(formatted, cols=synonyms, names_sep="_")
#'
#' # format the returned list of children into a tibble
#' r <- lookup_wcvp("30000055-2")
#' formatted <- format(r)
#' tidyr::unnest(formatted, cols=children, names_sep="_")
#'
#' @family POWO functions
#' @seealso
#'  * [search_powo()] to search POWO using a taxon name.
#'
#' @export
lookup_powo <- function(taxonid, distribution=FALSE) {
  url <- powo_taxon_url_(taxonid)

  query <- NULL
  if (distribution) {
    query <- list(fields="distribution")
  }

  result <- make_request_(url, query=query)

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
