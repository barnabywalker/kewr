#' Search Kew's Tree of Life for specimens.
#' 
#' Query Kew's Tree of Life for specimens that have
#' been sampled for sequencing.
#' 
#' The [Tree of Life](https://treeoflife.kew.org/) is a database
#' of specimens sequenced as part of Kew's efforts to build
#' a comprehensive evolutionary tree of life for flowering plants.
#' 
#' The search API allows users to query the database for specimens
#' based on their taxonomic information. Filtering and keyword-search
#' are not currently implemented. All searches are based on taxonomic 
#' information, so `Myrcia` and `Myrtales` will return results, but 
#' `Brummitt` will not.
#' 
#' @param query The string to query the database with.
#' @param limit An integer specifying the number of results 
#'  to return.
#' @param page An integer specify the page of results to request.
#' @param .wait Time to wait before making a requests, to help
#'  rate limiting.
#' 
#' @return Returns an object of class `tol_search` that is a simple
#'  structure with slots for:
#'  
#'  * `total`: the total number of results held in ToL for the query.
#'  * `page`: the page of results requested.
#'  * `limit`: the maximum number of results requested from the API.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#' 
#' @examples
#' # get the first 50 of all sequenced specimens
#' search_tol(limit=50)
#' 
#' # search for all sequenced Myrcia specimens
#' search_tol("Myrcia")
#' 
#' # get all sequenced specimens
#' search_tol(limit=5000)
#' 
#' # search for a species name and print the results
#' r <- search_tol("Myrcia guianensis")
#' print(r)
#' 
#' # simplify search results to a `tibble`
#' r <- search_tol("Myrcia")
#' tidy(r)
#'
#' # gene stats are nested in the results
#' r <- search_tol("Myrcia")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=gene_stats)
#' 
#' # species names are nested in the results
#' r <- search_tol("Myrcia")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=species, names_sep="_")
#' 
#' # as is higher taxonomy
#' r <- search_tol("Myrcia")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=species, names_sep="_")
#' 
#' @references
#' Baker W.J., Bailey P., Barber V., Barker A., Bellot S., Bishop D., Botigue L.R., Brewer G., Carruthers T., Clarkson J.J., Cook J., Cowan R.S., Dodsworth S., Epitawalage N., Francoso E., Gallego B., Johnson M., Kim J.T., Leempoel K., Maurin O., McGinnie C., Pokorny L., Roy S., Stone M., Toledo E., Wickett N.J., Zuntini A.R., Eiserhardt W.L., Kersey P.J., Leitch I.J. & Forest F. 2021. A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life. Systematic Biology, 2021; syab035, https://doi.org/10.1093/sysbio/syab035
#' 
#' @family ToL functions
#'  * [lookup_tol()] to lookup information about a sequenced specimen
#'    using a valid ToL ID.
#' 
#' @export
search_tol <- function(query="", limit=50, page=1, .wait=0.2) {
  url <- tol_search_url_()

  original_query <- query
  query <- format_query_(query, "tol")

  query$per_page <- limit
  query$page <- page
  results <- make_request_(url, query, .wait=.wait)

  # calculate total number of pages, because it isn't returned
  total_pages <- ceiling(results$content$total / limit)

  structure(
    list(
      total=results$content$total,
      pages=total_pages,
      page=results$content$page,
      limit=limit,
      results=results$content$items,
      query=original_query,
      response=results$response
    ),
    class="tol_search"
  )
}

#' Look up a sequenced specimen in ToL.
#'
#' Request the record for a sequenced specimen in ToL using
#' its ToL ID.
#'
#' The [Tree of Life](https://treeoflife.kew.org/) is a database
#' of specimens sequenced as part of Kew's efforts to build
#' a comprehensive evolutionary tree of life for flowering plants.
#'
#' The specimen lookup API allows users to retrieve taxonomic and sequencing
#' information for a specific sequenced specimen using the unique ToL ID.
#' If this is not known, it can be found out using the [ToL search API][kewr::search_tol].
#'
#' @param specimenid A string containing a valid ToL specimen ID.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return A `tol_specimen` object, which is a simple structure with fields
#'   for each of the fields returned by the lookup API, 
#'   as well as the the [httr response object][httr::response].
#'
#' @examples
#'
#' # retrieve information for a particular specimen
#' lookup_tol("1296")
#'
#' # print a summary of the returned information
#' r <- lookup_tol("1296")
#' print(r)
#'
#' # tidy into a tibble
#' r <- lookup_tol("1296")
#' tidy(r)
#'
#' # tidy the returned list of synonyms into a tibble
#' r <- lookup_tol("1296")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=synonyms, names_sep="_")
#'
#' # expand the child entries returned for each entry
#' r <- lookup_wcvp("30000055-2")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=children, names_sep="_")
#'
#' @family ToL functions
#' @seealso
#'  * [search_tol()] to search ToL using taxonomic information.
#'
#' @references
#' Baker W.J., Bailey P., Barber V., Barker A., Bellot S., Bishop D., Botigue L.R., Brewer G., Carruthers T., Clarkson J.J., Cook J., Cowan R.S., Dodsworth S., Epitawalage N., Francoso E., Gallego B., Johnson M., Kim J.T., Leempoel K., Maurin O., McGinnie C., Pokorny L., Roy S., Stone M., Toledo E., Wickett N.J., Zuntini A.R., Eiserhardt W.L., Kersey P.J., Leitch I.J. & Forest F. 2021. A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life. Systematic Biology, 2021; syab035, https://doi.org/10.1093/sysbio/syab035
#'
#' @export
lookup_tol <- function(specimenid, .wait=0.1) {
  url <- tol_specimen_url_(specimenid)

  result <- make_request_(url, query=NULL, .wait=.wait)

  # this might be better if things were explicitly listed
  record <- result$content
  record$response <- result$response
  record$queryId <- specimenid

  structure(
    record,
    class="tol_specimen"
  )
}

#' Make the ToL specimen lookup URL.
#'
#' @param specimenid A valid ToL ID.
#'
#' @noRd
#'
#' @importFrom glue glue
tol_specimen_url_ <- function(specimenid) {
  base <- get_url_("tol")

  glue("{base}/specimens/{specimenid}")
}

#' Make Tree of Life search URL.
#' 
#' @importFrom glue glue
#' 
#' @noRd
tol_search_url_ <- function() {
  base <- get_url_("tol")

  glue("{base}/specimens")
}