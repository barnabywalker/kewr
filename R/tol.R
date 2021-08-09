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
#' based on their taxonomic information.
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

#' Make Tree of Life search URL.
#' 
#' @importFrom glue glue
#' 
#' @noRd
tol_search_url_ <- function() {
  base <- get_url_("tol")

  glue("{base}/specimens")
}