#' Match a name using KRS.
#'
#' Use the Kew Reconciliation Service to match a name against IPNI.
#'
#' The [Kew Reconciliation Service (KRS)](http://data1.kew.org/reconciliation/about/IpniName)
#' allows name matching against IPNI using an Open Refine reconcilliation API.
#'
#' @param query The name to match using the reconciliation service.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return
#' Returns an object of class `krs_match` that is a simple
#' structure with slots for:
#'
#'  * `results`: the query results parsed into a list.
#'  * `response`: the [httr response object][httr::response].
#'
#' @examples
#' # Match a single name.
#' match_krs("Solanum sanchez-vegae")
#'
#' @seealso
#'  * [match_knms()] to use simple matching for a vector of names.
#'
#' @importFrom jsonlite toJSON
#'
#' @export
match_krs <- function(query, .wait=0.2) {
  url <- krs_url_()

  # keeping a copy of this to return in the result object
  original_query <- query

  if (! is.list(query)) {
    # different from all the other services, so do a little formatting here
    query <- list(query=query)
  }

  query <- format_query_(query, "krs")

  # all queries are submitted as a JSON specification
  query <- toJSON(query, auto_unbox=TRUE)

  query <- list(query=query)

  results <- make_request_(url, query, .wait=.wait)

  structure(
    list(
      results=results$content$result,
      query=original_query,
      response=results$response
    ),
    class="krs_match"
  )
}

#' Make the KNMS URL.
#'
#' @noRd
krs_url_ <- function() {
  get_url_("krs")
}
