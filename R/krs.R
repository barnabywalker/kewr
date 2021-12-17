#' Match a name using KRS.
#'
#' Use the Kew Reconciliation Service to match a name against IPNI.
#'
#' The [Kew Reconciliation Service (KRS)](http://data1.kew.org/reconciliation/about/IpniName)
#' allows name matching against IPNI using an Open Refine reconcilliation API.
#'
#' @param query The name to match using the reconciliation service. Use a named list to
#'  match parts of a name.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#' @param .retries The max number of times to retry the request to KRS. KRS seems
#'  to fail every other request, so adding a small number of retries helps prevent
#'  unnecessary failure.
#'
#' @return
#' Returns an object of class `krs_match` that is a simple
#' structure with slots for:
#'
#'  * `results`: the query results parsed into a list.
#'  * `response`: the [httr response object][httr::response].
#'
#' @examples
#' # Match a name.
#' match_krs("Solanum sanchez-vegae")
#'
#' # Match a name using name parts
#' match_krs(list(genus="Solanum", species="sanchez-vegae", author="Knapp"))
#'
#' # Format a returned match as a dataframe
#' match <- match_krs(list(genus="Solanum", species="sanchez-vegae", author="Knapp"))
#' tidy(match)
#'
#' @seealso
#'  * [match_knms()] to use simple matching for a vector of names.
#'
#' @importFrom jsonlite toJSON
#'
#' @export
match_krs <- function(query, .wait=0.2, .retries=3) {
  url <- krs_url_()

  # keeping a copy of this to return in the result object
  original_query <- query

  query <- format_refine_query_(query, "krs")

  results <- make_request_(url, query, .wait=.wait, .retries=.retries)

  structure(
    list(
      matches=length(results$content$result),
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
