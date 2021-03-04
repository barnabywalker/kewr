
#' Request the next page of search results for a kewr results object.
#'
#' @param object A kewr search results object.
#'
#' @return The next page of results as a kewr search object of
#' the same type.
#'
#' @export
request_next <- function(object) {
  UseMethod("request_next")
}

#' @describeIn request_next Request the next page of WCVP search results.
#'
#' @examples
#' r <- search_wcvp("Poa")
#' request_next(r)
#'
#' @export
request_next.wcvp_search <- function(object) {
  .wait <- calculate_wait_(object)
  current_page <- object$page

  search_wcvp(
    query=object$query,
    filters=object$filters,
    limit=object$limit,
    cursor=object$cursor,
    .wait=.wait
  )
}

#' @describeIn request_next Request the next page of POWO search results.
#'
#' @examples
#'
#' \donttest{
#' r <- search_powo("Poa")
#' request_next(r)
#' }
#'
#' @export
request_next.powo_search <- function(object) {
  .wait <- calculate_wait_(object)
  current_page <- object$page

  search_powo(
    query=object$query,
    filters=object$filters,
    limit=object$limit,
    cursor=object$cursor,
    .wait=.wait
  )
}

#' @describeIn request_next Request the next page of IPNI search results.
#'
#' @examples
#' r <- search_ipni("Poa")
#' request_next(r)
#'
#' @export
request_next.ipni_search <- function(object) {
  .wait <- calculate_wait_(object)
  current_page <- object$page

  search_ipni(
    query=object$query,
    filters=object$filters,
    limit=object$limit,
    cursor=object$cursor,
    .wait=.wait
  )
}

calculate_wait_ <- function(object) {
  response_time <- object$response$times["total"]
  if (is.na(response_time)) {
    response_time <- 0.2
  }

  response_time / 2
}
