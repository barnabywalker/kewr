#' Get valid filters for a resource.
#'
#' @param resource The resource being queried.
#'
#' @return A character vector of valid filters.
#'
#' @examples
#' get_filters("wcvp")
#' get_filters("powo")
#' get_filters("ipni")
#'
#' @export
get_filters <- function(resource=c("wcvp", "powo", "ipni")) {
  resource <- match.arg(resource)

  filters <- get_filters_(resource)
  names(filters)
}

#' Get valid keywords for a resource.
#'
#' @param resource The resource being queried.
#'
#' @return A character vector of valid keywords.
#'
#' @examples
#' get_keywords("wcvp")
#' get_keywords("powo")
#' get_keywords("ipni")
#'
#' @export
get_keywords <- function(resource=c("wcvp", "powo", "ipni")) {
  resource <- match.arg(resource)

  keywords <- get_keywords_(resource)
  names(keywords)
}
