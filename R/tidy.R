# wcvp ----
#' @importFrom purrr map_dfr
#'
#' @export
tidy.wcvp_search <- function(x, ...) {
  map_dfr(x$results, parse_nested_list_)
}

#' @export
tidy.wcvp_taxon <- function(x, ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

# powo ----

#' @importFrom purrr map_dfr
#'
#' @export
tidy.powo_search <- function(x, ...) {
  map_dfr(x$results, parse_nested_list_)
}

#' @export
tidy.powo_taxon <- function(x, field=c("none", "accepted", "synonyms", "classification", "basionym", "distribution", "distributionEnvelope"), ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

# ipni ----

#' @importFrom purrr map_dfr
#'
#' @export
tidy.ipni_search <- function(x, ...) {
  map_dfr(x$results, parse_nested_list_)
}

#' @export
tidy.ipni_citation <- function(x, ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

#' @export
tidy.ipni_author <- function(x, ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

#' @export
tidy.ipni_publication <- function(x, ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

# tol ----
#' @importFrom purrr map_dfr
#'
#' @export
tidy.tol_search <- function(x, ...) {
  map_dfr(x$results, parse_nested_list_)
}

#' @export
tidy.tol_specimen <- function(x, ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

#' @export
tidy.tol_gene <- function(x, ...) {
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list_(x)
}

# knms ----

#' @importFrom purrr map_lgl map_dfr pluck
#' @importFrom tidyr fill
#' @importFrom rlang .data
#'
#' @export
tidy.knms_match <- function(x, ...) {
  parsed <- map_dfr(x$results, parse_knms_line_)

  tidied <- fill(parsed, .data$submitted, .data$matched)
  tidied$matched <- tidied$matched %in% c("true", "multiple_matches")

  tidied
}

# krs ----

#' @importFrom purrr map_dfr
#'
#' @export
tidy.krs_match <- function(x, ...) {
  map_dfr(x$results, parse_nested_list_)
}

# utils ----

#' Simple utility to wrap nested lists into a tibble.
#'
#' Nested lists are also converted to tibbles and inserted in list
#' columns.
#'
#' @importFrom purrr map_chr map
#' @importFrom tibble as_tibble_row
#'
#' @noRd
parse_nested_list_ <- function(l) {
  if (is.null(names(l))) {
    return(map_dfr(l, parse_nested_list_))
  }

  null_cols <- map_lgl(l, is.null)
  l[null_cols] <- NA

  list_cols <- map_lgl(l, is.list)
  l[list_cols] <- map(l[list_cols], ~list(parse_nested_list_(.x)))

  as_tibble_row(l)
}

#' Parse a single match result from KNMS.
#'
#' @importFrom stringr str_extract
#' @importFrom dplyr na_if
#' @importFrom tibble tibble
#'
#' @noRd
parse_knms_line_ <- function(line) {
  submitted <- na_if(line[[1]], "")
  matched <- na_if(line[[2]], "")

  if (length(line) > 2) {
    ipni_id <- str_extract(line[[3]], "(?<=names:)[0-9\\-]+$")
  } else {
    ipni_id <- NA_character_
  }

  if (length(line) > 3) {
    matched_record <- line[[4]]
  } else {
    matched_record <- NA_character_
  }

  tibble(submitted=submitted,
         matched=matched,
         ipni_id=ipni_id,
         matched_record=matched_record)
}
