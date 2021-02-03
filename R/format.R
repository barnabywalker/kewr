# wcvp ----
#' @importFrom purrr map_dfr
#'
#' @export
format.wcvp_search <- function(x, ...) {
  .Deprecated("tidy.wcvp_search")
  map_dfr(x$results, parse_nested_list)
}

#' @export
format.wcvp_taxon <- function(x, ...) {
  .Deprecated("tidy.wcvp_taxon")
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list(x)
}

# powo ----

#' @importFrom purrr map_dfr
#'
#' @export
format.powo_search <- function(x, ...) {
  .Deprecated("tidy.powo_search")
  map_dfr(x$results, parse_nested_list)
}

#' @export
format.powo_taxon <- function(x, field=c("none", "accepted", "synonyms", "classification", "basionym", "distribution", "distributionEnvelope"), ...) {
  .Deprecated("tidy.powo_taxon")
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list(x)
}

# ipni ----

#' @importFrom purrr map_dfr
#'
#' @export
format.ipni_search <- function(x, ...) {
  .Deprecated("tidy.ipni_search")
  map_dfr(x$results, parse_nested_list)
}

#' @export
format.ipni_citation <- function(x, ...) {
  .Deprecated("tidy.ipni_citation")
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list(x)
}

#' @export
format.ipni_author <- function(x, ...) {
  .Deprecated("tidy.ipni_author")
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list(x)
}

#' @export
format.ipni_publication <- function(x, ...) {
  .Deprecated("tidy.ipni_publication")
  x <- x[! names(x) %in% c("response", "queryId")]

  parse_nested_list(x)
}

# knms ----

#' @importFrom purrr map_lgl map_dfr pluck
#' @importFrom tidyr fill
#' @importFrom rlang .data
#'
#' @export
format.knms_match <- function(x, ...) {
  .Deprecated("tidy.knms_match")
  parsed <- map_dfr(x$results, parse_knms_line)

  formatted <- fill(parsed, .data$submitted, .data$matched)
  formatted$matched <- formatted$matched %in% c("true", "multiple_matches")

  formatted
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
parse_nested_list <- function(l) {
  if (is.null(names(l))) {
    return(map_dfr(l, parse_nested_list))
  }

  null_cols <- map_lgl(l, is.null)
  l[null_cols] <- NA_character_

  list_cols <- map_lgl(l, is.list)
  l[list_cols] <- map(l[list_cols], ~list(parse_nested_list(.x)))

  as_tibble_row(l)
}

#' Parse and format a single match result from KNMS.
#'
#' @importFrom stringr str_extract
#' @importFrom dplyr na_if
#' @importFrom tibble tibble
#'
#' @noRd
parse_knms_line <- function(line) {
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
