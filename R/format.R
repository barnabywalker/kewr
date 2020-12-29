# wcvp ----
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble tibble
#' @export
format.wcvp_search <- function(x, synonyms=c("ignore", "simplify", "expand"), ...) {
  synonyms <- match.arg(synonyms)

  if (synonyms == "ignore") {
    fcn <- as_tibble
  } else if (synonyms == "simplify") {
    fcn <- function(r) {
      synonym_id <- r$synonymOf$id
      r$synonymOf <- NULL
      formatted <- as_tibble(r)
      formatted$synonymOf <- synonym_id

      formatted
    }
  } else if (synonyms == "expand") {
    fcn <- function(r) {
      synonym_col <- as_tibble(
        r$synonymOf,
        .name_repair=~paste0("synonymOf_", .x)
      )
      r$synonymOf <- NULL
      formatted <- as_tibble(r)
      bind_cols(r, synonym_col)
    }
  }

  map_dfr(x$results, fcn)
}

#' @importFrom purrr map_lgl map_dfr pluck
#' @importFrom tibble as_tibble tibble
#' @export
format.wcvp_taxon <- function(x, field=c("none", "accepted", "synonyms", "parent", "children", "hierarchy"), ...) {
  field <- match.arg(field)
  if (field == "none") {
    x$response <- NULL
    x$queryId <- NULL

    list_field <- map_lgl(x, is.list)
    x <- x[! list_field]

    null_field <- map_lgl(x, is.null)
    x[null_field] <- NA_character_

    as_tibble(x)
  } else if (field %in% c("parent", "accepted")) {
    x <- pluck(x, field)

    as_tibble(x)
  } else {
    x <- pluck(x, field)

    map_dfr(x, as_tibble)
  }
}

# powo ----

#' @importFrom purrr map_dfr
#'
#' @export
format.powo_search <- function(x, ...) {
  map_dfr(x$results, as_tibble)
}

#' @importFrom purrr map_lgl map_dfr pluck
#' @importFrom tibble as_tibble tibble
#' @export
format.powo_taxon <- function(x, field=c("none", "accepted", "synonyms", "classification", "basionym", "distribution", "distributionEnvelope"), ...) {
  field <- match.arg(field)
  if (field == "none") {
    x$response <- NULL
    x$queryId <- NULL

    list_field <- map_lgl(x, is.list)
    x <- x[! list_field]

    null_field <- map_lgl(x, is.null)
    x[null_field] <- NA_character_

    as_tibble(x)
  } else if (field %in% c("accepted")) {
    x <- pluck(x, field)

    as_tibble(x)
  } else if (field %in% c("distribution")) {
    x <- pluck(x, field)


    map_dfr(x, ~map_dfr(.x, as_tibble))
  } else {
    x <- pluck(x, field)

    if (field == "distributionEnvelope") {
      warning("Unknown CRS, use with caution...")
    }

    map_dfr(x, as_tibble)
  }
}

