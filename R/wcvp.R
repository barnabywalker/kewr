#' Search WCVP for a taxon.
#'
#' Query the World Checklist of Vascular Plants search API
#' for a taxon string.
#'
#' The [World Checklist of Vascular Plants (WCVP)](https://wcvp.science.kew.org/)
#' is a global consensus view of all known vascular plant species.
#' It has been compiled by staff at RBG Kew in consultation with plant
#' group experts.
#'
#' The search API allows users to query the checklist for plant names.
#' Currently, it does not support partial or fuzzy matching.
#' In order to get a result, the user must supply a valid name string.
#' For example, 'Myrcia' and 'Myrcia guianensis' will return results,
#' but 'M' or 'Myr' will not.
#'
#' The API will return taxonomic information (the family, authority, status, and rank)
#' of all names matching the query. These results can be limited, for example to accepted species,
#' using filters.
#'
#' @param query The taxon string to search WCVP for.
#' @param filters Filter to apply to search results. Can be one
#' or more of `accepted`, `generic`, `specific`, `intraspecific`.
#' Multiple filters must be supplied as a character vector.
#' @param page An integer specifying the page of results to return.
#' @param limit An integer specifying the maximum number of results
#'  to return.
#' @return Returns an object of class `wcvp_search` that is a simple
#' stucture with slots for:
#'
#'  * `total`: the total number of results held in WCVP for the query
#'  * `page`: the page number requested from the API.
#'  * `limit`: the maximum number of results requested from the API.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `filter`: the filter strings submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#'
#' @examples
#' # search for all entries containing a genus name
#' search_wcvp("Myrcia")
#'
#' # search for all accepted species within a genus
#' search_wcvp("Myrcia", filters=c("specific", "accepted"))
#'
#' # search for up to 10,000 species in a genus
#' search_wcvp("Poa", filters=c("specific"), limit=10000)
#'
#' # search for a species name and print the results
#' r <- search_wcvp("Myrcia guianensis", filters=c("specific"))
#' print(r)
#'
#' # simplify search results to a `tibble`
#' r <- search_wcvp("Poa", filters=c("specific"))
#' format(r)
#'
#' # accepted name info is nested inside the records for synonyms
#' # simplify accepted name info to the name ID
#' r <- search_wcvp("Poa", filters=c("specific"))
#' format(r, synonyms="simplify")
#'
#' # expand accepted name info
#' r <- search_wcvp("Poa", filters=c("specific"))
#' format(r, synonyms="expand")
#'
#' @references
#' WCVP (2020). World Checklist of Vascular Plants, version 2.0. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://wcvp.science.kew.org/
#'
#' @seealso [lookup_wcvp()] to lookup information about a taxon name
#'   using a valid IPNI ID.
#'
#' @export
search_wcvp <- function(query, filters=NULL, page=0, limit=50) {
  url <- wcvp_search_url()

  query <- list(q=query, page=page, limit=limit)
  query$f <- format_filters(filters)

  results <- make_request_(url, query)

  structure(
    list(
      total=results$content$total,
      page=results$content$page,
      limit=results$content$limit,
      results=results$content$results,
      query=query$q,
      filters=query$f,
      response=results$response
    ),
    class="wcvp_search"
  )
}

#' Look up a taxon in WCVP.
#'
#' Request the record for a taxon in the World Checklist of
#' Vascular Plants (WCVP) using the IPNI ID.
#'
#' The [World Checklist of Vascular Plants (WCVP)](https://wcvp.science.kew.org/)
#' is a global consensus view of all known vascular plant species.
#' It has been compiled by staff at RBG Kew in consultation with plant
#' group experts.
#'
#' The taxon lookup API allows users to retrieve taxonomic information for
#' a specific taxon name using the unique IPNI ID. If this is not known,
#' it can be found out using the [WCVP search API](kewr::search_wcvp).
#'
#' @param taxonid A string containing a valid IPNI ID.
#'
#' @return A `wcvp_taxon` object, which is a simple structure with fields
#'   for each of the fields returned by the lookup API, as well as the the [httr response object][httr::response].
#'
#' @examples
#'
#' # retrieve taxonomic information for a taxon name
#' lookup_wcvp("271445-2")
#'
#' # print a summary of the returned information
#' r <- lookup_wcvp("271445-2")
#' print(r)
#'
#' # format the top-level information into a tibble
#' r <- lookup_wcvp("271445-2")
#' format(r)
#'
#' # format the returned list of synonyms into a tibble
#' r <- lookup_wcvp("60447743-2")
#' format(r, field="synonyms")
#'
#' # format the returned list of children into a tibble
#' r <- lookup_wcvp("30000055-2")
#' format(r, field="children")
#'
#' @seealso [search_wcvp()] to search WCVP using a taxon name.
#'
#' @references
#' WCVP (2020). World Checklist of Vascular Plants, version 2.0. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://wcvp.science.kew.org/
#'
#' @export
lookup_wcvp <- function(taxonid) {
  url <- wcvp_taxon_url(taxonid)

  result <- make_request_(url, query=NULL)

  # this might be better if things were explicitly listed
  record <- result$content
  record$response <- result$response
  record$queryId <- taxonid

  structure(
    record,
    class="wcvp_taxon"
  )
}

#' Format filters for WCVP search API.
#'
#' Checks the filters are valid before joining them
#' together with as a comma-separated string.
#'
#' @param filters A character vector of filter names.
#'
#' @noRd
format_filters <- function(filters) {
  if (is.null(filters)) {
    return(NULL)
  }

  valid_filters <- get_filters_("wcvp")
  bad_filters <- setdiff(filters, valid_filters)
  if (length(bad_filters) > 0) {
    stop(
      sprintf(
        "Filters must be one of [%s]\n[%s] are not recognised.",
        paste(valid_filters, collapse=","),
        paste(bad_filters, collapse=",")
      )
    )
  }

  paste(filters, collapse=",")
}

#' @importFrom glue glue
#' @importFrom utils str head
#' @export
print.wcvp_search <- function(x, ...) {
  filters <- ifelse(is.null(x$filters), "none", x$filters)
  message <- glue("<WCVP search: '{x$query}' filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  str(head(x$results, 1))
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.wcvp_taxon <- function(x, ...) {
  accepted_id <- ifelse(is.null(x$accepted), x$id, x$accepted$id)

  message <- glue("<WCVP taxon id: {x$queryId}>",
                  "Name: {x$name}",
                  "Authors: {x$authors}",
                  "Status: {x$status}",
                  "Rank: {x$rank}",
                  "Accepted taxon ID: {accepted_id}",
                  "Synonyms: {length(x$synonyms)}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

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

#' Make the WCVP taxon lookup URL.
#'
#' @param taxonid A valid IPNI ID.
#'
#' @noRd
#'
#' @importFrom glue glue
wcvp_taxon_url <- function(taxonid) {
  base <- get_url_("wcvp")

  glue("{base}/taxon/{taxonid}")
}

#' Make the WCVP search URL.
#'
#' @noRd
wcvp_search_url <- function() {
  base <- get_url_("wcvp")

  paste0(base, "/search")
}
