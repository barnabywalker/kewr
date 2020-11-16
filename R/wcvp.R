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
#' There is some support for querying using keyword arguments. The API is
#' not currently documented, so only keywords that are definitely there have
#' been implemented. See arguments for all implemented keywords.
#'
#' The API will return taxonomic information (the family, authority, status, and rank)
#' of all names matching the query. These results can be limited, for example to accepted species,
#' using filters. See arguments for all implemented filters.
#'
#' @param query The taxon string to search WCVP for. If using keywords,
#'  the query must be formatted as a list and keywords must be one or more
#'  of `family`, `genus`, `species`.
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
#' # search for all names in a family
#' search_wcvp(list(family="Myrtaceae"))
#'
#' # search for genera within a family
#' search_wcvp(list(family="Myrtaceae"), filters=c("generic"))
#'
#' # search for all names with a specific epithet
#' search_wcvp(list(species="guianensis"))
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
#' @family WCVP functions
#' @seealso
#'  * [lookup_wcvp()] to lookup information about a taxon name
#'   using a valid IPNI ID.
#'  * [download_wcvp()] to download the entire WCVP.
#'
#' @export
search_wcvp <- function(query, filters=NULL, page=0, limit=50) {
  url <- wcvp_search_url_()

  query <- format_query_(query)
  # keeping a copy of this to return in the result object
  original_query <- query

  query$page <- page
  query$limit <- limit
  query$f <- format_filters_(filters)

  results <- make_request_(url, query)

  structure(
    list(
      total=results$content$total,
      page=results$content$page,
      limit=results$content$limit,
      results=results$content$results,
      query=original_query,
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
#' it can be found out using the [WCVP search API][kewr::search_wcvp].
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
#' @family WCVP functions
#' @seealso
#'  * [search_wcvp()] to search WCVP using a taxon name.
#'  * [download_wcvp()] to download the entire WCVP.
#'
#' @references
#' WCVP (2020). World Checklist of Vascular Plants, version 2.0. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://wcvp.science.kew.org/
#'
#' @export
lookup_wcvp <- function(taxonid) {
  url <- wcvp_taxon_url_(taxonid)

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

#' Download the whole of the WCVP.
#'
#' Download the latest or a specific version of the World
#' Checklist of Vascular Plants (WCVP).
#'
#' The [World Checklist of Vascular Plants (WCVP)](https://wcvp.science.kew.org/)
#' is a global consensus view of all known vascular plant species.
#' It has been compiled by staff at RBG Kew in consultation with plant
#' group experts.
#'
#' Versioned downloads of the whole WCVP are provided on the website.
#' This function allows the user to download the latest or a specific
#' version of the WCVP.
#'
#' @param save_dir A string specifying the folder to save the download in. If
#'   no value is provided, \link[here]{here} will be used.
#' @param version An integer version number to download. The latest
#'   version will be downloaded by default.
#'
#' @examples
#' \dontrun{
#'  # download the latest version
#'  download_wcvp()
#'
#'  # download version 1
#'  download_wcvp(version=1)
#' }
#'
#' @family WCVP functions
#' @seealso
#'  * [lookup_wcvp()] to lookup information about a taxon name
#'   using a valid IPNI ID.
#'  * [search_wcvp()] to search WCVP using a taxon name.
#'
#' @references
#' WCVP (2020). World Checklist of Vascular Plants, version 2.0. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://wcvp.science.kew.org/
#'
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom utils download.file
#'
#' @export
download_wcvp <- function(save_dir=NULL, version=NULL) {
  if (is.null(save_dir)) {
    save_dir <- here()
  }

  download_link <- wcvp_download_url_(version)
  filename <- str_extract(download_link, "(?<=/)wcvp.+\\.zip$")
  save_path <- file.path(save_dir, filename)

  if (is.null(version)) {
    version <- "latest"
  }

  message <- glue("Downloading WCVP version {version}",
                  "to: {save_path}\n",
                  .sep=" ", .trim=FALSE)

  cat(message)

  download.file(download_link, save_path)

  invisible()
}

# query format functions ----

#' Format query for WCVP search API.
#'
#' Checks if query is a keyword or string query,
#' and makes sure any keywords a valid.
#'
#' @param query A string or list specifying the query.
#'
#' @importFrom glue glue
#'
#' @noRd
format_query_ <- function(query) {

  if (! is.list(query) & length(query) > 1) {
    message <- glue("WCVP search query must be a named list or a string.",
                    "Provided query type: {typeof(query)}",
                    "Provided query length: {query_length}",
                    "",
                    .sep="\n", .trim=FALSE)

    stop(message, call.=FALSE)
  }

  if (is.list(query)) {
    valid_keywords <- get_keywords_("wcvp")
    bad_keywords <- setdiff(names(query), valid_keywords)

    if (length(bad_keywords) > 0) {
      stop(
        sprintf(
          "Query keywords must be one of [%s]\n[%s] are not recognised.",
          paste(valid_keywords, collapse=","),
          paste(bad_keywords, collapse=",")
        )
      )
    }
  }

  if(is.list(query)) {
    query
  } else {
    list(q=query)
  }
}

#' Format filters for WCVP search API.
#'
#' Checks the filters are valid before joining them
#' together with as a comma-separated string.
#'
#' @param filters A character vector of filter names.
#'
#' @noRd
format_filters_ <- function(filters) {
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

# object print methods ----

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

# object format methods ----

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

# URL utility functions ----

#' Make the WCVP taxon lookup URL.
#'
#' @param taxonid A valid IPNI ID.
#'
#' @noRd
#'
#' @importFrom glue glue
wcvp_taxon_url_ <- function(taxonid) {
  base <- get_url_("wcvp")

  glue("{base}/taxon/{taxonid}")
}

#' Make the WCVP search URL.
#'
#' @noRd
wcvp_search_url_ <- function() {
  base <- get_url_("wcvp")

  paste0(base, "/search")
}

#' Get a WCVP download URL.
#'
#' @importFrom httr GET
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_detect str_extract
#' @importFrom glue glue
#'
#' @noRd
wcvp_download_url_ <- function(version=NULL) {
  base <- "http://sftp.kew.org/pub/data-repositories/WCVP/"
  response <- GET(base)

  page <- content(response)
  link_nodes <- html_nodes(page, "a")
  links <- html_attr(link_nodes, "href")

  download_links <- links[str_detect(links, "\\.zip$")]
  versions <- str_extract(download_links, "(?<=_v)\\d+")

  if (is.null(version)) {
    version <- max(versions)
  }

  if (! version %in% versions) {
    message <- glue("Not a recognised version of WCVP: {version}",
                    "Available versions: {paste0(versions, collapse=',')}",
                    "",
                    .sep="\n", .trim=FALSE)

    stop(message, call.=FALSE)
  }

  download_link <- download_links[str_detect(download_links, paste0("_v", version))]
  paste0(base, download_link)
}


