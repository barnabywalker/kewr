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
#' or more of `accepted`, `families`, `genera`, `species`, `infraspecies`.
#' Multiple filters must be supplied as a character vector.
#' @param page An integer specifying the page of results to return.
#' @param limit An integer specifying the maximum number of results
#'  to return.
#' @return Returns an object of class `wcvp_search` that is a simple
#' structure with slots for:
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
#' search_wcvp("Myrcia", filters=c("species", "accepted"))
#'
#' # search for up to 10,000 species in a genus
#' search_wcvp("Poa", filters=c("species"), limit=10000)
#'
#' # search for all names in a family
#' search_wcvp(list(family="Myrtaceae"))
#'
#' # search for genera within a family
#' search_wcvp(list(family="Myrtaceae"), filters=c("genera"))
#'
#' # search for all names with a specific epithet
#' search_wcvp(list(species="guianensis"))
#'
#' # search for a species name and print the results
#' r <- search_wcvp("Myrcia guianensis", filters=c("species"))
#' print(r)
#'
#' # simplify search results to a `tibble`
#' r <- search_wcvp("Poa", filters=c("species"))
#' format(r)
#'
#' # accepted name info is nested inside the records for synonyms
#' # simplify accepted name info to the name ID
#' r <- search_wcvp("Poa", filters=c("species"))
#' formatted <- format(r)
#' tidyr::unnest(formatted, cols=synonymOf, names_sep="_")
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
search_wcvp <- function(query, filters=NULL, page=1, limit=50) {
  url <- wcvp_search_url_()

  # keeping a copy of this to return in the result object
  original_query <- query

  query <- format_query_(query, "wcvp")

  query$page <- page
  query$limit <- limit

  query$f <- format_filters_(filters, "wcvp")

  results <- make_request_(url, query)

  # calculate total number of pages, because it isn't returned
  total_pages <- ceiling(results$content$total / results$content$limit)

  structure(
    list(
      total=results$content$total,
      pages=total_pages,
      page=results$content$page,
      limit=results$content$limit,
      results=results$content$results,
      query=original_query,
      filters=filters,
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
#' # format into a tibble
#' r <- lookup_wcvp("271445-2")
#' format(r)
#'
#' # format the returned list of synonyms into a tibble
#' r <- lookup_wcvp("60447743-2")
#' formatted <- format(r)
#' tidyr::unnest(formatted, cols=synonyms, names_sep="_")
#'
#' # expand the child entries returned for each entry
#' r <- lookup_wcvp("30000055-2")
#' formatted <- format(r)
#' tidyr::unnest(formatted, cols=children, names_sep="_")
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

  # fill in status if unplaced
  status <- record$status
  record$status <- ifelse(is.null(status), "unplaced", status)

  # make sure author string is not null
  authors <- record$authors
  record$authors <- ifelse(is.null(authors), NA_character_, authors)

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


