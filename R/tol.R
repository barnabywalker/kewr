#' Search Kew's Tree of Life for specimens or genes.
#' 
#' Query Kew's Tree of Life for specimens that have
#' been sampled for sequencing.
#' 
#' The [Tree of Life](https://treeoflife.kew.org/) is a database
#' of specimens sequenced as part of Kew's efforts to build
#' a comprehensive evolutionary tree of life for flowering plants.
#' 
#' The search API allows users to query the database for specimens
#' based on their taxonomic information. Filtering and keyword-search
#' are not currently implemented. All searches are based on taxonomic 
#' information, so `Myrcia` and `Myrtales` will return results, but 
#' `Brummitt` will not.
#' 
#' The search API also allows users to download information about sequenced
#' genes. There is currently no ability to search within the results for genes,
#' but a table of all genes can be accessed using keyword argument `genes=TRUE`.
#' 
#' @param query The string to query the database with.
#' @param genes Set to TRUE to download results for genes instead of specimens.
#' @param limit An integer specifying the number of results 
#'  to return.
#' @param page An integer specify the page of results to request.
#' @param .wait Time to wait before making a requests, to help
#'  rate limiting.
#' 
#' @return Returns an object of class `tol_search` that is a simple
#'  structure with slots for:
#'  
#'  * `total`: the total number of results held in ToL for the query.
#'  * `page`: the page of results requested.
#'  * `limit`: the maximum number of results requested from the API.
#'  * `results`: the query results parsed into a list.
#'  * `query`: the query string submitted to the API.
#'  * `response`: the [httr response object][httr::response].
#' 
#' @examples
#' # get the first 50 of all sequenced specimens
#' search_tol(limit=50)
#' 
#' # search for all sequenced Myrcia specimens
#' search_tol("Myrcia")
#' 
#' # get all sequenced specimens
#' search_tol(limit=5000)
#' 
#' # search for a species name and print the results
#' r <- search_tol("Myrcia guianensis")
#' print(r)
#' 
#' # simplify search results to a `tibble`
#' r <- search_tol("Myrcia")
#' tidy(r)
#'
#' # gene stats are nested in the results
#' r <- search_tol("Myrcia")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=gene_stats)
#' 
#' # species names are nested in the results
#' r <- search_tol("Myrcia")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=species, names_sep="_")
#' 
#' # as is higher taxonomy
#' r <- search_tol("Myrcia")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=species, names_sep="_")
#' 
#' # search for all gene entries and print results
#' r <- search_tol(genes=TRUE, limit=500)
#' print(r)
#' 
#' # tidy the returned genes
#' tidy(r)
#' 
#' @references
#' Baker W.J., Bailey P., Barber V., Barker A., Bellot S., Bishop D., Botigue L.R., Brewer G., Carruthers T., Clarkson J.J., Cook J., Cowan R.S., Dodsworth S., Epitawalage N., Francoso E., Gallego B., Johnson M., Kim J.T., Leempoel K., Maurin O., McGinnie C., Pokorny L., Roy S., Stone M., Toledo E., Wickett N.J., Zuntini A.R., Eiserhardt W.L., Kersey P.J., Leitch I.J. & Forest F. 2021. A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life. Systematic Biology, 2021; syab035, https://doi.org/10.1093/sysbio/syab035
#' 
#' @family ToL functions
#'  * [lookup_tol()] to lookup information about a sequenced specimen
#'    using a valid ToL ID.
#'  * [download_tol()] to download a file from the ToL SFTP server.
#' 
#' @export
search_tol <- function(query="", genes=FALSE, limit=50, page=1, .wait=0.2) {
  if (genes) {
    url <- tol_search_url_(type="genes")
    query <- "genes"
  } else {
    url <- tol_search_url_()
  }

  original_query <- query
  query <- format_query_(query, "tol")

  query$per_page <- limit
  query$page <- page
  results <- make_request_(url, query, .wait=.wait)

  # calculate total number of pages, because it isn't returned
  total_pages <- ceiling(results$content$total / limit)

  structure(
    list(
      total=results$content$total,
      pages=total_pages,
      page=results$content$page,
      limit=limit,
      results=results$content$items,
      query=original_query,
      response=results$response
    ),
    class="tol_search"
  )
}

#' Look up a sequenced specimen or gene in ToL.
#'
#' Request the record for a sequenced specimen or gene in ToL using
#' its ToL ID.
#'
#' The [Tree of Life](https://treeoflife.kew.org/) is a database
#' of specimens sequenced as part of Kew's efforts to build
#' a comprehensive evolutionary tree of life for flowering plants.
#'
#' The lookup API allows users to retrieve taxonomic and sequencing
#' information for a specific sequenced specimen or gene using the unique ToL ID.
#' If this is not known, it can be found out using the [ToL search API][kewr::search_tol].
#'
#' @param id A string containing a valid ToL ID.
#' @param type The type of record to lookup, either `specimen` or `gene`.
#' @param .wait Time to wait before making a request, to help
#'  rate limiting.
#'
#' @return A `tol_{type}` object, which is a simple structure with fields
#'   for each of the fields returned by the lookup API, 
#'   as well as the the [httr response object][httr::response].
#'
#' @examples
#'
#' # retrieve information for a particular specimen
#' lookup_tol("1296")
#'
#' # print a summary of the returned information
#' r <- lookup_tol("1296")
#' print(r)
#'
#' # tidy into a tibble
#' r <- lookup_tol("1296")
#' tidy(r)
#'
#' # extract the returned gene stats for the specimen
#' r <- lookup_tol("1296")
#' tidied <- tidy(r)
#' tidied$gene_stats
#'
#' # expand the taxonomy info
#' r <- lookup_tol("1296")
#' tidied <- tidy(r)
#' tidyr::unnest(tidied, cols=taxonomy, names_sep="_")
#' 
#' # retrieve information for a particular gene
#' lookup_tol("51", type="gene")
#' 
#' # print a summary of the returned information
#' r <- lookup_tol("51", type="gene")
#' print(r)
#'
#' # tidy into a tibble
#' r <- lookup_tol("51", type="gene")
#' tidy(r)
#'
#' @family ToL functions
#' @seealso
#'  * [search_tol()] to search ToL using taxonomic information.
#'  * [download_tol()] to download a file from the ToL SFTP server
#'
#' @references
#' Baker W.J., Bailey P., Barber V., Barker A., Bellot S., Bishop D., Botigue L.R., Brewer G., Carruthers T., Clarkson J.J., Cook J., Cowan R.S., Dodsworth S., Epitawalage N., Francoso E., Gallego B., Johnson M., Kim J.T., Leempoel K., Maurin O., McGinnie C., Pokorny L., Roy S., Stone M., Toledo E., Wickett N.J., Zuntini A.R., Eiserhardt W.L., Kersey P.J., Leitch I.J. & Forest F. 2021. A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life. Systematic Biology, 2021; syab035, https://doi.org/10.1093/sysbio/syab035
#'
#' @export
lookup_tol <- function(id, type=c("specimen", "gene"), .wait=0.1) {
  type <- match.arg(type)
  url <- tol_lookup_url_(id, type)

  result <- make_request_(url, query=NULL, .wait=.wait)

  # this might be better if things were explicitly listed
  record <- result$content
  record$response <- result$response
  record$queryId <- id

  structure(
    record,
    class=paste0("tol_", type)
  )
}

#' Download a file from the ToL SFTP server.
#'
#' Download an alignment, sequence, or tree file from the ToL
#' SFTP server.
#'
#' The [Tree of Life](https://treeoflife.kew.org/) is a database
#' of specimens sequenced as part of Kew's efforts to build
#' a comprehensive evolutionary tree of life for flowering plants.
#'
#' Sequence, alignment, and Newick tree files are help on an SFTP server
#' for download. The URLs to access these are stored in entries for specimens
#' and genes in the ToL database. These can be accessed by either using [search_tol()]
#' to get all specimens for a particular order, family, genus, or species or by
#' looking up a specific specimen or gene using [lookup_tol()]
#' 
#' @param download_link A string specifying the URL to download the file from.
#'  You can get a download URL for a particular specimen or gene using [lookup_tol()].
#' @param save_dir A string specifying the folder to save the download in. If
#'   no value is provided, \link[here]{here} will be used.
#'
#' @examples
#' \dontrun{
#'  # download a specimen fasta file
#'  specimen_info <- lookup_tol("1296")
#'  download_tol(specimen_info$fasta_file_url)
#' 
#'  # download a gene alignment file
#'  gene_info <- lookup_tol("51", type="gene")
#'  download_tol(gene_info$alignment_file_url)
#' 
#'  # download the gene tree
#'  download_tol(gene_info$tree_file_url)
#' }
#'
#' @family ToL functions
#' @seealso
#'  * [lookup_tol()] to lookup information about a sequenced specimen
#'   using a valid ToL ID.
#'  * [search_tol()] to search ToL using taxonomic info.
#'
#' @references
#' Baker W.J., Bailey P., Barber V., Barker A., Bellot S., Bishop D., Botigue L.R., Brewer G., Carruthers T., Clarkson J.J., Cook J., Cowan R.S., Dodsworth S., Epitawalage N., Francoso E., Gallego B., Johnson M., Kim J.T., Leempoel K., Maurin O., McGinnie C., Pokorny L., Roy S., Stone M., Toledo E., Wickett N.J., Zuntini A.R., Eiserhardt W.L., Kersey P.J., Leitch I.J. & Forest F. 2021. A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life. Systematic Biology, 2021; syab035, https://doi.org/10.1093/sysbio/syab035
#'
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom utils download.file
#'
#' @export
download_tol <- function(download_link, save_dir=NULL) {
  if (is.null(save_dir)) {
    save_dir <- here()
  }

  filename <- str_extract(download_link, "(?<=/)[^/]+$")
  save_path <- file.path(save_dir, filename)

  message <- glue("Downloading file {filename}",
                  "to: {save_path}\n",
                  .sep=" ", .trim=FALSE)

  cat(message)

  download.file(download_link, save_path)

  invisible()
}

#' Make the ToL lookup URL.
#'
#' @param id A valid ToL ID.
#'
#' @noRd
#'
#' @importFrom glue glue
tol_lookup_url_ <- function(id, type=c("specimen", "gene")) {
  type <- match.arg(type)
  base <- get_url_("tol")

  glue("{base}/{type}s/{id}")
}

#' Make Tree of Life search URL.
#' 
#' @importFrom glue glue
#' 
#' @noRd
tol_search_url_ <- function(type=c("specimens", "genes")) {
  type <- match.arg(type)
  base <- get_url_("tol")

  glue("{base}/{type}")
}