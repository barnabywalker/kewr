# wcvp ----

#' @importFrom glue glue glue_collapse
#' @importFrom utils str head
#'
#' @export
print.wcvp_search <- function(x, ...) {
  if (! is.null(x$filters)) {
    filters <- glue_collapse(x$filters, sep=", ")
  } else {
    filters <- "none"
  }

  if (! is.null(names(x$query))) {
    query <- glue("{names(x$query)}='{x$query}'")
    query <- glue_collapse(query, sep=", ")
  } else {
    query <- glue("'{x$query}'")
  }

  message <- glue("<WCVP search: {query} filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "total pages: {x$pages}",
                  "current page: {x$page}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  if (! is.null(x$results)) {
    str(head(x$results, 1), max.level=2)
  }
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

# powo ----

#' @importFrom glue glue glue_collapse
#' @importFrom utils str head
#'
#' @export
print.powo_search <- function(x, ...) {
  if (! is.null(x$filters)) {
    filters <- glue_collapse(x$filters, sep=", ")
  } else {
    filters <- "none"
  }

  if (! is.null(names(x$query))) {
    query <- glue("{names(x$query)}='{x$query}'")
    query <- glue_collapse(query, sep=", ")
  } else {
    query <- glue("'{x$query}'")
  }

  message <- glue("<POWO search: {query} filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "total pages: {x$pages}",
                  "First result:",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  if (! is.null(x$results)) {
    str(head(x$results, 1), max.level=2)
  }
  invisible()
}

#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom utils str
#'
#' @export
print.powo_taxon <- function(x, ...) {
  has_dist <- length(x$distributions) > 0
  message <- glue("<POWO taxon id: {x$queryId}>",
                  "Name: {x$scientificName}",
                  "Authors: {x$author}",
                  "Has distribution: {has_dist}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

# ipni ----

#' @importFrom glue glue glue_collapse
#' @importFrom utils str head
#'
#' @export
print.ipni_search <- function(x, ...) {
  if (! is.null(x$filters)) {
    filters <- glue_collapse(x$filters, sep=", ")
  } else {
    filters <- "none"
  }

  if (! is.null(names(x$query))) {
    query <- glue("{names(x$query)}='{x$query}'")
    query <- glue_collapse(query, sep=", ")
  } else {
    query <- glue("'{x$query}'")
  }

  message <- glue("<IPNI search: {query}, filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "total pages: {x$pages}",
                  "current page: {x$page}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  if (! is.null(x$results)) {
    str(head(x$results, 1), max.level=2)
  }
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.ipni_citation <- function(x, ...) {
  message <- glue("<IPNI name id: {x$queryId}, type: {x$recordType}>",
                  "Name: {x$name}",
                  "Authors: {x$authors}",
                  "Publication: {x$publication}",
                  "Publication Year: {x$publicationYear}",
                  "Reference: {x$reference}",
                  "Rank: {x$rank}",
                  "In POWO: {x$inPowo}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.ipni_author <- function(x, ...) {
  message <- glue("<IPNI name id: {x$queryId}, type: {x$recordType}>",
                  "Name: {x$forename} {x$surname}",
                  "Standard form: {x$standardForm}",
                  "Dates: {x$dates}",
                  "Focal groups: {x$taxonGroups}",
                  "Example taxon: {x$examples}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.ipni_publication <- function(x, ...) {
  message <- glue("<IPNI name id: {x$queryId}, type: {x$recordType}>",
                  "Title: {x$title}",
                  "Abbreviation: {x$abbreviation}",
                  "LC Number: {ifelse(is.null(x$lcNumber), '', x$lcNumber)}",
                  "BPH Number: {x$bphNumber}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

# tol -----

#' @importFrom glue glue
#' @importFrom utils str head
#'
#' @export
print.tol_search <- function(x, ...) {
  if (! is.null(names(x$query))) {
    query <- glue("{names(x$query)}='{x$query}'")
    query <- glue_collapse(query, sep=", ")
  } else {
    query <- glue("'{x$query}'")
  }

  message <- glue("<ToL search: {query}>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "total pages: {x$pages}",
                  "current page: {x$page}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  if (! is.null(x$results)) {
    str(head(x$results, 1), max.level=2)
  }
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.tol_specimen <- function(x, ...) {

  raw_reads <- x$raw_reads[[1]]
  taxonomy <- x$taxonomy

  message <- glue("<ToL specimen id: {x$queryId}>",
                  "Species: {taxonomy$species}",
                  "Family: {taxonomy$family}",
                  "Order: {taxonomy$order}",
                  "Collector: {x$collector}",
                  "Project: {x$project$data_source$name}",
                  "No. of reads: {format(raw_reads$reads_count, big.mark=',')}",
                  "Sequencing platform: {raw_reads$sequence_platform}",
                  "Suspicious placement: {x$is_suspicious_placement}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.tol_gene <- function(x, ...) {

  raw_reads <- x$raw_reads[[1]]
  taxonomy <- x$taxonomy

  message <- glue("<ToL gene id: {x$queryId}>",
                  "Exemplar name: {x$exemplar_name}",
                  "Exemplar source species: {x$exemplar_species}",
                  "No. species: {x$species_count}",
                  "No. genera: {x$genera_count}",
                  "Avg. recovered length: {x$average_contig_length}",
                  "Avg. % recovered: {x$average_contig_length_percent}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.tol_tree <- function(x, ...) {

  message <- glue("<ToL tree url: {x$response$url}>",
                  "Preview:",
                  substr(x$content, 1, 100),
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

#' @importFrom glue glue
#' @importFrom utils str
#' @export
print.tol_fasta <- function(x, ...) {

  message <- glue("<ToL fasta url: {x$response$url}>",
                  "Preview:",
                  substr(x$content, 1, 100),
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

# knms ----

#' @importFrom glue glue
#' @export
print.knms_match <- function(x, ...) {
  message <- glue("<KNMS match: {length(x$submitted)} names submitted>",
                  "Matches returned: {x$matched}",
                  "Multiple matches: {x$multiple_matches}",
                  "Unmatched names: {x$unmatched}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  str(head(x$results, 1))
  invisible()
}
