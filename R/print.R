# wcvp ----

#' @importFrom glue glue
#' @importFrom utils str head
#' @export
print.wcvp_search <- function(x, ...) {
  filters <- ifelse(is.null(x$filters), "none", x$filters)

  query <- glue("{names(x$query)}='{x$query}'")
  query <- glue_collapse(query, sep=", ")

  message <- glue("<WCVP search: {query} filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
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

#' @importFrom glue glue
#' @export
print.powo_search <- function(x, ...) {
  filters <- ifelse(is.null(x$filters), "none", x$filters)

  query <- glue("{names(x$query)}='{x$query}'")
  query <- glue_collapse(query, sep=", ")

  message <- glue("<POWO search: {query} filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
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
#' @export
print.powo_taxon <- function(x, ...) {
  if ("accepted" %in% names(x)) {
    accepted_id <- str_extract(x$accepted$fqId,
                               "(?<=names\\:)[0-9\\-]+$")
  } else if (x$taxonomicStatus == "Accepted") {
    accepted_id <- x$queryId
  } else {
    accepted_id <- "UNDEFINED"
  }

  has_distribution <- "distribution" %in% names(x)

  message <- glue("<POWO taxon id: {x$queryId}>",
                  "Name: {x$name}",
                  "Authors: {x$authors}",
                  "Status: {x$taxonomicStatus}",
                  "Rank: {x$rank}",
                  "Accepted taxon ID: {accepted_id}",
                  "Synonyms: {length(x$synonyms)}",
                  "Includes distribution: {has_distribution}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  invisible()
}

# ipni ----

#' @importFrom glue glue
#' @export
print.ipni_search <- function(x, ...) {
  filters <- ifelse(is.null(x$filters), "none", x$filters)

  query <- glue("{names(x$query)}='{x$query}'")
  query <- glue_collapse(query, sep=", ")

  message <- glue("<IPNI search: {query}, filters: '{filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
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
