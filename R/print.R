# wcvp ----

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

# powo ----

#' @importFrom glue glue
#' @export
print.powo_search <- function(x, ...) {
  message <- glue("<POWO search: '{x$query}' filters: '{x$filters}'>",
                  "total results: {x$total}",
                  "returned results: {length(x$results)}",
                  "",
                  .sep="\n", .trim=FALSE)

  cat(message)
  str(head(x$results, 1))
  invisible()
}