#' Match names with KNMS.
#'
#' Use the Kew Names Matching Service to match taxon names to
#' records in Plants of the World Online.
#'
#' The [Kew Names Matching Service (KNMS)](http://namematch.science.kew.org/) allows
#' a user to submit taxon names for matching against records in
#' [Plants of the World Online (POWO)](http://powo.science.kew.org/).
#' As far as I can tell, it uses exact matching as well as some rules-based matching
#' to account for common orthographic variants and Latin mistakes.
#'
#' Names can be submitted to KNMS with or without an author string.
#' If a name can match to multiple different records, for instance
#' with synonyms, KNMS will return multiple matches. As such, we recommend
#' submitting names first with the taxonomic authority and then without
#' if no match can be found.
#'
#' KNMS allows multiple names to be submitted at once. However, it can
#' be slow in returning results if too many names are submitted. For lots of names,
#' [the website provides an interface for submitting a CSV file](http://namematch.science.kew.org/csv).
#'
#' @param names A list or character vector of taxon names for matching.
#'
#' @return A `knms_match` object - a simple structure containing the match
#'   results and some statistics about the number of matches.
#'
#' @examples
#'
#' # match a name
#' match_knms("Poa annua L.")
#'
#' # match a vector of names
#' names <- c("Myrcia guianensis", "Calyptranthes ranulphii", "Poa annua")
#' match_knms(names)
#'
#' # tidy match results into a table
#' names <- c("Myrcia guianensis", "Bad plant", "Poa annua")
#' matches <- match_knms(names)
#' tidy(matches)
#'
#' @export
match_knms <- function(names) {
  url <- knms_url_()

  body <- format_body_(names)

  results <- make_request_(url, body=body)

  structure(
    list(
      response=results$response,
      submitted=body,
      matched=results$content$stats$matched,
      unmatched=results$content$stats$unmatched,
      multiple_matches=results$content$stats$multipleMatches,
      results=results$content$records
    ),
    class="knms_match"
  )
}

#' Make the KNMS URL.
#'
#' @noRd
knms_url_ <- function() {
  get_url_("knms")
}
