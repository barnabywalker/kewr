#' Get citation for Kew data resource.
#'
#' Given the result of a query to a Kew data resource, get the appropriate
#' citation.
#'
#' @param x Result of a call to [search_powo()], [lookup_powo()], [search_wcvp()],
#'  [lookup_wcvp()], [search_ipni()], [lookup_ipni()], [search_tol()], [load_tol()],
#'  [match_knms()], or [match_krs()]
#'
#' @return A citation object with a print method for nice display.
#'
#' @examples
#' r <- search_powo(list(character="poison"))
#' kew_citation(r)
#'
#' r <- lookup_powo("320035-2")
#' kew_citation(r)
#'
#' r <- search_wcvp(list(genus="Poa"))
#' kew_citation(r)
#'
#' r <- lookup_wcvp("320035-2")
#' kew_citation(r)
#'
#' r <- search_ipni(list(publishing_author="L."))
#' kew_citation(r)
#'
#' r <- lookup_ipni("12653-1")
#' kew_citation(r)
#'
#' r <- search_tol("Poa")
#' kew_citation(r)
#'
#' r <- lookup_tol(2717)
#' kew_citation(r)
#'
#' tree <- load_tol()
#' kew_citation(tree)
#'
#' match <- match_knms("Poa annua")
#' kew_citation(match)
#'
#' match <- match_krs("Poa annua")
#' kew_citation(match)
#'
#'
#' @export
kew_citation <- function(x) {
  UseMethod("kew_citation")
}


#' @importFrom glue glue
#'
#' @export
kew_citation.powo <- function(x) {
  header <- "To cite POWO in publication please use:"

  info <- list(
    title="Plants of the World Online.",
    author="POWO",
    year="2021",
    note="Facilitated by the Royal Botanic Gardens, Kew.",
    accessed=Sys.Date(),
    url="http://www.plantsoftheworldonline.org/"
  )

  ref <- bibentry(
    bibtype="Manual",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title} {info$note} {info$url}.",
                     "Accessed {info$accessed}",
                     .sep=" "),
    header=header,
    other=info
  )

  class(ref) <- c("citation", "bibentry")

  ref
}


#' @importFrom glue glue
#'
#' @export
kew_citation.wcvp <- function(x) {
  header <- "To cite WCVP in publication please use:"

  info <- list(
    title="World Checklist of Vascular Plants",
    author="WCVP",
    year="2021",
    version="2.0",
    note="Facilitated by the Royal Botanic Gardens, Kew.",
    accessed=Sys.Date(),
    url="http://wcvp.science.kew.org/"
  )

  ref1 <- bibentry(
    bibtype="Manual",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title}, version {info$version}.",
                     "{info$note} {info$url}.",
                     "Accessed {info$accessed}",
                     .sep=" "),
    other=info
  )

  info <- list(
    title="The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity",
    author="Govaerts, R., Nic Lughadha, E., Black, N., Turner, R. and Paton, A.",
    year="2021",
    journal="Scientific Data",
    volume=8,
    number=215,
    url="https://doi.org/10.1038/s41597-021-00997-6"
  )

  ref2 <- bibentry(
    bibtype="Article",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title}. {info$journal},",
                     "{info$volume}({info$number}), {info$url}",
                     .sep=" "),
    other=info
  )

  ref <- c(ref1, ref2)
  attr(ref, "mheader") <- paste(header, collapse="\n")

  class(ref) <- c("citation", "bibentry")

  ref
}

#' @importFrom glue glue
#'
#' @export
kew_citation.ipni <- function(x) {
  header <- "To cite IPNI in publication please use:"

  info <- list(
    title="International Plant Names Index",
    author="IPNI",
    year="2021",
    note="The Royal Botanic Gardens, Kew, Harvard University Herbaria & Libraries and Australian National Botanic Gardens",
    accessed=Sys.Date(),
    url="https://ipni.org/"
  )

  ref <- bibentry(
    bibtype="Manual",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title}. {info$note}. {info$url}.",
                     "Accessed {info$accessed}",
                     .sep=" "),
    header=header,
    other=info
  )

  class(ref) <- c("citation", "bibentry")

  ref
}

#' @importFrom glue glue
#'
#' @export
kew_citation.tol <- function(x) {
  header <- "To cite ToL in publication please use:"

  info <- list(
    title="A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life",
    author="Baker W.J., Bailey P., Barber V., Barker A., Bellot S., Bishop D., Botigue L.R., Brewer G., Carruthers T., Clarkson J.J., Cook J., Cowan R.S., Dodsworth S., Epitawalage N., Francoso E., Gallego B., Johnson M., Kim J.T., Leempoel K., Maurin O., McGinnie C., Pokorny L., Roy S., Stone M., Toledo E., Wickett N.J., Zuntini A.R., Eiserhardt W.L., Kersey P.J., Leitch I.J. and Forest F.",
    year="2021",
    note="The Royal Botanic Gardens, Kew, Harvard University Herbaria & Libraries and Australian National Botanic Gardens",
    journal="Systematic Biology",
    volume="syab035",
    url="https://doi.org/10.1093/sysbio/syab035"
  )

  ref <- bibentry(
    bibtype="Article",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title}. {info$journal},",
                     "{info$volume}, {info$url}",
                     .sep=" "),
    other=info,
    header=header
  )

  class(ref) <- c("citation", "bibentry")

  ref
}

#' @importFrom glue glue
#'
#' @export
kew_citation.knms_match <- function(x) {
  header <- "To cite KNMS in publication please use:"

  info <- list(
    title="Kew Names Matching Service",
    author="KNMS",
    year="2021",
    accessed=Sys.Date(),
    url="http://namematch.science.kew.org/"
  )

  ref <- bibentry(
    bibtype="Manual",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title}. {info$url}.",
                     "Accessed {info$accessed}",
                     .sep=" "),
    header=header,
    other=info
  )

  class(ref) <- c("citation", "bibentry")

  ref
}


#' @importFrom glue glue
#'
#' @export
kew_citation.krs_match <- function(x) {
  header <- "To cite KRS in publication please use:"

  info <- list(
    title="Kew Reconciliation Service",
    author="KRS",
    year="2016",
    accessed=Sys.Date(),
    url="http://data1.kew.org/reconciliation/"
  )

  ref <- bibentry(
    bibtype="Manual",
    textVersion=glue("{info$author} ({info$year}).",
                     "{info$title}. {info$url}.",
                     "Accessed {info$accessed}",
                     .sep=" "),
    header=header,
    other=info
  )

  class(ref) <- c("citation", "bibentry")

  ref
}

