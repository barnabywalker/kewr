#' Extinction risk assessments for Danish plants.
#'
#' A dataset containing global extinction risk assessments for
#' plants found in Denmark. Source from the IUCN Red List of
#' Threatened Plants using the `rredlist` package.
#'
#' @format A data frame with 361 rows and 4 variables:
#' \describe{
#'   \item{taxonid}{IUCN Red List unique ID for the taxon}
#'   \item{scientific_name}{The scientific name of the taxon}
#'   \item{authority}{The taxonomic authority of the taxon name}
#'   \item{category}{The IUCN Red List assessment category}
#' }
#'
#' @source \url{https://www.iucnredlist.org/}
#'
#' @references
#' Scott Chamberlain (2020). rredlist: 'IUCN' Red List Client.
#' R package version 0.7.0. https://CRAN.R-project.org/package=rredlist
#'
#' IUCN 2021. The IUCN Red List of Threatened Species. Version 2020-3.
#' <https://www.iucnredlist.org>
"danish_plants"
