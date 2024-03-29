% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{angiosperm_assessments}
\alias{angiosperm_assessments}
\title{Extinction risk assessments for angiosperms.}
\format{
A data frame with 53,542 rows and 4 variables:
\describe{
\item{taxonid}{IUCN Red List unique ID for the taxon}
\item{scientific_name}{The scientific name of the taxon}
\item{taxonomic_authority}{The taxonomic authority of the taxon name}
\item{category}{The IUCN Red List assessment category}
}
}
\source{
\url{https://www.iucnredlist.org/}
}
\usage{
angiosperm_assessments
}
\description{
A dataset containing global extinction risk assessments for
all assessed angiosperm species. Sourced from the IUCN Red List of
Threatened Plants using the \code{rredlist} package. Last updated with
version 2021-1 of the IUCN Red List.
}
\references{
Scott Chamberlain (2020). rredlist: 'IUCN' Red List Client.
R package version 0.7.0. https://CRAN.R-project.org/package=rredlist

IUCN 2021. The IUCN Red List of Threatened Species. Version 2020-3.
\url{https://www.iucnredlist.org}
}
\keyword{datasets}
