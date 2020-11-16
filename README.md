
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kewr

<!-- badges: start -->

<!-- badges: end -->

An R package to access data from RGB Kew’s APIs.

## Overview

kewr is meant to make accessing data from one of RGB Kew easier and to
provide a consistent interface their public APIs.

This package should cover:

  - [x] [World Checklist of Vascular
    Plants](https://wcvp.science.kew.org/)
  - [ ] [Plants of the World Online](http://powo.science.kew.org/)
  - [ ] [International Plant Names Index](https://www.ipni.org/)
  - [ ] [Kew Names Matching Service](http://namematch.science.kew.org/)

New sources will be added as they come up.

## Installation

kewr is not on CRAN yet but you can install the latest development
version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("barnabywalker/kewr")
```

## Examples

Entities in Kew’s taxonomic APIs are linked together across databases by
a unique identifier. The unique identifier refers to an individual name
and is given to that name when it is added to the IPNI database, so is
called the IPNI ID.

IPNI IDs can be used to directly look up information for a name in WCVP,
POW, and IPNI. These three APIs can also be queried using a taxonomic
name, or with keyword arguments. The Kew Names Matching service accepts
names and will return potential matches along with their IPNI ID.

### WCVP

To lookup taxonomic information for a particular IPNI ID:

``` r
info <- lookup_wcvp("271445-2")
print(info)
```

To search WCVP by name:

``` r
results <- search_wcvp("Myrcia guianensis")
print(results)
```

To get all accepted species in a genus:

``` r
results <- search_wcvp(list(genus="Myrcia"), filters=c("generic"))
print(results)
```

The search and lookup results can be formatted as tibbles:

``` r
format(results, synonyms="simplify")
format(info)
```

It is also possible to just download a zip file containing the latest
version of WCVP:

``` r
download_wcvp()
```
