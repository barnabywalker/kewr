
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kewr

<!-- badges: start -->

[![R build
status](https://github.com/barnabywalker/kewr/workflows/R-CMD-check/badge.svg)](https://github.com/barnabywalker/kewr/actions)
<!-- badges: end -->

An R package to access data from RGB Kew’s APIs.

## Overview

kewr is meant to make accessing data from one of RGB Kew easier and to
provide a consistent interface their public APIs.

This package should cover:

  - [x] [World Checklist of Vascular
    Plants](https://wcvp.science.kew.org/)
  - [x] [Plants of the World Online](http://powo.science.kew.org/)
  - [x] [International Plant Names Index](https://www.ipni.org/)
  - [x] [Kew Names Matching Service](http://namematch.science.kew.org/)

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
results <- search_wcvp(list(genus="Myrcia"), filters=c("accepted"))
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

### POWO

The interface to POWO is very similar to WCVP, except it returns
slightly different information and field names.

To lookup taxonomic information for a particular IPNI ID:

``` r
info <- lookup_powo("271445-2")
print(info)
```

POWO contains distribution data for many taxa, which we can request
using the lookup function:

``` r
info <- lookup_powo("271445-2")
print(info)
```

To search POWO by name:

``` r
results <- search_powo("Myrcia guianensis")
print(results)
```

To get all accepted species in a genus:

``` r
results <- search_powo(list(genus="Myrcia"), filters=c("accepted"))
print(results)
```

Looking at the documentation for the POWO API, it uses a cursor for
pagination. To keep things the similar between the POWO and WCVP
interfaces, I haven’t implemented this yet. Simple pagination is
implemented by changing the maximum number of results returned.

``` r
results <- search_powo(list(distribution="Mexico"), filters=c("accepted"), limit=200)
print(results)
```

The search and lookup results can be formatted as tibbles:

``` r
format(results, synonyms="simplify")
format(info)
format(info, field="distribution")
```

### IPNI

The interface for IPNI is similar to WCVP and POWO.

There are three different types of record that you can look up in IPNI:
taxon, author, or publication.

To look up a taxon citation with an IPNI ID:

``` r
info <- lookup_ipni("271445-2", type="taxon")
print(info)
```

To look up an author with an IPNI ID:

``` r
info <- lookup_ipni("30001404-2", type="author")
print(info)
```

To look up a publication with an IPNI ID:

``` r
info <- lookup_ipni("987-2", type="publication")
print(info)
```

IPNI can also be searched by name:

``` r
results <- search_ipni("Myrcia guianensis")
print(results)
```

Or using keywords, for instance to get all family names published in
1989:

``` r
results <- search_ipni(list(published="1989"), filters=c("families"))
print(results)
```

The search and lookup results can also be formatted as tibbles:

``` r
format(results)
format(info)
```

### KNMS

Multiple names can be submitted to the Kew Names Matching Service at
once:

``` r
names <- c("Myrcia guianensis", "Poa annua L.", "Psidium sp.")
matches <- match_knms(names)
print(matches)
```

The raw results from KNMS are returned without column headers, and can
be a little difficult to understand. They can be formatted into a nice
tibble though:

``` r
format(matches)
```
