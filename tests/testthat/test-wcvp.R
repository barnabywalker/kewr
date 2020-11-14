test_that("search URL returns status 200", {
  url <- wcvp_search_url()
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- wcvp_search_url()
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("taxon URL response is json", {
  url <- wcvp_taxon_url("30001404-2")
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("raises error for unimplemented filter", {
  filters <- c("monkey")
  query <- "Myrcia guianensis"

  expect_error(search_wcvp(query, filters),
               "Filter.+ not recognised")
})

#' @import purrr
test_that("accepted filter only returns accepted names", {
  query <- "Myrcia"
  filters <- c("accepted")

  results <- search_wcvp(query, filters)
  all_accepted <- purrr::every(results$content$results,
                               ~.x$accepted)

  expect_true(all_accepted)
})

test_that("specific filter only returns species", {
  query <- "Myrcia"
  filters <- c("specific")

  results <- search_wcvp(query, filters)
  all_species <- purrr::every(results$content$results,
                               ~.x$rank == "Species")

  expect_true(all_species)
})

test_that("generic filter only returns genera", {
  query <- "Myrcia"
  filters <- c("generic")

  results <- search_wcvp(query, filters)
  all_genera <- purrr::every(results$content$results,
                              ~.x$rank == "Genus")

  expect_true(all_genera)
})

test_that("infraspecific filter only returns infraspecifics", {
  infra_ranks <- c("Variety", "Subspecies", "Form")

  query <- "Poa annua"
  filters <- c("infraspecific")

  results <- search_wcvp(query, filters)
  all_infra <- purrr::every(results$content$results,
                             ~.x$rank %in% infra_ranks)

  expect_true(all_infra)
})
