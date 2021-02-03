test_that("search URL returns status 200", {
  url <- ipni_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- ipni_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("raises error for unimplemented keyword", {
  query <- list(name="Myrcia guianensis")

  expect_error(search_ipni(query),
               "Query keyword.+ not recognised")
})

test_that("raises error for bad query input type", {
  query <- c("this", "is", "a", "bad", "query")

  expect_error(search_ipni(query))
})

test_that("tidy search results returns tibble", {
  results <- search_ipni("Poa annua")
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("tidy lookup results returns tibble", {
  results <- lookup_ipni("30001404-2")
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("specific filter only returns species", {
  query <- "Myrcia"
  filters <- c("species")

  results <- search_ipni(query, filters)
  all_species <- purrr::every(results$results,
                              ~.x$rank == "spec.")

  expect_true(all_species)
})

test_that("infraspecific filter only returns infraspecifics", {
  infra_ranks <- c("Variety", "Subspecies", "Form")

  query <- "Poa annua"
  filters <- c("infraspecies")

  results <- search_wcvp(query, filters)
  all_infra <- purrr::every(results$results,
                            ~.x$rank %in% infra_ranks)

  expect_true(all_infra)
})

test_that("generic filter only returns genera", {
  query <- "Myrcia"
  filters <- c("genera")

  results <- search_ipni(query, filters)
  all_genera <- purrr::every(results$results,
                             ~.x$rank == "gen.")

  expect_true(all_genera)
})

test_that("infrageneric filter only returns infragenera", {
  query <- "Behenantha"
  filters <- c("infragenera")

  results <- search_ipni(query, filters)
  all_genera <- purrr::every(results$results,
                             ~.x$rank == "sect.")

  expect_true(all_genera)
})

test_that("family filter only returns families", {

  query <- "poaceae"
  filters <- c("families")

  results <- search_wcvp(query, filters)
  all_families <- purrr::every(results$results,
                               ~.x$rank == "fam.")

  expect_true(all_families)
})

test_that("infrafamily filter only returns infrafamilies", {

  query <- "Rosoideae"
  filters <- c("infrafamilies")

  results <- search_ipni(query, filters)
  all_families <- purrr::every(results$results,
                               ~.x$rank == "subfam.")

  expect_true(all_families)
})

