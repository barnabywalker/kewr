test_that("search URL returns status 200", {
  url <- powo_search_url_()

  Sys.sleep(0.3)
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- powo_search_url_()

  Sys.sleep(0.3)
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("taxon URL response is json", {
  url <- powo_taxon_url_("30001404-2")

  Sys.sleep(0.3)
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("taxon URL returns 404 for bad ID", {
  url <- powo_taxon_url_("bad id")

  Sys.sleep(0.3)
  response <- httr::GET(url)

  expect_equal(status_code(response), 404)
})

test_that("raises error for unimplemented keyword", {
  query <- list(published="1920")
  expect_error(search_powo(query, .wait=0.3),
               "Query keyword.+ not recognised")
})

test_that("accepted filter only returns accepted names", {
  query <- "Myrcia"
  filters <- c("accepted")

  results <- search_powo(query, filters, .wait=0.3)
  all_accepted <- purrr::every(results$results,
                               ~.x$accepted)

  expect_true(all_accepted)
})

test_that("specific filter only returns species", {
  query <- "Myrcia"
  filters <- c("species")

  results <- search_powo(query, filters, .wait=0.3)
  all_species <- purrr::every(results$results,
                              ~.x$rank == "Species")

  expect_true(all_species)
})

test_that("generic filter only returns genera", {
  query <- "Myrcia"
  filters <- c("genera")

  results <- search_powo(query, filters, .wait=0.3)
  all_genera <- purrr::every(results$results,
                             ~.x$rank == "Genus")

  expect_true(all_genera)
})

test_that("infraspecific filter only returns infraspecifics", {
  infra_ranks <- c("Variety", "Subspecies", "Form")

  query <- "Poa annua"
  filters <- c("infraspecies")

  results <- search_powo(query, filters, .wait=0.3)
  all_infra <- purrr::every(results$results,
                            ~.x$rank %in% infra_ranks)

  expect_true(all_infra)
})

test_that("family filter only returns families", {

  query <- "poaceae"
  filters <- c("families")

  results <- search_powo(query, filters, .wait=0.3)
  all_families <- purrr::every(results$results,
                            ~.x$rank == "Family")

  expect_true(all_families)
})

test_that("image filter only returns things with images", {

  query <- "Myrcia"
  filters <- c("has_images")

  results <- search_powo(query, filters, .wait=0.3)
  all_images <- purrr::every(results$results,
                             ~length(.x$images) > 0)

  expect_true(all_images)
})

test_that("lookup with distribution returns distribution field", {
  taxonid <- "320035-2"

  results <- lookup_powo(taxonid, distribution=TRUE, .wait=0.3)

  expect_true("distribution" %in% names(results))
})

test_that("tidy search results returns tibble", {
  results <- search_powo("Poa annua", filters=c("species"), .wait=0.3)
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("tidy lookup results returns tibble", {
  results <- lookup_powo("30001404-2", .wait=0.3)
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("cursor returns next page of resutls", {
  query <- list(genus="Ulex")

  page1 <- search_powo(query, .wait=0.3)
  page2 <- search_powo(query, cursor=page1$cursor, .wait=0.3)

  expect_false(page1$results[[1]]$fqId == page2$results[[1]]$fqId)
})
