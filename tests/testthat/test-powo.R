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

  Sys.sleep(0.3)
  expect_error(search_powo(query),
               "Query keyword.+ not recognised")
})

test_that("accepted filter only returns accepted names", {
  query <- "Myrcia"
  filters <- c("accepted")

  Sys.sleep(0.3)
  results <- search_powo(query, filters)
  all_accepted <- purrr::every(results$results,
                               ~.x$accepted)

  expect_true(all_accepted)
})

test_that("specific filter only returns species", {
  query <- "Myrcia"
  filters <- c("species")

  Sys.sleep(0.3)
  results <- search_powo(query, filters)
  all_species <- purrr::every(results$results,
                              ~.x$rank == "Species")

  expect_true(all_species)
})

test_that("generic filter only returns genera", {
  query <- "Myrcia"
  filters <- c("genera")

  Sys.sleep(0.3)
  results <- search_powo(query, filters)
  all_genera <- purrr::every(results$results,
                             ~.x$rank == "Genus")

  expect_true(all_genera)
})

test_that("infraspecific filter only returns infraspecifics", {
  infra_ranks <- c("Variety", "Subspecies", "Form")

  query <- "Poa annua"
  filters <- c("infraspecies")

  Sys.sleep(0.3)
  results <- search_powo(query, filters)
  all_infra <- purrr::every(results$results,
                            ~.x$rank %in% infra_ranks)

  expect_true(all_infra)
})

test_that("family filter only returns families", {

  query <- "poaceae"
  filters <- c("families")

  Sys.sleep(0.3)
  results <- search_powo(query, filters)
  all_families <- purrr::every(results$results,
                            ~.x$rank == "Family")

  expect_true(all_families)
})

test_that("image filter only returns things with images", {

  query <- "Myrcia"
  filters <- c("has_images")

  Sys.sleep(0.3)
  results <- search_powo(query, filters)
  all_images <- purrr::every(results$results,
                             ~length(.x$images) > 0)

  expect_true(all_images)
})

test_that("lookup with distribution returns distribution field", {
  taxonid <- "320035-2"

  Sys.sleep(0.3)
  results <- lookup_powo(taxonid, distribution=TRUE)

  expect_true("distribution" %in% names(results))
})

test_that("tidy search results returns tibble", {
  Sys.sleep(0.3)
  results <- search_powo("Poa annua", filters=c("species"))
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("tidy lookup results returns tibble", {
  Sys.sleep(0.3)
  results <- lookup_powo("30001404-2")
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("cursor returns next page of resutls", {
  query <- list(genus="Ulex")

  Sys.sleep(0.3)
  page1 <- search_powo(query)

  Sys.sleep(0.3)
  page2 <- search_powo(query, cursor=page1$cursor)

  expect_false(page1$results[[1]]$fqId == page2$results[[1]]$fqId)
})
