test_that("search URL returns status 200", {
  url <- powo_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- powo_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("accepted filter only returns accepted names", {
  query <- "Myrcia"
  filters <- c("accepted")

  results <- search_powo(query, filters)
  all_accepted <- purrr::every(results$results,
                               ~.x$accepted)

  expect_true(all_accepted)
})

test_that("specific filter only returns species", {
  query <- "Myrcia"
  filters <- c("species")

  results <- search_powo(query, filters)
  all_species <- purrr::every(results$results,
                              ~.x$rank == "Species")

  expect_true(all_species)
})

test_that("generic filter only returns genera", {
  query <- "Myrcia"
  filters <- c("genera")

  results <- search_powo(query, filters)
  all_genera <- purrr::every(results$results,
                             ~.x$rank == "Genus")

  expect_true(all_genera)
})

test_that("infraspecific filter only returns infraspecifics", {
  infra_ranks <- c("Variety", "Subspecies", "Form")

  query <- "Poa annua"
  filters <- c("infraspecies")

  results <- search_powo(query, filters)
  all_infra <- purrr::every(results$results,
                            ~.x$rank %in% infra_ranks)

  expect_true(all_infra)
})

test_that("family filter only returns families", {

  query <- "poaceae"
  filters <- c("families")

  results <- search_powo(query, filters)
  all_families <- purrr::every(results$results,
                            ~.x$rank == "Family")

  expect_true(all_families)
})

test_that("image filter only returns things with images", {

  query <- "Myrcia"
  filters <- c("has_images")

  results <- search_powo(query, filters)
  all_images <- purrr::every(results$results,
                             ~length(.x$images) > 0)

  expect_true(all_images)
})
