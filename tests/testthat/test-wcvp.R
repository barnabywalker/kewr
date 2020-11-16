test_that("search URL returns status 200", {
  url <- wcvp_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- wcvp_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("taxon URL response is json", {
  url <- wcvp_taxon_url_("30001404-2")
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("taxon URL returns 404 for bad ID", {
  url <- wcvp_taxon_url_("bad id")
  response <- httr::GET(url)
  expect_equal(status_code(response), 404)
})

test_that("raises error for unimplemented filter", {
  filters <- c("monkey")
  query <- "Myrcia guianensis"

  expect_error(search_wcvp(query, filters),
               "Filter.+ not recognised")
})

test_that("raises error for unimplemented keyword", {
  query <- list(name="Myrcia guianensis")

  expect_error(search_wcvp(query),
               "Query keyword.+ not recognised")
})

test_that("raises error for bad query input type", {
  query <- c("this", "is", "a", "bad", "query")

  expect_error(search_wcvp(query))
})

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

test_that("format search results returns tibble", {
  results <- search_wcvp("Poa annua", filters=c("specific"))
  formatted <- format(results)

  expect_s3_class(formatted, "tbl_df")
})

test_that("format lookup results returns tibble", {
  results <- lookup_wcvp("30001404-2")
  formatted <- format(results)

  expect_s3_class(formatted, "tbl_df")
})

test_that("wcvp download link is a zip file", {
  download_link <- wcvp_download_url_()

  expect_true(endsWith(download_link, "zip"))
})

test_that("wcvp download link returns right version", {
  download_link <- wcvp_download_url_(2)

  expect_true(stringr::str_detect(download_link, "2"))
})

test_that("wcvp download link errors for unimplemented version", {
  expect_error(wcvp_download_url_(3000),
               "Not a recognised version")
})
