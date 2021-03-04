test_that("search URL returns status 200", {
  url <- ipni_search_url_()
  vcr::use_cassette("ipni-search-url", {
    response <- httr::GET(url)
  }, serialize_with="json")

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- ipni_search_url_()
  vcr::use_cassette("ipni-search-json", {
    response <- httr::GET(url)
  }, serialize_with="json")

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
  vcr::use_cassette("ipni-search-tidy", {
    results <- search_ipni("Poa annua")
  }, serialize_with="json")

  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("tidy lookup results returns tibble", {
  vcr::use_cassette("ipni-lookup-tidy", {
    results <- lookup_ipni("30001404-2")
  }, serialize_with="json")

  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("specific filter only returns species", {
  query <- "Myrcia"
  filters <- c("species")

  vcr::use_cassette("ipni-filter-species", {
    results <- search_ipni(query, filters)
  }, serialize_with="json")

  all_species <- purrr::every(results$results,
                              ~.x$rank == "spec.")

  expect_true(all_species)
})

test_that("infraspecific filter only returns infraspecifics", {
  infra_ranks <- c("Variety", "Subspecies", "Form")

  query <- "Poa annua"
  filters <- c("infraspecies")

  vcr::use_cassette("ipni-filter-infraspecies", {
    results <- search_wcvp(query, filters)
  }, serialize_with="json")

  all_infra <- purrr::every(results$results,
                            ~.x$rank %in% infra_ranks)

  expect_true(all_infra)
})

test_that("generic filter only returns genera", {
  query <- "Myrcia"
  filters <- c("genera")

  vcr::use_cassette("ipni-filter-genus", {
    results <- search_ipni(query, filters)
  }, serialize_with="json")

  all_genera <- purrr::every(results$results,
                             ~.x$rank == "gen.")

  expect_true(all_genera)
})

test_that("infrageneric filter only returns infragenus", {
  query <- "Behenantha"
  filters <- c("infragenera")

  vcr::use_cassette("ipni-filter-infragenus", {
    results <- search_ipni(query, filters)
  }, serialize_with="json")

  all_genera <- purrr::every(results$results,
                             ~.x$rank == "sect.")

  expect_true(all_genera)
})

test_that("family filter only returns families", {

  query <- "poaceae"
  filters <- c("families")

  vcr::use_cassette("ipni-filter-family", {
    results <- search_wcvp(query, filters)
  }, serialize_with="json")

  all_families <- purrr::every(results$results,
                               ~.x$rank == "fam.")

  expect_true(all_families)
})

test_that("infrafamily filter only returns infrafamilies", {

  query <- "Rosoideae"
  filters <- c("infrafamilies")
  vcr::use_cassette("ipni-filter-infrafamily", {
    results <- search_ipni(query, filters)
  }, serialize_with="json")

  all_families <- purrr::every(results$results,
                               ~.x$rank == "subfam.")

  expect_true(all_families)
})

test_that("cursor returns next page of results", {
  query <- list(genus="Ulex")
  vcr::use_cassette("ipni-search-cursor", {
    page1 <- search_ipni(query)
    page2 <- search_ipni(query, cursor=page1$cursor)
  }, serialize_with="json")

  expect_false(page1$results[[1]]$fqId == page2$results[[1]]$fqId)
})
