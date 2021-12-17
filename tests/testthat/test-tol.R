test_that("search URL returns status 200", {
  url <- tol_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 200)
})

test_that("search URL response is json", {
  url <- tol_search_url_()
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("search URL is for genes when asked for", {
  url <- tol_search_url_(type="genes")

  expect_true(stringr::str_detect(url, "/genes"))
})

test_that("specimen URL response is json", {
  url <- tol_lookup_url_("2699")
  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("gene lookup returns gene URL", {
  url <- tol_lookup_url_("51", type="gene")

  expect_true(stringr::str_detect(url, "/genes/"))
})

test_that("specimen URL returns 404 for bad ID", {
  url <- tol_lookup_url_("plant")
  response <- httr::GET(url)
  expect_equal(status_code(response), 404)
})

test_that("raises error for keyword search", {
  query <- list(name="Myrcia guianensis")

  expect_error(search_tol(query),
               "Keyword-based search not implemented")
})

test_that("raises error for bad query input type", {
  query <- c("this", "is", "a", "bad", "query")

  expect_error(search_tol(query))
})

test_that("tidy search results returns tibble", {
  results <- search_tol("Poa annua")
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("tidy lookup results returns tibble", {
  results <- lookup_tol("2699")
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

test_that("loading fasta gives a fasta object", {
  url <- "http://sftp.kew.org/pub/paftol/current_release/fasta/by_gene/5328.dna.fasta"
  r <- load_tol(url)

  expect_equal(class(r), c("tol_fasta", "tol"))
})

test_that("loading tree gives a tree object", {
  r <- load_tol()

  expect_equal(class(r), c("tol_tree", "tol"))
})
