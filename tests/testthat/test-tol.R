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
