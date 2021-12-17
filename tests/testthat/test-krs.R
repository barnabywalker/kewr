test_that("match URL returns status 200", {
  url <- krs_url_()
  response <- httr::RETRY("GET", url, times=3)

  expect_equal(httr::status_code(response), 200)
})

test_that("match URL response is json", {
  url <- krs_url_()

  response <- httr::GET(url)

  expect_equal(httr::http_type(response), "application/json")
})

test_that("raises error for unimplemented keyword", {
  query <- list(published="1920")
  expect_error(match_krs(query), "Query keyword.+ not recognised")
})

test_that("tidy match results returns tibble", {
  results <- match_krs("Poa annua")
  tidied <- tidy(results)

  expect_s3_class(tidied, "tbl_df")
})

