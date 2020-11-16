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
