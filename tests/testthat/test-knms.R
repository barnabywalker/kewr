test_that("GET request to KNMS returns 405 error", {
  url <- knms_url_()
  response <- httr::GET(url)

  expect_equal(httr::status_code(response), 405)
})

test_that("POST request to KNMS returns 200", {
  url <- knms_url_()
  response <- httr::POST(url, body=list(""), encode="json")

  expect_equal(httr::status_code(response), 200)
})

test_that("POST request to KNMS returns a json", {
  url <- knms_url_()
  response <- httr::POST(url, body=list(""), encode="json")

  expect_equal(httr::http_type(response), "application/json")
})

test_that("Raises error if names aren't right", {
  names <- c(10, 20, 300, 500)

  expect_error(match_knms(names))
})

