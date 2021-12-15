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

test_that("Raises error if missing value in names to match", {
  names <- c("Poa annua", NA_character_, NA_character_, "Myrcia almasensis")

  expect_error(match_knms(names), regexp="NA is present")
})

test_that("Line parsing returns a tibble", {
  names <- c("Bad plant")
  matches <- match_knms(names)
  parsed <- parse_knms_line(matches$results[[1]])

  expect_s3_class(parsed, "tbl_df")
})

test_that("Match tidying returns a tibble", {
  names <- c("Bad plant", "Poa annua", "Myrcia guianensis")
  matches <- match_knms(names)
  tidied <- tidy(matches)

  expect_s3_class(tidied, "tbl_df")
})
