test_that("GET request to KNMS returns 405 error", {
  url <- knms_url_()
  vcr::use_cassette("knms-get", {
    response <- httr::GET(url)
  }, serialize_with="json")

  expect_equal(httr::status_code(response), 405)
})

test_that("POST request to KNMS returns 200", {
  url <- knms_url_()
  vcr::use_cassette("knms-post", {
    response <- httr::POST(url, body=list(""), encode="json")
  }, serialize_with="json")

  expect_equal(httr::status_code(response), 200)
})

test_that("POST request to KNMS returns a json", {
  url <- knms_url_()
  vcr::use_cassette("knms-json", {
    response <- httr::POST(url, body=list(""), encode="json")
  }, serialize_with="json")

  expect_equal(httr::http_type(response), "application/json")
})

test_that("Raises error if names aren't right", {
  names <- c(10, 20, 300, 500)

  expect_error(match_knms(names))
})

test_that("Line parsing returns a tibble", {
  names <- c("Bad plant")
  vcr::use_cassette("knms-parse-lines", {
    matches <- match_knms(names)
  }, serialize_with="json")

  parsed <- parse_knms_line(matches$results[[1]])

  expect_s3_class(parsed, "tbl_df")
})

test_that("Match tidying returns a tibble", {
  names <- c("Bad plant", "Poa annua", "Myrcia guianensis")
  vcr::use_cassette("knms-tidy", {
    matches <- match_knms(names)
  }, serialize_with="json")

  tidied <- tidy(matches)

  expect_s3_class(tidied, "tbl_df")
})
