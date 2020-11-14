test_that("raises 404 error for bad URL", {
  expect_error(make_request_("NOT A VALID URL"))
})

test_that("raises error for non-json response", {
  url <- "https://www.wcvp.science.kew.org"
  expect_error(make_request_(url))
})
