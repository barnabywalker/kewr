test_that("error for unimplemented WCVP filters", {
  filters <- c("accepted", "has_image", "monkey")

  expect_error(format_filters_(filters, "wcvp"),
               ".+\\[has_image,monkey\\] are not recognised.")
})

test_that("error for unimplemented POWO filters", {
  filters <- c("accepted", "author", "monkey")

  expect_error(format_filters_(filters, "powo"),
               ".+\\[author,monkey\\] are not recognised.")
})

test_that("error for unimplemented IPNI filters", {
  filters <- c("infrafamilies", "author", "monkey")

  expect_error(format_filters_(filters, "ipni"),
               ".+\\[author,monkey\\] are not recognised.")
})

test_that("error for unimplemented WCVP kewords", {
  query <- list("distribution"="Mexico")

  expect_error(format_query_(query, "wcvp"),
               ".+\\[distribution\\] are not recognised")
})

test_that("error for unimplemented WCVP kewords", {
  query <- list("lifeform"="epiphyte")

  expect_error(format_query_(query, "powo"),
               ".+\\[lifeform\\] are not recognised")
})

test_that("error for unrecognised resource", {
  filters <- c("accepted")

  expect_error(format_filters_(filter, "google"))
})
