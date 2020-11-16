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

test_that("error for unrecognised resource", {
  filters <- c("accepted")

  expect_error(format_filters_(filter, "google"))
})
