test_that("method exists for WCVP search results", {
  method_list <- methods(class="wcvp_search")
  method_list <- as.list(method_list)

  expect_true("request_next.wcvp_search" %in% method_list)
})

test_that("method exists for POWO search results", {
  method_list <- methods(class="powo_search")
  method_list <- as.list(method_list)

  expect_true("request_next.powo_search" %in% method_list)
})

test_that("method exists for IPNI search results", {
  method_list <- methods(class="ipni_search")
  method_list <- as.list(method_list)

  expect_true("request_next.ipni_search" %in% method_list)
})

test_that("cursor changes for WCVP", {
  page1 <- search_wcvp(list(genus="Poa"), filters="accepted")
  page2 <- request_next(page1)

  expect_false(page1$cursor == page2$cursor)
})

test_that("cursor changes for POWO", {
  page1 <- search_powo(list(genus="Poa"), filters="accepted")
  page2 <- request_next(page1)

  expect_false(page1$cursor == page2$cursor)
})

test_that("cursor changes for IPNI", {
  page1 <- search_ipni(list(genus="Poa"), filters="species")
  page2 <- request_next(page1)

  expect_false(page1$cursor == page2$cursor)
})

test_that("results change for WCVP", {
  page1 <- search_wcvp(list(genus="Poa"), filters="accepted")
  page2 <- request_next(page1)

  expect_false(page1$results[[1]]$id == page2$results[[1]]$id)
})

test_that("results change for POWO", {
  page1 <- search_powo(list(genus="Poa"), filters="accepted")
  page2 <- request_next(page1)

  expect_false(page1$results[[1]]$fqId == page2$results[[1]]$fqId)
})

test_that("results change for IPNI", {
  page1 <- search_ipni(list(genus="Poa"), filters="species")
  page2 <- request_next(page1)

  expect_false(page1$results[[1]]$id == page2$results[[1]]$id)
})
