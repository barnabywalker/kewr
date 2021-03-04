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

  vcr::use_cassette("request-next-wcvp-cursor", {
    page1 <- search_wcvp(list(genus="Poa"), filters="accepted")
    page2 <- request_next(page1)
  }, serialize_with="json")

  expect_false(page1$cursor == page2$cursor)
})

test_that("cursor changes for POWO", {

  vcr::use_cassette("request-next-powo-cursor", {
    page1 <- search_powo(list(genus="Poa"), filters="accepted")
    page2 <- request_next(page1)
  }, serialize_with="json")

  expect_false(page1$cursor == page2$cursor)
})

test_that("cursor changes for IPNI", {
  vcr::use_cassette("request-next-ipni-cursor", {
    page1 <- search_ipni(list(genus="Poa"), filters="species")
    page2 <- request_next(page1)
  }, serialize_with="json")

  expect_false(page1$cursor == page2$cursor)
})

test_that("results change for WCVP", {
  vcr::use_cassette("request-next-wcvp", {
    page1 <- search_wcvp(list(genus="Poa"), filters="accepted")
    page2 <- request_next(page1)
  }, serialize_with="json")

  expect_false(page1$results[[1]]$id == page2$results[[1]]$id)
})

test_that("results change for POWO", {
  vcr::use_cassette("request-next-powo", {
    page1 <- search_powo(list(genus="Poa"), filters="accepted")
    page2 <- request_next(page1)
  }, serialize_with="json")

  expect_false(page1$results[[1]]$fqId == page2$results[[1]]$fqId)
})

test_that("results change for IPNI", {
  vcr::use_cassette("request-next-ipni", {
    page1 <- search_ipni(list(genus="Poa"), filters="species")
    page2 <- request_next(page1)
  }, serialize_with="json")

  expect_false(page1$results[[1]]$id == page2$results[[1]]$id)
})
