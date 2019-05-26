testthat::context(".parse_search_terms")


test_that(".parse_search_terms", {

  search_terms_1 = c("hello", "world")
  search_terms_out_1 = "hello,world"

  search_terms_2 = c("hello, world", "it is me")
  search_terms_out_2 = c("'hello, world','it is me'")

  expect_equal(.parse_search_terms(search_terms_1), search_terms_out_1)
  expect_equal(.parse_search_terms(search_terms_2), search_terms_out_2)
})
