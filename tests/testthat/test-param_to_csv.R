testthat::context("param_to_csv")

testthat::test_that("param_to_csv", {

  char_vector <- c("hello", "world")
  char_vector_out <- "hello,world"


  non_char_list <- list(ids = c(123, 456),
                        subreddit = c("AskHistorians", "AskScience"))
  non_char_list_out <- list(ids = "123,456",
                            subreddit = "AskHistorians,AskScience")

  expect_equal(param_to_csv(char_vector), char_vector_out)
  expect_equal(param_to_csv(non_char_list), non_char_list_out)
})
