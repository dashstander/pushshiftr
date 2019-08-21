testthat::context(".replace_encoded_commas")

test_that("replace_encoded_commas", {

  url_in <- "https://api.pushshift.io/reddit/search/submission/?ids=bfghow%2Cbfghno&q=trump%2Crussia"
  url_out <- "https://api.pushshift.io/reddit/search/submission/?ids=bfghow,bfghno&q=trump%2Crussia"

  expect_equal(.replace_encoded_commas(url_in), url_out)

})
