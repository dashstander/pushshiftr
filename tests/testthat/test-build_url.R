testthat::context(".build_url")


test_that(".build_url", {
  url = "https://api.pushshift.io/"
  path = "reddit/search/comments/"
  query = "?q=rome&subreddit=askscience"

  expect_equal(.build_url(path, list(q = "rome", subreddit = "askscience")),
               paste0(url, path, query))
})
