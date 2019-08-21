context(".build_params")


test_that(".build_params", {
  output1 = list(q = "Cao Cao",
                 q = "Liu Bei",
                subreddit = "AskHistorians",
                size = 25,
                sort = "desc")

  output2 = list(q = "Lie Algebra",
                 subreddit = "math",
                 size = 500,
                 sort = "asc")

  expect_equal(.build_params("Cao Cao", "Liu Bei", subreddit = "AskHistorians"),
               output1)
  expect_equal(.build_params("Lie Algebra", subreddit = "math", size = 500, sort = "asc"),
               output2)
})
