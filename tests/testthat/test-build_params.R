
testthat::context(".build_params")


test_that(".build_params", {

  params_w_comma_sep <- list(ids = c("abc", "def", "ghi"),
                             q = c("donald", "trump"),
                             sort = "asc")

  params_wo_comma_sep <- list(subreddit = "AskHistorians",
                              q = "donald trump")

  params_w_comma_sep_out = list(ids = "abc,def,ghi",
                                q = "donald,trump",
                                sort = "asc",
                                size = 25,
                                sort_type = "created_utc")
  params_wo_comma_sep_out = list(subreddit = "AskHistorians",
                                 q = "'donald trump'",
                                 size = 25,
                                 sort = "desc",
                                 sort_type = "created_utc")


  expect_equal(do.call(.build_params, params_w_comma_sep), params_w_comma_sep_out)
  expect_equal(do.call(.build_params, params_wo_comma_sep), params_wo_comma_sep_out)


})
