context("replace_if_missing")

test_that("replace_if_missing", {

  defaults = list(size = 25,
                  sort = "desc",
                  sort_type = "created_utc")

  other_params = list(q = "test",
                      size = 25,
                      sort = "desc",
                      sort_type = "created_utc")

  expect_equal(replace_if_missing(list(), defaults), defaults)
  expect_equal(replace_if_missing(defaults, defaults), defaults)
  expect_equal(replace_if_missing(list(q = "test"), defaults), other_params)
})
