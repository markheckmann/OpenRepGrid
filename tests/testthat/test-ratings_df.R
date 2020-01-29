# dummy test

test_that("ratings_df returns a dataframe", {
  df <- ratings_df(boeker)
  expect_true(inherits(df, "data.frame"))
})
