
# test for issues #22 and #31 (constructs were missing after align)
test_that("align()", {

  sorted_poles <- function(x) {
    df_con <- constructs(x)
    l <- as.list(as.data.frame(t(df_con))) # df rows as list
    vapply(l, function(x) paste(sort(x), collapse = " "), character(1))
  }

  file <- testthat::test_path("testdata/issue_22.txt")
  x <- importTxt(file)
  x_aligned <- align(x)
  d <- setdiff(sorted_poles(x), sorted_poles(x_aligned))
  expect_length(d, 0)

  file <- testthat::test_path("testdata/issue_31.txt")
  x <- importTxt(file)
  x_aligned <- align(x)
  d <- setdiff(sorted_poles(x), sorted_poles(x_aligned))
  expect_length(d, 0)
})
