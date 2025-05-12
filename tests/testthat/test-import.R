test_that("importTxt - PREFERRED", {
  path <- test_path("testdata/grid_no_preferred.txt")
  x <- importTxt(path)
  expect_equal(preferredPoles(x), rep(NA_character_, 4))

  path <- test_path("testdata/grid_preferred.txt")
  x_pref <- importTxt(path)
  expect_equal(preferredPoles(x_pref), c("left", "right", "none", NA))

  preferredPoles(x) <- preferredPoles(x_pref)
  expect_identical(x, x_pref)

  path <- test_path("testdata/grid_preferred_incorrect_1.txt")
  expect_error(importTxt(path), regexp = "Number of preferred poles \\(3\\) does not match number of constructs \\(4\\)")

  path <- test_path("testdata/grid_preferred_incorrect_2.txt")
  expect_error(importTxt(path), regexp = "'arg' should be one of .{1}left.{1}, .{1}right.{1}, .{1}none.{1}, .{1}both.{1}, .{1}NA.{1}")
})


test_that("importTxt - RATINGS", {
  path <- test_path("testdata/grid_no_preferred.txt")
  x <- importTxt(path)
  r_same <- ratings(x) == cbind(1:4, c(2:4, 1), c(3:4, 1:2))
  expect_true(all(r_same))
})
