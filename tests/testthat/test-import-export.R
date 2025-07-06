# TEXTFILE --------------------------------------------

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



# EXCEL --------------------------------------------

test_that("export-import - roundtrip", {
  g_orig <- boeker

  file_wide <- saveAsExcel(g_orig, file = tempfile(fileext = ".xlsx"), format = "wide")
  g_wide <- importExcel(file_wide, format = "wide")
  expect_identical(g_orig, g_wide)

  file_long <- saveAsExcel(g_orig, file = tempfile(fileext = ".xlsx"), format = "long")
  g_long <- importExcel(file_long, format = "long")
  expect_identical(g_orig, g_long)
})


test_that("importExcel - PREFERRED", {
  path <- test_path("testdata/grids.xlsx")

  x <- importExcel(path, sheet = "no preferred")
  x_pref <- importExcel(path, sheet = "with preferred")

  expect_equal(preferredPoles(x), rep(NA_character_, 10))
  expect_equal(preferredPoles(x_pref), c("left", "right", "none", NA, "left", "left", "left", "right", NA, NA))

  preferredPoles(x) <- preferredPoles(x_pref)
  expect_identical(x, x_pref)

  expect_error(
    regexp = "'arg' should be one of .{1}left.{1}, .{1}right.{1}, .{1}none.{1}, .{1}both.{1}, .{1}NA.{1}",
    importExcel(path, sheet = "preferred incorrect 1")
  )
})


# DATAFRAME --------------------------------------------

test_that("importDataframe", {
  # sample dataframes yield same results
  g1 <- importDataframe(df_element_columns)
  g2 <- importDataframe(df_construct_columns, format = "c", rmin = 1, rmax = 5)
  preferredPoles(g2) <- c("l", "r", "n")
  g3 <- importDataframe(df_long, format = "long")

  expect_identical(g1, g2)
  expect_identical(g1, g3)
})
