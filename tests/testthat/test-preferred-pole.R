test_that("preferredPoles", {
  x <- boeker
  nc <- nrow(x)

  preferredPoles(x) <- "left"
  expect_equal(preferredPoles(x), rep_len("left", nc))

  xr <- reverse(x)
  expect_equal(preferredPoles(xr), rep_len("right", nc))

  preferredPoles(x) <- "none"
  expect_equal(preferredPoles(x), rep_len("none", nc))

  preferredPoles(x) <- "both"
  expect_equal(preferredPoles(x), rep_len("both", nc))

  preferredPoles(x) <- NA
  expect_equal(preferredPoles(x), rep_len(NA_character_, nc))
})


test_that("preferred_pole_colors returns correct colors", {
  x <- boeker
  nc <- nrow(x)

  # set mixed preferences
  prefs <- c("left", "right", "none", "both", NA)
  prefs <- rep_len(prefs, nc)
  preferredPoles(x) <- prefs

  colors <- preferred_pole_colors(x)

  # left preferred -> left green, right red

  expect_equal(colors$left[1], "green")
  expect_equal(colors$right[1], "red")

  # right preferred -> left red, right green
  expect_equal(colors$left[2], "red")
  expect_equal(colors$right[2], "green")

  # none -> both dark grey
  expect_equal(colors$left[3], grey(.4))
  expect_equal(colors$right[3], grey(.4))

  # both -> both green
  expect_equal(colors$left[4], "green")
  expect_equal(colors$right[4], "green")

  # NA -> both dark grey
  expect_equal(colors$left[5], grey(.4))
  expect_equal(colors$right[5], grey(.4))

  # custom colors
  colors2 <- preferred_pole_colors(x, col_preferred = "blue", col_nonpreferred = "orange", col_neutral = "white")
  expect_equal(colors2$left[1], "blue")
  expect_equal(colors2$right[1], "orange")
  expect_equal(colors2$left[3], "white")
})
