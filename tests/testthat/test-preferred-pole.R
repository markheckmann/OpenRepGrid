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

