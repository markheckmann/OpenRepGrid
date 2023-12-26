test_that("gridlist works correctly", {
  gl <- gridlist(boeker, feixas2004)
  expect_s3_class(gl, "gridlist")
  expect_length(gl, 2)

  gl2 <- rep(boeker, n = 3)
  expect_s3_class(gl2, "gridlist")
  expect_length(gl2, 3)

  expect_error(rep.repgrid(1, n = 4), "'x' must be a 'repgrid' object")

  expect_error(gridlist(1, 2), "All element of 'x' must be 'repgrid' objects")

  expect_equal(gl, as.gridlist(gl))
  expect_error(as.gridlist(c(1, 2)), "'x' must be a list.")

  expect_true(is.gridlist(gl))
  expect_false(is.gridlist(list()))
  expect_false(is.gridlist(1:3))

  dim_expected <- list(constructs = c(15, 13), elements = c(14, 20))
  expect_equal(dim(gl), dim_expected)

  expect_output(print(gl, all = TRUE))
  expect_output(print(gl),
    regexp = NULL,
    "length: 2", "no of constructs [min, max]: [13, 15]",
    "no of elements [min, max]: [14, 20]"
  )
  expect_error(print.gridlist(list(), "'x'must be a 'gridlist' object"))
})
