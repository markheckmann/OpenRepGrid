# basic operations

test_that("addAvgElement works correctly", {
  # average of one element yields same values
  x <- addAvgElement(feixas2004, "AVG", i = 1)
  R <- ratings(x[, c(1, 14)])
  expect_equal(R[, 1], R[, 2])

  # error is thrown if i is out of range
  expect_error({
    x <- addAvgElement(feixas2004, "AVG", i = 0)
  })
  expect_error({
    x <- addAvgElement(feixas2004, "AVG", i = 16)
  })
  expect_error({
    x <- addAvgElement(feixas2004, "AVG", i = 1:16)
  })
  # existing element name
  expect_error({
    x <- addAvgElement(feixas2004, "Mother", i = 1:16)
  })

  # duplicate indexes generate warning
  expect_warning({
    x <- addAvgElement(feixas2004, "AVG", i = c(1, 1, 2))
  })
})


test_that("stop_if_not_is_repgrid works correctly", {
  expect_error(
    stop_if_not_is_repgrid("a")
  )

  expect_error(
    stop_if_not_is_repgrid("a", "some_name")
  )
})


test_that("reverse works correctly", {
  expect_equal(swapPoles(boeker), reverse(boeker))

  ii <- c(1, 4, 6)
  expect_equal(swapPoles(boeker, ii), reverse(boeker, ii))
})



test_that("extract element by name", {
  x <- boeker[, 1:2]
  y <- boeker[, c("self", "ideal self")]
  expect_equal(x, y)

  ii <- sample(ncol(boeker))
  el <- elements(boeker)[ii]
  x <- boeker[, ii]
  y <- boeker[, el]
  expect_equal(x, y)

  expect_error(boeker[, c("xxx", "yyy")], regex = "Unknown elements: xxx, yyy")
})


test_that("cbind and `/`", {
  x <- boeker[, 1:2]
  y <- boeker[, 5:6]
  z <- boeker[, c(1:2, 5)]
  y_reordered <- y[sample(nrow(y)), ]
  y_swapped <- swapPoles(y)
  x_scale <- setScale(x, max = 8)

  expect_error(cbind(x, x_scale), regexp = "x and y must have identical scale ranges.")
  expect_no_error(xy_1 <- cbind(x, y))
  expect_no_error(xy_2 <- x / y)
  expect_no_error(xx_1 <- x / x)
  xy_3 <- cbind(x, y_reordered)
  expect_equal(xy_1, xy_2)
  expect_equal(xy_1, xy_3)

  expect_error(cbind(x, y_reordered, .reorder = FALSE), regexp = "Constructs in x and y have different orders.")
  expect_error(cbind(x, y_swapped), regexp = "x and y have different constructs.")
  expect_error(cbind(x, z, .unique = TRUE), regexp = "x and y have common elements.")
})
