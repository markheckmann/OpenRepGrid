# Test for for indexes to return correct values, e.g.
# tested against other software


test_that("PVAFF returns expected value", {
  suppressMessages({
    v <- indexPvaff(boeker)
  })
  expect_equal(v, 0.42007)
})


test_that("indexDilemma matches Gridcor results", {
  id <- indexDilemma(boeker, self = 1, ideal = 2, r.min = .35)
  expect_equal(id$no_ids, 4)

  # zero dilemmas do not cause error
  id <- indexDilemma(boeker, self = 1, ideal = 2, r.min = .99)
  expect_equal(id$no_ids, 0)
})


test_that("indexSelfConstruction works correctly", {
  # by default, other element are all except self and ideal
  x <- indexSelfConstruction(feixas2004, self = 1, ideal = 13)
  check <- length(x$other_elements) == ncol(feixas2004) - 2
  expect_true(check)

  # error is thrown if i is out of range
  expect_error({
    x <- indexSelfConstruction(feixas2004, 1, 14)
  })
  expect_error({
    x <- indexSelfConstruction(feixas2004, 0, 13)
  })
  expect_error({
    x <- indexSelfConstruction(feixas2004, 1, 13, others = 0)
  })
  # duplicate indexes not allowed
  expect_error({
    x <- indexSelfConstruction(feixas2004, 1, 13, others = c(3, 3, 4))
  })
  # rounding of 'others' element works correctly
  a <- indexSelfConstruction(feixas2004, self = 1, ideal = 13, round = FALSE)
  b <- indexSelfConstruction(feixas2004, self = 1, ideal = 13, round = TRUE)
  expect_true({
    all(round(ratings(a$grid)) == ratings(b$grid))
  })
})


test_that("indexDilemmatic works correctly", {
  # by default, other element are all except self and ideal
  x <- indexDilemmatic(feixas2004, ideal = 13)
  expect_true(x$n_dilemmatic == 1)

  # error is thrown if i is out of range
  expect_error({
    x <- indexDilemmatic(feixas2004, ideal = 14)
  })
  expect_error({
    x <- indexDilemmatic(feixas2004, ideal = 0)
  })

  # warn for even scale length
  expect_warning({
    x <- randomGrid(range = c(1, 6))
    indexDilemmatic(x, ideal = 1)
  })
  # warn for 0 deviation with long scale
  expect_warning({
    x <- randomGrid(range = c(0, 100))
    indexDilemmatic(x, ideal = 1)
  })
})


test_that("matches works correctly", {
  m <- matches(feixas2004)

  # error is thrown if i is out of range
  expect_error({
    m <- matches(feixas2004, deviation = -1)
  })
  expect_error({
    m <- matches(feixas2004, diag.na = NA)
  })
  expect_output(
    expect_error({
      print(m, width = -1)
    })
  )

  ## check results
  x <- feixas2004

  # maximal no of matches per C/E correct (infinity case gives max number)
  m <- matches(x, deviation = Inf, diag.na = FALSE)
  expect_true(all(m$elements == nrow(x)))
  expect_true(all(m$constructs == ncol(x)))

  # maximal possible number of matches is correct
  m <- matches(x, deviation = Inf)
  expect_true(m$max_constructs == sum(m$constructs, na.rm = T) / 2)
  expect_true(m$max_elements == sum(m$elements, na.rm = T) / 2)
})


test_that("indexBieri works correctly", {
  x <- feixas2004
  b <- indexBieri(x)

  # error is thrown if i is out of range
  expect_error({
    b <- matches(x, deviation = -1)
  })

  ## check results
  # maximal no of matches per C correct (infinity case gives max number)
  b <- indexBieri(x, deviation = Inf)
  expect_true(all(na.omit(b$constructs) == ncol(x)))
})



test_that("indexDDI works correctly", {
  files <- system.file("extdata", c("dep_grid_walker_1988_1.xlsx", "dep_grid_walker_1988_2.xlsx"), package = "OpenRepGrid")

  g_1 <- importExcel(files[1])
  g_2 <- importExcel(files[2])

  # check results against values from paper (Walker et al. (1988), p. 65 ff.)
  di <- indexDDI(g_1, 2:5)
  expected <- c(1.6, 1.8, 2.0, 2.0) # table 1
  expect_equal(round(di, 1), expected)

  di <- indexDDI(g_2, 2:5)
  expected <- c(1.9, 2.7, 3.3, 3.9) # table 2
  expect_equal(round(di, 1), expected)

  # negative values for ds are not allowed
  expect_error({
    indexDDI(x, ds = -1)
  })

  # only 0/1 ratings are allowed
  expect_error({
    indexDDI(boeker, ds = 2)
  })
})


test_that("indexUncertainty works correctly", {
  file <- system.file("extdata", "dep_grid_bell_2001.xlsx", package = "OpenRepGrid")
  g <- importExcel(file)

  # check results against values from paper (Bell, 2001 p.231, Fig.1)
  ui <- indexUncertainty(g)
  given <- round(ui, 2)
  expected <- c("Uncertainty Index" = .99)
  expect_equal(given, expected)

  # only 0/1 ratings are allowed
  expect_error({
    indexUncertainty(boeker)
  })
})
