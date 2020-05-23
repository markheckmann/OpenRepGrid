# Test for for indexes to return correct values, e.g.
# tested against other software


test_that("PVAFF returns expected value", {
  v <- indexPvaff(boeker)
  expect_equal(v, 0.42007)
})


test_that("indexDilemma matches Gridcor results", {
  id <- indexDilemma(boeker, self = 1, ideal = 2, r.min = .35)
  expect_equal(id$no_ids, 4)
  
  #zero dilemmas do not cause error
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
    x <- indexSelfConstruction(feixas2004, 1, 13, others = c(3,3,4))
  })
  # rounding of 'others' element works correctly
  a <- indexSelfConstruction(feixas2004, self = 1, ideal = 13, round = FALSE)
  b <- indexSelfConstruction(feixas2004, self = 1, ideal = 13, round = TRUE)
  expect_true({
    all(round(ratings(a$grid)) == ratings(b$grid))
  })

})

