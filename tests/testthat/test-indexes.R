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
  expect_equal(id$no_ids, 4)
})

