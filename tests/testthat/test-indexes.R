# Test for for indexes to return correct values, e.g.
# tested against other software

test_that("PVAFF returns expected value", {
  v <- indexPvaff(boeker)
  expect_equal(v, 0.42007)
})
