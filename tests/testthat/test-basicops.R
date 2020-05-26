# basic operations

test_that("addAvgElement works correctly", {
  
  # average of one element yields same values
  x <- addAvgElement(feixas2004, "AVG", i = 1)
  R <- ratings(x[, c(1,14)])
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
    x <- addAvgElement(feixas2004, "AVG", i = c(1,1,2))
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
  
