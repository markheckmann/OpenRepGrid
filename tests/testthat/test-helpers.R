test_that("stop_if_not_in_element_range", {

  expect_no_error({
    stop_if_not_in_element_range(boeker, 1)
    stop_if_not_in_element_range(boeker, "self")
  })

  expect_error(stop_if_not_in_element_range(boeker, -1))
  expect_error(stop_if_not_in_element_range(boeker, 16))
  expect_error(stop_if_not_in_element_range(boeker, NA))
  expect_error(stop_if_not_in_element_range(boeker, "xxx"))
  expect_error(stop_if_not_in_element_range(boeker, 1.1))
})



