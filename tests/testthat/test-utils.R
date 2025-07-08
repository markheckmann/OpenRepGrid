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


test_that("fortify_element_*", {
  x <- boeker
  nr <- nrow(x)
  ee <- elements(x)
  for (i in seq_len(nr)) {
    e <- ee[i]
    expect_equal(fortify_element_id(x, i), i)
    expect_equal(fortify_element_id(x, e), i)
    expect_equal(fortify_element_name(x, i), e)
    expect_equal(fortify_element_name(x, e), e)
  }

  expect_error(fortify_element_id(x, -1))
  expect_error(fortify_element_name(x, "xxx"))
})


test_that("get_names_na*", {
  l1 <- as.list(1:3)
  expect_equal(get_names_na(l1), rep(NA_character_, 3))

  l2 <- list(a = 1, b = 2, 3)
  expect_equal(get_names_na(l2), c("a", "b", NA))

  l3 <- setNames(list(1, 2, 3), c("x", "", "z"))
  expect_equal(get_names_na(l3), c("x", NA, "z"))
})
