require(openxlsx)


test_that("saveAsExcel", {
  file <- saveAsExcel(boeker, tempfile(fileext = ".xlsx"))
  expect_equal(getSheetNames(file), "grid")

  rgs <- randomGrids(nc = 3)
  file <- saveAsExcel(rgs, tempfile(fileext = ".xlsx"))
  expect_equal(getSheetNames(file), paste("grid", 1:3))

  rgs <- randomGrids(nc = 3)
  file <- saveAsExcel(rgs, tempfile(fileext = ".xlsx"), sheet = "xxxx")
  expect_equal(getSheetNames(file), paste("xxxx", 1:3))

  names(rgs) <- c("my grid", NA, NA)
  file <- saveAsExcel(rgs, tempfile(fileext = ".xlsx"))
  expect_equal(getSheetNames(file), c("my grid", "grid 2", "grid 3"))
})


test_that("saveAsWorkbook", {
  wb <- createWorkbook()
  saveAsWorksheet(boeker, wb)
  expect_equal(names(wb), "grid")

  wb <- createWorkbook()
  rgs <- randomGrids(nc = 3)
  saveAsWorksheet(rgs, wb)
  expect_equal(names(wb), paste("grid", 1:3))

  wb <- createWorkbook()
  names(rgs) <- c("my grid", NA, NA)
  saveAsWorksheet(rgs, wb, sheet = "xxxx")
  expect_equal(names(wb), c("my grid", "xxxx 2", "xxxx 3"))

  wb <- createWorkbook()
  saveAsWorksheet(boeker, wb)
  expect_error(saveAsWorksheet(boeker, wb), "Worksheet ['\"]grid['\"] already exists") # windows renders \"
})
