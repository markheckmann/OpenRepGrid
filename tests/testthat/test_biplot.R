library(testthat)
library(vdiffr)

library(vdiffr)

test_that("biplots work", {
  create_biplot2d <- function() {
    set.seed(0)
    biplot2d(boeker)
  }
  expect_doppelganger("biplot2d", create_biplot2d)

  create_biplotPseudo3d <- function() {
    set.seed(0)
    biplotPseudo3d(boeker)
  }
  expect_doppelganger("biplotPseudo3d", create_biplotPseudo3d)
})
