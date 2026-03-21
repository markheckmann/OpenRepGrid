library(testthat)
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

  create_biplot2d_preferred <- function() {
    set.seed(0)
    x <- preferredPolesByIdeal(boeker, "ideal self")
    biplot2d(x, c.color.preferred = TRUE)
  }
  expect_doppelganger("biplot2d-preferred-colors", create_biplot2d_preferred)
})
