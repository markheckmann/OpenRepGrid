library(vdiffr)

test_that("bertin works", {
  . <- expect_doppelganger("bertin", bertin(feixas2004))

  create_bertinCluster <- function() {
    set.seed(0)
    h <- bertinCluster(feixas2004)
  }
  . <- expect_doppelganger("bertinCluster", create_bertinCluster)
})
