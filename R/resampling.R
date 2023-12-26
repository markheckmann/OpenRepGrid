## Resampling of grid


#' Resample constructs
#'
#' The goal of resampling is to build variations of a single grid.
#' Two variants are implemented: The first is the *leave-n-out* approach which
#' builds all possible grids when dropping n constructs. The second is a
#' *bootstrap* approach, randomly drawing n constructs from the grid.
#' @param x A repgrid object.
#' @param n Number of constructs to drop or to sample in each generated grid.
#' @return List of grids.
#' @export
#' @rdname resampling
#' @example inst/examples/example-resampling.R
#'
grids_leave_n_out <- function(x, n = 0) {
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }
  nc <- getNoOfConstructs(x) # size construct system
  prop <- n / nc # proportion left out
  if (prop > .4) {
    warning("Be aware that you leave more than 40% or more of the constructs", call. = FALSE)
  }
  n.subset <- nc - n
  l <- combn(seq_len(nc), n.subset, simplify = F) # list of subset indexes
  l <- lapply(l, function(i, x) x[i, ], x = x)
  as.gridlist(l)
}


#' @param reps Number of grids to generate.
#' @param replace Resample constructs with replacement?
#' @export
#' @rdname resampling
#'
grids_bootstrap <- function(x, n = nrow(x), reps = 100, replace = TRUE) {
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }
  nc <- getNoOfConstructs(x) # size construct system
  prop <- n / nc # proportion to sample
  if (prop < .6) {
    warning("Be aware that you resample less than 60% of the constructs", call. = FALSE)
  }
  l_i <- replicate(reps, sample(seq_len(nc), n, replace = replace), simplify = FALSE)
  l <- lapply(l_i, function(i, x) x[i, ], x = x)
  as.gridlist(l)
}
