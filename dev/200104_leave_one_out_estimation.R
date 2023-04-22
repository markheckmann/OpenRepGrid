# simulating variation in grid statistics
# when resampling from the same grid



#' Resample constructs
#' 
#' The goal of resampling is to build variations of a single grid. 
#' Two variants are implemented: The first is the \emph{leave-n-out} approach which 
#' builds all possible grids when dropping n constructs. The second is a 
#' \emph{bootstrap} approach, randomly drawing n constructs from the grid.
#' 
#' @param n Number of constructs to drop or to sample in each generated grid. Defaults
#'   to 1 for \code{grids_leave_n_out} and to the number of constructs for
#'   \code{grids_bootstrap}.
#' @return List of grids.
#' @export
#' @rdname resampling
#' 
grids_leave_n_out <- function(x, n = 1)
{
  nc <- getNoOfConstructs(x)    # size construct system
  prop <- n / nc        # proportion left out
  if (prop > .4) 
    warning("Be aware that you leave more than 40% or more of the constructs", call. = FALSE)
  n.subset <- nc - n
  l <- combn(seq_len(nc), n.subset, simplify = F) # list of subset indexes
  l <- lapply(l, function(i, x) x[i, ], x = x)
  as.repgridlist(l)
}


#' @param reps Number of grids to generate.
#' @param replace Resample constructs with replacement?
#' @export
#' @rdname resampling
#' 
grids_bootstrap <- function(x, n = nrow(x), reps = 100, replace = TRUE)
{
  nc <- getNoOfConstructs(x)    # size construct system
  prop <- n / nc                # proportion to sample
  if (prop < .6) 
    warning("Be aware that you resample less than 60% of the constructs", call. = FALSE)
  l_i <- replicate(reps, sample(seq_len(nc), n, replace = replace), simplify = FALSE)
  l <- lapply(l_i, function(i, x) x[i, ], x = x)
  as.repgridlist(l)
}


#' Number of constructs and elements for grids in repgridlist
#' @return Numeric vector with the number of constructs or elements 
#'   of each grid in repgridlist object.
#' @export
#' @keywords internal
nrow.repgridlist <- function(x) {
  vapply(x, nrow, numeric(1))
}


#' @export
#' @keywords internal
ncol.repgridlist <- function(x) {
  vapply(x, ncol, numeric(1))
}



#### testing / examples  ------------------------------------------------------

library(OpenRepGrid)
library(tidyverse)



