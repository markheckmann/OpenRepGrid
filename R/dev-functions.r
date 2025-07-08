######################### Developer's functions  ##############################
###   Functions that can be used by developers. Often these are             ###
###   are simple wrappers to access slots of the repgrid object.            ###
###   They are supposd to facilitate adding new functions for               ###
###   newcomers to R.                                                       ###
###   Most functions are documentetd in OpenRepGrid-internal                ###
# //////////////////////////////////////////////////////////////////////////////


#' Generate a random grid (quasis) of prompted size.
#'
#' This feature is useful for research purposes like
#' exploring distributions of indexes etc.
#'
#' @param nc        Number of constructs (default 10).
#' @param ne        Number of elements (default 15).
#' @param nwc       Number of random words per construct.
#' @param nwe       Number of random words per element.
#' @param range     Minimal and maximal scale value (default `c(1, 5)`).
#' @param prob      The probability of each rating value to occur.
#'                  If `NULL` (default) the distribution is uniform.
#' @param options   Use random sentences as constructs and elements (1) or
#'                  not (0). If not, the elements and constructs are given
#'                  default names and are numbered.
#' @param preferred Add preferred pole info? (default `TRUE`)
#' @return `repgrid` object.
#'
#' @export
#' @examples \dontrun{
#' x <- randomGrid()
#' x
#' x <- randomGrid(10, 25, preferred = FALSE)
#' x
#' x <- randomGrid(10, 25, options = 0)
#' x
#' }
#'
randomGrid <- function(nc = 10, ne = 15, nwc = 8, nwe = 5, range = c(1, 5), prob = NULL, options = 1, preferred = TRUE) {
  if (options == 1) { # full constructs and element names
    elem <- randomSentences(ne, nwe)
    left <- randomSentences(nc, nwc)
    right <- randomSentences(nc, nwc)
  } else { # short element and construct names
    elem <- paste("element", seq_len(ne))
    left <- paste("left pole", seq_len(nc))
    right <- paste("right pole", seq_len(nc))
  }
  scores <- sample(range[1]:range[2], nc * ne, replace = TRUE, prob = prob)
  preferred_pole <- sample(c("left", "right", NA_character_), replace = TRUE, size = nc, prob = c(.7, .2, .1))
  args <- list(
    name = elem,
    l.name = left,
    r.name = right,
    scores = scores,
    preferred_pole = if (preferred) preferred_pole else NULL
  )
  x <- makeRepgrid(args)
  setScale(x, min = range[1], max = range[2])
}


#' Generate a list of random grids (quasis) of prompted size.
#'
#' This feature is useful for research purposes like
#' exploring distributions of indexes etc. The function is a
#' simple wrapper around [randomGrid()].
#'
#' @param rep       Number of grids to be produced (default is `3`).
#' @param nc        Number of constructs (default 10).
#' @param ne        Number of elements (default 15).
#' @param nwc       Number of random words per construct.
#' @param nwe       Number of random words per element.
#' @param range     Minimal and maximal scale value (default `c(1, 5)`).
#' @param prob      The probability of each rating value to occur.
#'                  If `NULL` (default) the distribution is uniform.
#' @param options   Use random sentences as constructs and elements (1) or
#'                  not (0). If not, the elements and constructs are given
#'                  default names and are numbered.
#' @return    A list of `repgrid` objects.
#'
#' @export
#' @examples \dontrun{
#'
#' x <- randomGrids()
#' x
#' x <- randomGrids(5, 3, 3)
#' x
#' x <- randomGrids(5, 3, 3, options = 0)
#' x
#' }
#'
randomGrids <- function(rep = 3, nc = 10, ne = 15, nwc = 8, nwe = 5,
                        range = c(1, 5), prob = NULL, options = 1) {
  replicate(rep, randomGrid(
    nc = nc, ne = ne,
    nwc = nwc, nwe = nwe,
    range = range, prob = prob,
    options = options
  ))
}


#' Generate random grids and calculate 'Slater distances'
#' for the elements.
#'
#' All Slater distances are returned as a vector. The values can be used e.g. to assess the
#' distributions standard deviation.
#'
#' @param reps      Number of grids to be produced (default is `3`).
#' @param nc        Number of constructs (default 10).
#' @param ne        Number of elements (default 15).
#' @param range     Minimal and maximal scale value (default `c(1, 5)`).
#' @param prob      The probability of each rating value to occur.
#'                  If `NULL` (default) the distribution is uniform.
#' @param progress  Whether to show a progress bar.
#'
#' @return    A vector containing Slater distance values.
#' @keywords        internal
#' @export
#' @seealso [randomGrids()];
#'          [distanceSlater()];
#'          [distanceHartmann()].
#'
#' @examples \dontrun{
#'
#' vals <- quasiDistributionDistanceSlater(100, 10, 10, c(1, 5), pro = T)
#' vals
#' sd(vals)
#' hist(vals, breaks = 50)
#' }
#'
quasiDistributionDistanceSlater <- function(reps, nc, ne, range, prob = NULL, progress = TRUE) {
  quasis <- randomGrids(rep, nc = nc, ne = ne, range = range, prob = prob, options = 0)
  if (progress) { # whether to show progress bar
    lapply_fun <- lapply_pb
  } else {
    lapply_fun <- lapply
  }
  quasis.sd <- lapply_fun(quasis, function(x) {
    ds <- distanceSlater(x)
    ds[lower.tri(ds, diag = FALSE)]
  })
  unlist(quasis.sd) # return as vector
}


#' Generate a list with all possible construct reflections of a grid.
#'
#' @param x           `repgrid` object.
#' @param progress    Whether to show a progress bar (default is `TRUE`).
#'                    This may be sensible for a larger number of elements.
#' @return  A list of `repgrid` objects with all possible permutations
#'                    of the grid.
#'
#' @export
#' @examples \dontrun{
#'
#' l <- permuteConstructs(mackay1992)
#' l
#' }
#'
permuteConstructs <- function(x, progress = TRUE) {
  reflections <- rep(list(0:1), getNoOfConstructs(x))
  perm <- expand.grid(reflections) # all permutations
  if (progress) {
    apply_used <- apply_pb
  } else {
    apply_used <- apply
  }
  permGridList <- apply_used(perm, 1, function(perm, x) { # make grids with all possible reflections
    perm <- as.logical(perm)
    ncons <- seq_len(getNoOfConstructs(x))
    swapPoles(x, ncons[perm])
  }, x)
  permGridList
}


#' Permute rows, columns or whole grid matrix.
#'
#' Generate one or many permutations of the grid by shuffling
#' the rows, the columns or the whole grid matrix.
#'
#' @param x A `repgrid` object.
#' @param along What to permute. `along=1` (default) will permute the rows
#'  `along=2` the columns, `along=3` the whole matrix.
#' @param n The number of permutations to produce.
#' @returns  A `repgrid` object if `n = 1` or a list of `repgrid` objects if `n > 1`.
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#' # permute grid
#' permuteGrid(bell2010)
#' permuteGrid(bell2010)
#' permuteGrid(bell2010)
#'
#' # generate a list of permuted grids
#' permuteGrid(bell2010, n = 5)
#' }
#'
permuteGrid <- function(x, along = 1, n = 1) {
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }

  permuteGridInternal <- function(x, along = 1) {
    sc <- getRatingLayer(x)
    if (along == 1) {
      res <- t(apply(sc, 1, sample))
    }
    if (along == 2) {
      res <- apply(sc, 2, sample)
    }
    if (along == 0) {
      res <- sample(sc)
    }
    x[, ] <- res
    x
  }
  # generate n permuted grids
  res <- replicate(n, permuteGridInternal(x = x, along = along))
  # return repgrid object no list if n=1
  if (n == 1) {
    res <- res[[1]]
  }
  res
}
