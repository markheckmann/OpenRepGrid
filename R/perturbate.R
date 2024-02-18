## perturbate grid data


#' Perturbate grid ratings
#'
#' Randomly subtract or add an amount to a proportion of the grid ratings. This
#' emulates randomness during the rating process, producing a grid which might
#' also have resulted.
#'
#' @param x A `repgrid` object.
#' @param n Number of perturbated grid to generate.
#' @param prop The proportion of ratings to be perturbated.
#' @param amount The amount set of possible perturbations. Will depend on scale
#'   range. Usually `{-1, 1}` are reasonable settings.
#' @param prob Probability for each amount to occur.
#' @export
#' @example inst/examples/example-perturbate.R
#' @rdname perturbate
#'
perturbate <- function(x, prop = .1, amount = c(-1, 1), prob = c(.5, .5)) {
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }

  if (length(amount) != length(prob)) {
    stop("Length of 'amount' and 'prob' must be the same. ",
      "Each entry in amount must have a corresponding prob value.",
      call. = FALSE
    )
  }

  sc <- getScale(x) # min, max of rating scale
  smin <- sc[1]
  smax <- sc[2]
  r <- ratings(x)
  N <- length(r)
  n_sample <- floor(prop * N)
  ii <- sample(seq_len(N), size = n_sample, replace = FALSE)

  # perturbate grid rating by given amounts and probablity for each amount.
  # Values lower or higher thahn scale range are set back to min and max of scale range.
  perturbations <- sample(amount, size = n_sample, replace = TRUE, prob = prob)
  new_values <- r[ii] + perturbations
  new_values <- ifelse(new_values < smin, smin, new_values)
  new_values <- ifelse(new_values > smax, smax, new_values)
  r[ii] <- new_values
  ratings(x) <- r
  x
}


#' @export
#' @rdname perturbate
#'
grids_perturbate <- function(x, n = 10, prop = .1, amount = c(-1, 1), prob = c(.5, .5)) {
  l <- replicate(n,
    {
      perturbate(x, prop = prop, amount = amount, prob = prob)
    },
    simplify = FALSE
  )
  as.gridlist(l)
}
