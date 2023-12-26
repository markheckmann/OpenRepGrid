##
## gridlist object: a list of repgrid objects
##

#' Add repgrids into a gridlist
#' @param ... Objects to be converted into `gridlist`
#' @param x Any object.
#' @export
#' @rdname gridlist
gridlist <- function(...) {
  l <- list(...)
  as.gridlist(l)
}


#' Test or create object of class `gridlist`
#' @export
#' @rdname gridlist
is.gridlist <- function(x) {
  inherits(x, "gridlist")
}


#' @export
#' @rdname gridlist
as.gridlist <- function(x) {
  if (is.gridlist(x)) {
    return(x)
  }
  if (!is.list(x)) {
    stop("'x' must be a list.", call. = FALSE)
  }
  ii <- vapply(x, is.repgrid, logical(1))
  if (!all(ii)) {
    stop("All element of 'x' must be 'repgrid' objects", call. = FALSE)
  }

  class(x) <- c("gridlist", class(x))
  x
}


#' Number of constructs and elements all repgrids in gridlist
#'
#' @details `nrow` and `ncol` are also covered by this approach
#' as they access the dim method. The others are not generic
#' and making them so introduces warnings, which I try to avoid.
#'
#' @param x A gridlist object.
#' @return A list of length 2.
#' @export
#' @keywords internal
#'
dim.gridlist <- function(x) {
  list(
    constructs = vapply(x, ncol, numeric(1)),
    elements = vapply(x, nrow, numeric(1))
  )
}


#' Print method for gridlist objects
#'
#' @param x A `gridlist` object.
#' @param all Display all repgrids in console?
#' @export
#' @keywords internal
#'
print.gridlist <- function(x, all = FALSE, ...) {
  if (!is.gridlist(x)) {
    stop("'x'must be a 'gridlist' object")
  }
  if (all) {
    class(x) <- "list"
    print(x)
    return(invisible(NULL))
  }
  n <- length(x)
  nc <- nrow(x)[[1]] # because a list of length 1 is returned
  ne <- ncol(x)[[1]]
  cat("gridlist (list of repgrid objects):")
  cat("\n  length:", n)
  cat("\n  no of constructs [min, max]: [", min(nc), ", ", max(nc), "]", sep = "")
  cat("\n  no of elements [min, max]: [", min(ne), ", ", max(ne), "]", sep = "")
}


#' Replicate repgrid objects
#'
#' Implements the `rep` method for `repgrid` objects.
#'
#' @param x A `repgrid`` object.
#' @param n Number of times to replicate the grid.
#' @return A `gridlist`` object.
#' @export
#' @keywords internal
#'
#' @examples
#' l <- rep(boeker, 3) # gridlist with 3 boeker grids
rep.repgrid <- function(x, n = 1, ...) {
  if (!is.repgrid(x)) {
    stop("'x' must be a 'repgrid' object", call. = FALSE)
  }
  l <- replicate(n, x)
  as.gridlist(l)
}
