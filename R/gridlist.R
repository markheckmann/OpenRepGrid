
##
## gridlist object: a list of repgrid objects 
##

#' Add repgrids into a gridlist
#' @export
#' @rdname gridlist
gridlist <- function(...) {
  l <- list(...)
  as.gridlist(l)
}


#' Test or create object of class \code{gridlist}
#' @export
#' @rdname gridlist
is.gridlist <- function(x) {
  inherits(x, "gridlist")
}


#' @export
#' @rdname gridlist
as.gridlist <- function(x) 
{
  if (is.gridlist(x))
    return(x)
  if (!is.list(x))
    stop("'x' must be a list.", call. = FALSE)
  ii <- vapply(x, is.repgrid, logical(1))
  if (!all(ii)) 
    stop("All element of 'x' must be 'repgrid' objects", call. = FALSE)
  
  class(x) <- c("gridlist", class(x))
  x
}


#' Number of constructs and elements all repgrids in gridlist
#' 
#' @param x A repgridist object.
#' @return A list of length 2.
#' @export
#' @keywords internal
#' 
#' \code{nrow} and \code{ncol} are also covered by this approach
#' as the access the dim method. The others are not generic
#' and makiong them so introduces warnings, which I try to avoid.
#' 
dim.gridlist <- function(x) {
  list(
    constructs = vapply(x, ncol, numeric(1)),
    elements = vapply(x, nrow, numeric(1))
  )
}


#' Print method for gridlist obejcts
#' 
#' @param x A \code{gridlist} object.
#' @param all Display all repgrids in console?
#' @export
#' @keywords internal
#' 
print.gridlist <- function(x, all = FALSE) 
{
  if (!is.gridlist(x))
    stop("'x'muste be a 'gridlist' object")
  if (all) {
    class(x) <- "list"
    return(x)
  }
  n <- length(x)
  nc <- nrow(x)[[1]]   # because a list of length 1 is returned
  ne <- ncol(x)[[1]]
  cat("gridlist (list of repgrid objects):")
  cat("\n  length:", n)
  cat("\n  no of constructs [min, max]: [", min(nc), ", ", max(nc), "]", sep = "")
  cat("\n  no of elements [min, max]: [", min(ne), ", ", max(ne), "]", sep = "")
}



