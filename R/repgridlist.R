
##
## repgridlist object: a list of repgrid objects 
##


#' Test or create object of class \code{repgridlist}
#' @export
#' @rdname repgridlist
is.repgridlist <- function(x) {
  inherits(x, "repgridlist")
}


#' @export
#' @rdname repgridlist
as.repgridlist <- function(x) 
{
  if (is.repgridlist(x))
    return(x)
  if (!is.list(x))
    stop("'x' must be a list.", call. = FALSE)
  ii <- vapply(x, is.repgrid, logical(1))
  if (!all(ii)) 
    stop("All element of 'x' must be 'repgrid' objects", call. = FALSE)
  
  class(x) <- c("repgridlist", class(x))
  x
}


#' Number of constructs and elements all repgrids in repgridlist
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
dim.repgridlist <- function(x) {
  list(
    constructs = vapply(x, ncol, numeric(1)),
    elements = vapply(x, nrow, numeric(1))
  )
}


#' Print method for repgridlist obejcts
#' 
#' @param x A \code{repgridlist} object.
#' @param all Display all repgrids in console?
#' @export
#' @keywords internal
#' 
print.repgridlist <- function(x, all = FALSE) 
{
  if (!is.repgridlist(x))
    stop("'x'muste be a 'repgridlist' object")
  if (all) {
    class(x) <- "list"
    return(x)
  }
  n <- length(x)
  nc <- nrow(x)[[1]]   # because a list of length 1 is returned
  ne <- ncol(x)[[1]]
  cat("repgridlist (list of repgrid objects):")
  cat("\n  length:", n)
  cat("\n  no of constructs: [", min(nc), ", ", max(nc), "]", sep = "")
  cat("\n  no of elements: [", min(ne), ", ", max(ne), "]", sep = "")
}



