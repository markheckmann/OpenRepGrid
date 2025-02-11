# elements must be different
# constructs must be the same



#'
#' #' @rdname ops-methods
#' #' @include repgrid.r
#' #' @export
#' setMethod(
#'   "/", signature(e1 = "repgrid", e2 = "repgrid"),
#'   definition = function(e1, e2) {
#'       rbind(e1, e2)
#'     }
#' )
