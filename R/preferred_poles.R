
preferredPoles <- function(x) {
  stop_if_not_is_repgrid(x)
  left_is_preferred <- sapply(x@constructs, function(c) c$leftpole$preferred)
  right_is_preferred <- sapply(x@constructs, function(c) c$rightpole$preferred)
  case_when(
    left_is_preferred & !right_is_preferred ~ "left",
    !left_is_preferred & right_is_preferred ~ "right",
    left_is_preferred & right_is_preferred ~ "both",
    !left_is_preferred & !right_is_preferred ~ "none",
    .default = NA_character_
  )
}


`preferredPoles<-` <- function(x, value) {
  stop_if_not_is_repgrid(x)
  nc <- nrow(x)
  value <- rep_len(value, length.out = nc)
  value <- as.character(value) # all NA case
  value <- match.arg(value, c("left", "right", "none", "both", NA_character_), several.ok = TRUE)
  left_preferred <- value %in% c("left", "both")
  right_preferred <- value %in% c("right", "both")
  is.na(left_preferred) <- is.na(value)
  is.na(right_preferred) <- is.na(value)
  ii <- seq_len(nc)
  for (i in ii) {
    x@constructs[[i]]$leftpole$preferred <- left_preferred[i]
    x@constructs[[i]]$rightpole$preferred <- right_preferred[i]
  }
  x
}


# short form preference indicators for repgrid show method
preferred_indicators <- function(x) {
  preferred <- preferredPoles(x)
  indicators_left <- case_when(
    preferred == "left" ~ "+",
    preferred == "both" ~ "+",
    preferred == "none" ~ "/",
    preferred == "right" ~ "-",
    is.na(NA) ~ "."
  )
  indicators_right <- case_when(
    preferred == "left" ~ "-",
    preferred == "both" ~ "+",
    preferred == "none" ~ "/",
    preferred == "right" ~ "+",
    is.na(NA) ~ "."
  )
  list(left = indicators_left, right = indicators_right)
}


#' Set preferred pole by ideal element
#'
#' The preferred construct pole is inferred from the rating of the ideal element.
#' The preferred pole is the side of the ideal element. If the ideal is rated on the
#' scale midpoint (or within `none_range`), none of the poles is preferred.
#'
#' @param x A `repgrid` object.
#' @param ideal Index or name of ideal element.
#' @param none_range Range of ratings that do not allow assining a preferred pole (`NULL` be default).
#' @param align Align preferred poles on same side (default `FALSE`). See [alignByPreferredPoles()].
#' @export
preferredPolesByIdeal <- function(x, ideal, none_range = NULL, align = FALSE) {
  stop_if_not_is_repgrid(x)
  stop_if_not_in_element_range(x, ideal)

  midpoint <- getScaleMidpoint(x)
  sc <- getScale(x)
  if (is.null(none_range)) {
    none_range <- midpoint
  } else {
    stop_if_not_integerish(none_range, arg = "none_range")
  }

  idealRatings <- ratings(x)[, ideal]
  preferred_pole <- case_when(
    idealRatings %in% none_range ~ "none",
    idealRatings == midpoint ~ "none",
    idealRatings > midpoint ~ "right",
    idealRatings < midpoint ~ "left",
    .default = NA_character_
  )
  preferredPoles(x) <- preferred_pole

  if (align) {
    x <- alignByPreferredPoles(x)
  }
  x
}


#' Align constructs by preferred pole
#'
#' The direction of the constructs in a grid is arbitrary. While their reversal (see [reverse()]) does not affect the information
#' contained in the grid, it is often useful to align constructs for easier interpretation. One way of alignment
#' is placing all positive poles on the same side. Note that this this is only possible if the preferred poles
#' are defined (see [preferredPoles()]).
#'
#' @param x A `repgrid` object.
#' @param side_positive Align all positoive poles on '
#' @return A `repgrid` object with aligned constructs.
#' @export
#' @seealso [alignByLoadings()]
#' @examples
#' #TBD
alignByPreferredPole <- function(x, side = "right") {
  stop_if_not_is_repgrid(x)
  side <- match.arg(side, c("left", "right"))
  preferred_poles <- preferredPoles(x)
  ii_na <- is.na(preferred_poles)
  if (any(ii_na)) {
    warning(c("Some construct do not have a preferred pole and were not aligned.\n",
              "See 'preferredPoles() to set a preference'"), call. = FALSE)
  }
  if (side == "left") {
    ii_reverse <- preferred_poles == "right"
  } else {
    ii_reverse <- preferred_poles == "left"
  }
  ii_reverse <- which(ii_reverse)
  if (length(ii_reverse) > 0) {
    x <- reverse(x, ii_reverse)
  }
  x
}
