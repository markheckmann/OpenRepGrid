\name{alignByIdeal}
\alias{alignByIdeal}
\title{Align constructs using the ideal element to gain pole preferences.}
\usage{
  alignByIdeal(x, ideal, high = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object}

  \item{ideal}{Number of the element that is used for
  alignment (the ideal).}

  \item{high}{Logical. Whether to align the constructs so
  the ideal will have high ratings on the constructs (i.e.
  \code{TRUE}, default) or low ratings (\code{FALSE}). High
  scores will lead to the preference pole on the right
  side, low scores will align the preference pole on the
  left side.}
}
\value{
  \code{repgrid} object with aligned constructs.
}
\description{
  The direction of the constructs in a grid is arbitrary
  and a reflection of a scale does not affect the
  information contained in the grid. Nonetheless, the
  direction of a scale has an effect on inter-element
  correlations (Mackay, 1992) and on the spatial
  representation and clustering of the grid (Bell, 2010).
  Hence, it is desirable to follow a protocol to align
  constructs that will render unique results. A common
  approach is to align constucts by pole preference, i. e.
  aligninig all positive and negative poles. This can e. g.
  be achieved using \code{\link{swapPoles}}. If an ideal
  element is present, this element can be used to identify
  the positive and negative pole. The function
  \code{alignByIdeal} will align the constructs
  accordingly. Note that this approach does not always
  yield definite results as sometimes ratings do not show a
  clear preference for one pole (Winter, Bell & Watson,
  2010). If a preference cannot be determined definitely,
  the construct direction remains unchanged (a warning is
  issued in that case).
}
\examples{
\dontrun{

  feixas2004                         # original grid
  alignByIdeal(feixas2004, 13)       # aligned with preference pole on the right

  raeithel                           # original grid
  alignByIdeal(raeithel, 3, high=F)  # aligned with preference pole on the left

}
}
\author{
  Mark Heckmann
}
\references{
  Bell, R. C. (2010). A note on aligning constructs.
  \emph{Personal Construct Theory & Practice, 7}, 42-48.

  Mackay, N. (1992). Identification, Reflection, and
  Correlation: Problems in ihe bases of repertory grid
  measures. \emph{International Journal of Personal
  Construct Psychology, 5}(1), 57-75.

  Winter, D. A., Bell, R. C., & Watson, S. (2010). Midpoint
  ratings on personal constructs: Constriction or the
  middle way? \emph{Journal of Constructivist Psychology,
  23}(4), 337-356.
}
\seealso{
  \code{\link{alignByLoadings}}
}

