\name{center}
\alias{center}
\title{Centering of rows (constructs) and/or columns (elements).}
\usage{
  center(x, center = 1, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{center}{Numeric. The type of centering to be
  performed.  0= no centering, 1= row mean centering
  (construct), 2= column mean centering (elements), 3=
  double-centering (construct and element means), 4=
  midpoint centering of rows (constructs).  of the
  scale(default \code{FALSE}). Default is \code{1} (row
  centering).}

  \item{...}{Not evaluated.}
}
\value{
  \code{matrix} containing the transformed values.
}
\description{
  Centering of rows (constructs) and/or columns (elements).
}
\note{
  If scale midpoint centering is applied no row or column
  centering can be applied simultaneously.  TODO: After
  centering the standard representation mode does not work
  any more as it remains unclear what color values to
  attach to the centered values.
}
\examples{
\dontrun{

 center(bell2010)						      # no centering
 center(bell2010, rows=T)				  # row centering of grid
 center(bell2010, cols=T)				  # column centering of grid
 center(bell2010, rows=T, cols=T)	# row and column centering
}
}
\author{
  Mark Heckmann
}

