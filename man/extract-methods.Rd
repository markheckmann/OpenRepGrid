\name{[,repgrid-method}
\alias{[,repgrid-method}
\title{Extract parts of the repgrid object.}
\arguments{
  \item{drop}{For matrices and arrays.  If \code{TRUE} the
  result is coerced to the lowest possible dimension (see
  the examples).  This only works for extracting elements,
  not for the replacement.  See \code{\link{drop}} for
  further details.  }
}
\description{
  Methods for \code{"["}, i.e., subsetting of repgrid
  objects.
}
\examples{
\dontrun{
   x <- randomGrid()
   x[1:4, ]
   x[ , 1:3]
   x[1:4,1:3]
   x[1,1]
}
}
\author{
  Mark heckmann
}
\keyword{internal}

