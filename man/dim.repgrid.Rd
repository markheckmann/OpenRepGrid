\name{dim.repgrid}
\alias{dim.repgrid}
\title{Return size of a grid.}
\usage{
  \method{dim}{repgrid} (x)
}
\arguments{
  \item{x}{\code{repgrid} object.}
}
\value{
  Numeric vector of length two with the number of
  constructs and elements.
}
\description{
  \code{dim} returns a numeric vector of length two
  containing the number of constructs and elements.
}
\examples{
\dontrun{

     dim(bell2010)

}
}
\author{
  Mark Heckmann
}
\seealso{
  \code{\link{getNoOfConstructs}};
  \code{\link{getNoOfElements}}
}
\keyword{internal}

