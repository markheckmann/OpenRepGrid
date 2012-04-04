\name{[<-,repgrid-method}
\alias{[<-,repgrid-method}
\title{Method for "<-" assignment of the repgrid ratings.}
\arguments{
  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}
}
\description{
  It should be possible to use it for ratings on all
  layers.
}
\examples{
\dontrun{
   x <- randomGrid()
   x[1,1] <- 2
   x[1, ] <- 4
   x[,2] <- 3
}
}
\author{
  Mark Heckmann
}
\keyword{internal}

