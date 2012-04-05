\docType{methods}
\name{[<-}
\alias{[<-}
\alias{[<-,repgrid-method}
\title{Method for "<-" assignment of the repgrid ratings.}
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

