\name{sapply_pb}
\alias{sapply_pb}
\title{sapply with a progress bar}
\usage{
  sapply_pb(X, FUN, ...)
}
\arguments{
  \item{X}{see ?sapply for parameter explanation}

  \item{FUN}{see ?sapply}

  \item{...}{see ?sapply}
}
\value{
  list see ?sapply
}
\description{
  Can be used like standard base:::sapply. The ionly thing
  it does is create an additional progress bar.
}
\examples{
\dontrun{

   l <- sapply(1:20000, function(x) list(rnorm(1000)))
   head(sapply_pb(l, mean))

   # performance comparison
   l <- sapply(1:20000, function(x) list(rnorm(1000)))
   system.time(sapply(l, mean))
   system.time(sapply_pb(l, mean))

}
}
\author{
  Mark Heckmann
}
\seealso{
  \code{\link{sapply}}
}
\keyword{internal}

