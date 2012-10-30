\name{slaterStandardization}
\alias{slaterStandardization}
\title{Internal workhorse for Slater standardization.}
\usage{
  slaterStandardization(x)
}
\arguments{
  \item{x}{A matrix.}
}
\description{
  Function uses a matrix as input. All overhead of
  \code{repgrid} class is avoided. Needed for speedy
  simulations.
}
\keyword{internal}

