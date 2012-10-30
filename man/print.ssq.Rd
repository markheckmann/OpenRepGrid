\name{print.ssq}
\alias{print.ssq}
\title{Print method for class ssq.}
\usage{
  \method{print}{ssq} (x, digits = 2, dim = 5,
    cumulated = TRUE, ...)
}
\arguments{
  \item{x}{Object of class ssq.}

  \item{digits}{Number of digits to round the output to
  (default is \code{2}).}

  \item{dim}{The number of PCA dimensions to print. Default
  is \code{5} dimensions. \code{NA} will print all
  dimensions.}

  \item{cumulated}{Logical (default is \code{TRUE}).  Print
  a cumulated table of sum-of-squares?  If \code{FALSE} the
  uncumulated sum-of-squares are printed.  (default is
  \code{TRUE}).}

  \item{...}{Not evaluated.}
}
\description{
  Print method for class ssq.
}
\keyword{internal}

