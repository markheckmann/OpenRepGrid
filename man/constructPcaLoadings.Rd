\name{constructPcaLoadings}
\alias{constructPcaLoadings}
\title{Extract loadings from PCA of constructs.}
\usage{
  constructPcaLoadings(x)
}
\arguments{
  \item{x}{\code{repgrid} object. This object is returned
  by the function \code{\link{constructPca}}.}
}
\value{
  A matrix containing the factor loadings.
}
\description{
  Extract loadings from PCA of constructs.
}
\examples{
p <- constructPca(bell2010)
 l <- constructPcaLoadings(p)
 l[1, ]
 l[, 1]
 l[1,1]
}

