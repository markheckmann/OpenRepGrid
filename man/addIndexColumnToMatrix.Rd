\name{addIndexColumnToMatrix}
\alias{addIndexColumnToMatrix}
\title{add index column for neater colnames}
\usage{
  addIndexColumnToMatrix(x)
}
\arguments{
  \item{x}{\code{matrix} object}
}
\description{
  add index column for neater colnames
}
\examples{
\dontrun{
   x <- matrix(1:9, 3)
   colnames(x) <- rep("Long names that occupiy too much space", 3)
   rownames(x) <- rep("Some text", 3)
   addIndexColumnToMatrix(x)
}
}
\keyword{internal}

