\name{permuteGrid}
\alias{permuteGrid}
\title{Permute rows, columns or whole grid matrix.}
\usage{
  permuteGrid(x, along = 1, n = 1)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{along}{What to permute. \code{along=1} (default)
  will permute the rows \code{along=2} the columns,
  \code{along=3} the whole matrix.}

  \item{n}{The number of permutations to produce.}
}
\value{
  A \code{repgrid} object if \code{n=1} or a list of
  \code{repgrid} objects if \code{n>1}.
}
\description{
  Generate one or many permutations of the grid by
  shuffling the rows, the columns or the whole grid matrix.
}
\examples{
\dontrun{

   # permute grid
   permuteGrid(bell2010)
   permuteGrid(bell2010)
   permuteGrid(bell2010)

   # generate a list of permuted grids
    permuteGrid(bell2010, n=5)

}
}
\author{
  Mark Heckmann
}
\keyword{internal}

