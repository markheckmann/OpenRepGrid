\name{permuteConstructs}
\alias{permuteConstructs}
\title{Generate a list with all possible construct reflections of a grid.}
\usage{
  permuteConstructs(x, progress = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{progress}{Whether to show a progress bar (default
  is \code{TRUE}).  This may be sensible for a larger
  number of elements.}
}
\value{
  A list of \code{repgrid} objects with all possible
  permutations of the grid.
}
\description{
  Generate a list with all possible construct reflections
  of a grid.
}
\examples{
\dontrun{

  l <- permuteConstructs(mackay1992)
  l

}
}
\author{
  Mark Heckmann
}

