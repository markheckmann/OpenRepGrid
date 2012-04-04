\name{distanceSlater}
\alias{distanceSlater}
\title{'Slater distances' (standardized Euclidean distances).}
\usage{
  distanceSlater(x, trim = 10, indexcol = FALSE,
    digits = 2, output = 1, upper = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{trim}{The number of characters a element names are
  trimmed to (default is \code{10}). If \code{NA} no
  trimming is done. Trimming simply saves space when
  displaying the output.}

  \item{indexcol}{Logical. Whether to add an extra index
  column so the column names are indexes instead of element
  names. This option renders a neater output as long
  element names will stretch the output (default is
  \code{FALSE}). Note that the index column is the first
  matrix column.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{output}{The output type. The default
  (\code{output=1}) will print the Slater distances to the
  console.  \code{output=0} will suppress the printing.  In
  both cases a matrix list containig the results of the
  calculations is returned invisibly.}

  \item{upper}{Logical. Whether to display only upper part
  of the distance matrix (default \code{TRUE}).}
}
\value{
  A matrix is returned invisibly.
}
\description{
  Calculate Slater distance.
}
\details{
  The euclidean distance is often used as a measure of
  similarity between elements (see \code{\link{distance}}.
  A drawback of this measure is that it depends on the
  range of the rating scale and the number of constructs
  used, i. e. on the size of a grid. An approach to
  standardize the euclidean distance to make it independent
  from size and range of ratings and was proposed by Slater
  (1977, pp. 94). The 'Slater distance' is the Euclidean
  distance divided by the expected distance. Slater
  distances bigger than 1 are greater than expected, lesser
  than 1 are smaller than expected. The minimum value is 0
  and values bigger than 2 are rarely found. Slater
  distances have been be used to compare inter-element
  distances between different grids, where the grids do not
  need to have the same constructs or elements. Hartmann
  (1992) showed that Slater distance is not independent of
  grid size. Also the distribution of the Slater distances
  is asymmetric. Hence, the upper and lower limit to infer
  'significance' of distance is not symmetric. The
  practical relevance of Hartmann's findings have been
  demonstrated by Schoeneich and Klapp (1998). To calculate
  Hartmann's version of the standardized distances see
  \code{\link{distanceHartmann}}

  The Slater distance is calculated as follows. For a
  derivation see Slater (1977, p.94).  \cr Let matrix
  \eqn{D}{D} contain the row centered ratings. Then \deqn{P
  = D^TD}{P = D^TD} and \deqn{S = tr(P)}{S = tr(P)} The
  expected 'unit of expected distance' results as \cr
  \deqn{U = (2S/(m-1))^{1/2}}{U = (2S/(m-1))^.5} where
  \eqn{m}{m} denotes the number of elements of the grid.
  The standardized Slater distances is the matrix of
  Euclidean distances \eqn{E}{E} devided by the expected
  distance \eqn{U}{U}. \deqn{E/U}{E/U}
}
\examples{
\dontrun{

   distanceSlater(bell2010)
   distanceSlater(bell2010, upper=F)
   distanceSlater(bell2010, trim=40, index=T)

   d <- distanceSlater(bell2010, out=0, digits=4)
   d
}
}
\author{
  Mark Heckmann
}
\references{
  Hartmann, A. (1992). Element comparisons in repertory
  grid technique: Results and consequences of a Monte Carlo
  study. \emph{International Journal of Personal Construct
  Psychology, 5}(1), 41-56.

  Schoeneich, F., & Klapp, B. F. (1998). Standardization of
  interelement distances in repertory grid technique and
  its consequences for psychological interpretation of
  self-identity plots: An empirical study.  \emph{Journal
  of Constructivist Psychology, 11}(1), 49-58.

  Slater, P. (1977). \emph{The measurement of intrapersonal
  space by Grid technique.} Vol. II. London: Wiley.
}
\seealso{
  \code{\link{distanceHartmann}}
}

