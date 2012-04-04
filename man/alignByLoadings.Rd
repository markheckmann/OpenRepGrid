\name{alignByLoadings}
\alias{alignByLoadings}
\title{Align constructs by loadings on first pricipal component.}
\usage{
  alignByLoadings(x, trim = 20, output = 0, digits = 1)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{10}). If \code{NA} no
  trimming is done. Trimming simply saves space when
  displaying the output.}

  \item{output}{Numeric. The function returns either the
  \code{repgrid} object with aligned constructs
  (\code{output=0 or 1}), additionaly prints the
  calculation results to the console (\code{output=1}) or
  returns a \code{list} containing the calculation results,
  i.e.  correlation matrices before and after reversal,
  loadings on first PC before and after reversal and a list
  (printout) of the constructs that have been reversed.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}
}
\value{
  Aligned \code{repgrid} object for \code{output=0 or 1} or
  a list for \code{output=2}.
}
\description{
  In case a construct loads negatively on the first
  principal component, the function \code{alignByLoadings}
  will reverse it so that all constructs have positive
  loadings on the first principal component (see deatil
  section for more).
}
\details{
  The direction of the constructs in a grid is arbitrary
  and a reflection of a scale does not affect the
  information contained in the grid. Nonetheless, the
  direction of a scale has an effect on inter-element
  correlations (Mackay, 1992) and on the spatial
  representation and clustering of the grid (Bell, 2010).
  Hence, it is desirable to follow a protocol to align
  constructs that will render unique results. A common
  approach is to align constructs by pole preference, but
  this information is not always accessible. Bell (2010)
  proposed another solution for the problem of construct
  alignment. As a unique protocol he suggests to align
  constructs in a way so they all have positive loadings on
  the first component of a grid PCA.
}
\note{
  Bell (2010) proposed a solution for the problem of
  construct alignment. As construct reversal has an effect
  on element correlation and thus on any measure that based
  on element correlation (Mackay, 1992), it is desireable
  to have a standard method for construct alignment
  independently from its semantics (preferred pole etc.).
  Bell (2010) proposes to align constructs in a way so they
  all have positive loadings on the first component of a
  grid PCA.
}
\examples{
\dontrun{

  # reproduction of the example in the Bell (2010)
  bell2010                    # show grid
  alignByLoadings(bell2010)   # constructs aligned by loadings on PC 1

  alignByLoadings(bell2010, output=2)  # show results

}
}
\author{
  Mark Heckmann
}
\references{
  Bell, R. C. (2010). A note on aligning constructs.
  \emph{Personal Construct Theory & Practice, 7}, 42-48.

  Mackay, N. (1992). Identification, Reflection, and
  Correlation: Problems in ihe bases of repertory grid
  measures. \emph{International Journal of Personal
  Construct Psychology, 5}(1), 57-75.
}
\seealso{
  \code{\link{alignByIdeal}}
}

