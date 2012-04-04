\name{distanceHartmann}
\alias{distanceHartmann}
\title{'Hartmann distance' (standardized Euclidean distances).}
\usage{
  distanceHartmann(x, rep = 100, meantype = 2,
    quant = c(0.05, 0.5, 0.95), significant = FALSE,
    trim = 10, indexcol = FALSE, prob = NULL, digits = 2,
    output = 1, progress = TRUE, upper = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{rep}{Number of random grids to generate to produce
  sample distribution for Slater distances (default is
  \code{100}). Note that a lot of samples may take a while
  to calculate. Set \code{progress = TRUE} to monitor
  progress for large samples.}

  \item{meantype}{The type of mean to use for
  standardization.  \code{meantype=1} will use the
  empirical mean (which is 1 and the default)
  \code{meantype=2} will use the expected mean, i. e. 1.}

  \item{quant}{The propabities of the quantiles from the
  Slater distance distribution that will be returned.  The
  default is \code{c(.05, .5, .95)}. This corresponds to
  the lower 5 \%, the mean and the upper 5 \% of the
  distribution.}

  \item{significant}{Whether to only show values that are
  outside the quantiles defined in \code{quant}, i.e.
  onsidered as 'significant' (default is \code{FALSE}.)
  The first and last value of \code{quant} is used to
  determine the indifference region. This options only
  applies when \code{output == 1} is used.}

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

  \item{prob}{The probability of each rating value to
  occur.  If \code{NULL} (default) the distribution is
  uniform.  The number of values must match the length of
  the rating scale.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{output}{The output type. The default
  (\code{output=1}) will print the Hartmann distances,
  \code{output=2} the Slater and \code{output=3} both
  distances to the console.  \code{output=0} will suppress
  the printing to the console.  In all cases a list
  containig the results of the calculations is returned
  invisibly. See value for details.}

  \item{progress}{Whether to show a progress bar (default
  is \code{TRUE}).  May be useful when the distribution is
  estimated on the basis of many quasis.}

  \item{upper}{Logical. Whether to display only upper part
  of the distance matrix (default \code{TRUE}).}
}
\value{
  A matrix containing Hartmann distances (\code{output=1}
  and \code{output=2}) or a list (\code{output=3})
  containing: \cr \item{hartmann}{matrix of Hartmann
  distances} \item{h.quantiles}{quantiles for Hartmann
  distances} \item{h.vals}{random values of Hartmann}
  \item{h.sd}{standard deviation of distribution of
  Hartmann values} \item{slater}{matrix of Slater
  distances} \item{sl.quantiles}{quantiles for Slater
  distances} \item{sl.vals}{vector of all Slater distances}
  \item{ls.sd}{standard deviation of random Slater
  distances}
}
\description{
  Calculate Hartmann distance
}
\details{
  Hartmann (1992) showed in a Monte Carlo study that Slater
  distances (see \code{\link{distanceSlater}}) based on
  random grids, for which Slater coined the expression
  quasis, have a skewed distribution, a mean and a standard
  deviation depending on the number of constructs elicited.
  He suggested a linear transformation (z-transformation)
  which takes into account the estimated (or expected) mean
  and the standard deviation of the derived distribution to
  standardize Slater distance scores across different grid
  sizes. 'Hartmann distances' represent a more accurate
  version of 'Slater distances'. Note that Hartmann
  distances are multiplied by -1. Hence, negative Hartmann
  values represent dissimilarity, i.e. a big Slater
  distance.  \cr \cr The function \code{distanceHartmann}
  conducts a small Monte Carlo simulation for the supplied
  grid. I. e. a number of quasis of the same size and with
  the same scale range as the grid under investigation are
  generated. A distrubution of Slater distances derived
  from the quasis is calculated and used for Hartmann's
  standardization.  \cr \cr It is also possible to return
  the quantiles of the sample distribution and only the
  element distances consideres 'significant' according to
  the quantiles defined.

  The 'Hartmann distance' is calculated as follows
  (Hartmann 1992, p. 49).  \cr \deqn{D = -1
  (\frac{D_{slater} - M_c}{sd_c})}{D = -1 (D_slater - M_c /
  sd_c)} Where \eqn{D_{slater}}{D_slater} denotes the
  Slater distances of the grid, \eqn{M_c}{M_c} the sample
  distribution's mean value and \eqn{sd_c}{sd_c} the sample
  distributions's standard deviation.
}
\examples{
\dontrun{

   distanceHartmann(bell2010)
   distanceHartmann(bell2010, trim=40, index=T, sig=T)

   ### histogram of Slater distances and indifference region
   d <- distanceHartmann(bell2010, out=0)
   hist(d$sl.vals, breaks=100)
   abline(v=d$sl.quant, col="red")

   ### histogram of Hartmann distances and indifference region
   hist(d$h.vals, breaks=100)
   abline(v=d$h.quant, col="red")

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
}
\seealso{
  \code{\link{distanceSlater}}
}

