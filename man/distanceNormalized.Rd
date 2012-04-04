\name{distanceNormalized}
\alias{distanceNormalized}
\title{'normalized inter-element distances' (power transformed Hartmann distances).}
\usage{
  distanceNormalized(x, rep = 100,
    quant = c(0.05, 0.5, 0.95), significant = FALSE,
    trim = 10, indexcol = FALSE, prob = NULL, digits = 2,
    output = 1, progress = TRUE, upper = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{rep}{Number of random grids to generate to produce
  sample distribution for Hartmann distances (default is
  \code{100}). Note that a lot of samples may take a while
  to calculate. Set \code{progress = TRUE} to monitor
  progress for large samples.}

  \item{quant}{The propabities of the quantiles from the
  power transformed Hartmann distance distribution that
  will be returned.  The default is \code{c(.05, .5, .95)}.
  This corresponds to the lower 5 \%, the mean and the
  upper 5 \% of the distribution.}

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
  (\code{output=1}) will print the power transformed
  Hartmann distances to the console.  \code{output=0} will
  suppress the printing to the console.  In all cases a
  list containig the results of the calculations is
  returned invisibly. See value for details.}

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
  distances} \item{normalized}{matrix of power transformed
  Hartmann distances} \item{n.quantiles}{quantiles for
  power transformed Hartmann distances}
  \item{n.vals}{vector of all power transformed Hartmann
  distances} \item{n.sd}{standard deviation of random power
  transformed Hartmann distances}
}
\description{
  Calculate power-transformed Hartmann distances.
}
\details{
  Hartmann (1992) suggested a transformation of Slater
  (1977) distances to make them independent from the size
  of a grid. Hartmann distances are supposed to yield
  stable cutoff values used to determine 'significance' of
  inter-element distances. It can be shown that Hartmann
  distances are still affected by grid parameters like size
  and the range of the rating scale used. The function
  \code{distanceNormalize} applies a Box-Cox (1964)
  transformation to the Hartmann distances in order to
  remove the skew of the Hartmann distance distribution.
  The normalized values show to have more stable cutoffs
  (quantiles) and better properties for comparison across
  grids of different size and scale range.  \cr \cr The
  function \code{distanceNormalize} will return Slater,
  Hartmann or power transfpormed Hartmann distances if
  prompted. It is also possible to return the quantiles of
  the sample distribution and only the element distances
  consideres 'significant' according to the quantiles
  defined.

  The 'power tranformed or normalized Hartmann distance'
  are calulated as follows: The simulated Hartmann
  distribution is added a constant as the Box-Cox
  transformation can only be applied to positive values.
  Then a range of values for lambda in the Box-Cox
  transformation (Box & Cox, 1964) are tried out. The best
  lambda is the one maximizing the correlation of the
  quantiles with the standard normal distribution. The
  lambda value maximizing normality is used to transform
  Hartmann distances. As the resulting scale of the power
  transformation depends on lambda, the resulting values
  are z-transformed to derive a common scaling.

  The code for the calculation of the optimal lambda was
  written by Ioannis Kosmidis.
}
\examples{
\dontrun{

   distanceNormalized(bell2010)
   distanceNormalized(bell2010, trim=40, index=T, sig=T)

   ### histogram of power transformed Hartmann distances indifference region
   d <- distanceNormalized(bell2010, out=0)
   hist(d$n.vals, breaks=100)
   abline(v=d$n.quant, col="red")

   ### histogram of Hartmann distances and indifference region
   hist(d$h.vals, breaks=100)
   abline(v=d$h.quant, col="red")

}
}
\author{
  Mark Heckmann
}
\references{
  Box, G. E. P., & Cox, D. R. (1964). An Analysis of
  Transformations.  \emph{Journal of the Royal Statistical
  Society.  Series B (Methodological), 26}(2), 211-252.

  Hartmann, A. (1992). Element comparisons in repertory
  grid technique: Results and consequences of a Monte Carlo
  study. \emph{International Journal of Personal Construct
  Psychology, 5}(1), 41-56.

  Slater, P. (1977). \emph{The measurement of intrapersonal
  space by Grid technique}. London: Wiley.
}
\seealso{
  \code{\link{distanceHartmann}} and
  \code{\link{distanceSlater}}.
}

