% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance.R
\name{distanceNormalized}
\alias{distanceNormalized}
\title{Calculate power-transformed Hartmann distances.}
\usage{
distanceNormalized(
  x,
  reps = 1000,
  prob = NULL,
  progress = TRUE,
  distributions = TRUE
)
}
\arguments{
\item{x}{\code{repgrid} object.}

\item{reps}{Number of random grids to generate to produce
sample distribution for Hartmann distances
(default is \code{1000}). Note that
a lot of samples may take a while to calculate.}

\item{prob}{The probability of each rating value to occur.
If \code{NULL} (default) the distribution is uniform.
The number of values must match the length of the rating scale.}

\item{progress}{Whether to show a progress bar during simulation
(default is \code{TRUE}) (for \code{method="simulate"}).
May be useful when the distribution is estimated on the basis
of many quasis.}

\item{distributions}{Whether to additionally return the values of the simulated
distributions (Slater etc.) The default is \code{FALSE}
as it will quickly boost the object size.}
}
\value{
A matrix containing the standardized distances. \cr
Further data is contained in the object's attributes: \cr
\item{\code{"arguments"}}{A list of several parameters
including \code{mean} and \code{sd} of Slater distribution.}
\item{\code{"quantiles"}}{Quantiles for Slater, Hartmann
and power transformed distance distributions.}
\item{\code{"distributions"}}{List with values of the
simulated distributions, if \code{distributions=TRUE}.}
}
\description{
Hartmann (1992) suggested a transformation of Slater (1977) distances to make
them independent from the size of a grid. Hartmann distances are supposed to
yield stable cutoff values used to determine 'significance' of inter-element
distances. It can be shown that Hartmann distances are still affected by grid
parameters like size and the range of the rating scale used (Heckmann, 2012).
The function \code{distanceNormalize} applies a Box-Cox (1964) transformation to the
Hartmann distances in order to remove the skew of the Hartmann distance
distribution. The normalized values show to have more stable cutoffs
(quantiles) and better properties for comparison across grids of different
size and scale range.  \cr
}
\details{
The function \code{distanceNormalize} can also return
the quantiles of the sample distribution and only the element distances
considered 'significant' according to the quantiles defined.
}
\section{Calculations}{

The 'power transformed Hartmann distance' are calculated as
follows: The simulated Hartmann distribution is added a constant as the
Box-Cox transformation can only be applied to positive values. Then a range
of values for lambda in the Box-Cox transformation (Box & Cox, 1964) are
tried out. The best lambda is the one maximizing the correlation of the
quantiles with the standard normal distribution. The lambda value maximizing
normality is used to transform Hartmann distances. As the resulting scale of
the power transformation depends on lambda, the resulting values are
z-transformed to derive a common scaling.

The code for the calculation of the optimal lambda was written by Ioannis
Kosmidis.
}

\examples{
\dontrun{

### basics  ###

distanceNormalized(bell2010)
n <- distanceNormalized(bell2010)
n

# printing options
print(n)
print(n, digits = 4)
# 'significant' distances only
print(n, p = c(.05, .95))

# access cells of distance matrix
n[1, 2]

### advanced  ###

# histogram of Slater distances and indifference region
n <- distanceNormalized(bell2010, distributions = TRUE)
l <- attr(n, "distributions")
hist(l$bc, breaks = 100)
}

}
\references{
Box, G. E. P., & Cox, D. R. (1964). An Analysis of Transformations.
\emph{Journal of the Royal Statistical Society. Series B (Methodological), 26}(2), 211-252.

Hartmann, A. (1992). Element comparisons in repertory grid technique: Results and consequences of a Monte Carlo
study. \emph{International Journal of Personal Construct Psychology, 5}(1), 41-56.

Heckmann, M. (2012). \emph{Standardizing inter-element distances in grids - A revision of Hartmann's distances}, 11th
Biennal Conference of the European Personal Construct Association (EPCA), Dublin, Ireland, Paper presentation,
July 2012.

Slater, P. (1977). \emph{The measurement of intrapersonal space by Grid technique}. London: Wiley.
}
\seealso{
\code{\link[=distanceHartmann]{distanceHartmann()}} and \code{\link[=distanceSlater]{distanceSlater()}}.
}
