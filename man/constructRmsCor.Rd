\name{constructRmsCor}
\alias{constructRmsCor}
\title{Root mean square (RMS) of inter-construct correlations.}
\usage{
  constructRmsCor(x, method = "pearson", trim = NA)
}
\arguments{
  \item{x}{\code{repgrid} object}

  \item{method}{A character string indicating which
  correlation coefficient is to be computed. One of
  \code{"pearson"} (default), \code{"kendall"} or
  \code{"spearman"}, can be abbreviated.  The default is
  \code{"pearson"}.}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{NA}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}
}
\value{
  \code{dataframe} of the RMS of inter-construct
  correlations
}
\description{
  The RMS is also known as 'quadratic mean' of the
  inter-construct correlations. The RMS serves as a
  simplification of the correlation table. It reflects the
  average relation of one construct to all other
  constructs. Note that as the correlations are squared
  during its calculation, the RMS is not affected by the
  sign of the correlation (cf. Fransella, Bell & Bannister,
  2003, p. 86).
}
\examples{
# data from grid manual by Fransella, Bell and Bannister
   constructRmsCor(fbb2003)
   constructRmsCor(fbb2003, trim=20)

   # modify output
   r <- constructRmsCor(fbb2003)
   print(r, digits=5)
   # access calculation results
   r[2, 1]
}
\author{
  Mark Heckmann
}
\references{
  Fransella, F., Bell, R. C., & Bannister, D. (2003).  A
  Manual for Repertory Grid Technique (2. Ed.). Chichester:
  John Wiley & Sons.
}
\seealso{
  \code{\link{elementRmsCor}}, \code{\link{constructCor}}
}

