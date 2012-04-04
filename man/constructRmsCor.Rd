\name{constructRmsCor}
\alias{constructRmsCor}
\title{Root mean square (RMS) of inter-construct correlations.}
\usage{
  constructRmsCor(x,
    method = c("pearson", "kendall", "spearman"),
    trim = NA, digits = 2, output = 1)
}
\arguments{
  \item{x}{\code{repgrid} object}

  \item{method}{a character string indicating which
  correlation coefficient is to be computed. One of
  \code{"pearson"} (default), \code{"kendall"} or
  \code{"spearman"}, can be abbreviated.  The default is
  \code{"pearson"}.}

  \item{trim}{the number of characters a construct is
  trimmed to (default is \code{7}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}

  \item{digits}{\code{numeric}. Number of digits to round
  to (default is \code{2}).}

  \item{output}{The type of output printed to the console.
  \code{output=0} will supress printing of the output.}
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
\dontrun{

   # data from grid manual by Fransella, Bell and Bannister
   constructRmsCor(fbb2003)
   constructRmsCor(fbb2003, trim=20, digits=3)

   # calculate invisibly
   r <- constructRmsCor(fbb2003, out=0)
   r

}
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
  \code{\link{constructCor}}
}

