\name{elementRmsCor}
\alias{elementRmsCor}
\title{Root mean square (RMS) of inter-element correlations.}
\usage{
  elementRmsCor(x, rc = TRUE, method = "pearson",
    trim = NA)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{rc}{Whether to use Cohen's rc which is invariant to
  construct reflection (see desciption above). It is used
  as the default.}

  \item{method}{A character string indicating which
  correlation coefficient to be computed. One of
  \code{"pearson"} (default), \code{"kendall"} or
  \code{"spearman"}, can be abbreviated.  The default is
  \code{"pearson"}.}

  \item{trim}{The number of characters an element is
  trimmed to (default is \code{NA}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}
}
\value{
  \code{dataframe} of the RMS of inter-element correlations
}
\description{
  The RMS is also known as 'quadratic mean' of the
  inter-element correlations. The RMS serves as a
  simplification of the correlation table. It reflects the
  average relation of one element with all other elements.
  Note that as the correlations are squared during its
  calculation, the RMS is not affected by the sign of the
  correlation (cf. Fransella, Bell & Bannister, 2003, p.
  86).
}
\details{
  Note that simple element correlations as a measure of
  similarity are flawed as they are not invariant to
  construct reflection (Mackay, 1992; Bell, 2010). A
  correlation index invariant to construct reflection is
  Cohen's rc measure (1969), which can be calculated using
  the argument \code{rc=TRUE} which is the default option
  in this function.
}
\examples{
# data from grid manual by Fransella, Bell and Bannister
   elementRmsCor(fbb2003)
   elementRmsCor(fbb2003, trim=10)

   # modify output
   r <- elementRmsCor(fbb2003)
   print(r, digits=5)

   # access second row of calculation results
   r[2, "RMS"]
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
  \code{\link{constructRmsCor}}, \code{\link{elementCor}}
}

