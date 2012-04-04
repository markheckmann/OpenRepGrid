\name{constructPca}
\alias{constructPca}
\title{Principal component analysis (PCA) of inter-construct correlations.}
\usage{
  constructPca(x, nfactors = 3, rotate = "varimax",
    method = c("pearson", "kendall", "spearman"),
    trim = NA, digits = 2, cutoff = 0, output = 1)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{nfactors}{Number of components to extract (default
  is \code{3}).}

  \item{rotate}{\code{"none"}, \code{"varimax"},
  \code{"promax"} and \code{"cluster"} are possible
  rotations (default is \code{none}).}

  \item{method}{A character string indicating which
  correlation coefficient is to be computed. One of
  \code{"pearson"} (default), \code{"kendall"} or
  \code{"spearman"}, can be abbreviated.  The default is
  \code{"pearson"}.}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{7}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{cutoff}{Loadings smaller than cutoff are not
  printed.}

  \item{output}{The type of output printed to the console.
  \code{output=0} will supress printing of the output.
  \code{output=1} (default) will print results to the
  screen. \code{output==2} will invisibly retrn the whole
  object from \code{principal} not only the loadings.}
}
\value{
  Invisibly returns a matrix of loadings on the principal
  components.
}
\description{
  Various methods for rotation and methods for the
  calculation of the correlations are available. Note that
  the number of factors has to be specified. For more
  information on the PCA function itself type
  \code{?principal}.
}
\examples{
\dontrun{

   # data from grid manual by Fransella et al. (2003, p. 87)
   # note that the construct order is different
   constructPca(fbb2003, nf=2)

   # surpress printing to console
   m <- constructPca(fbb2003, nf=2)
   m

   # no rotation
   constructPca(fbb2003, rotate="none")

   # using a different correlation matrix (Spearman)
   constructPca(fbb2003, method="spearman")

}
}
\author{
  Mark Heckmann
}
\references{
  Fransella, F., Bell, R. & Bannister, D. (2003). \emph{A
  Manual for Repertory Grid Technique} (2. Ed.).
  Chichester: John Wiley & Sons.
}

