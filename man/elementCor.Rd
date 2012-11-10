\name{elementCor}
\alias{elementCor}
\title{Calculate the correlations between elements.}
\usage{
  elementCor(x, rc = TRUE, method = "pearson", trim = 20,
    index = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{rc}{Use Cohen's rc which is invariant to construct
  reflection (see desciption above). It is used as the
  default.}

  \item{method}{A character string indicating which
  correlation coefficient is to be computed. One of
  \code{"pearson"} (default), \code{"kendall"} or
  \code{"spearman"}, can be abbreviated.  The default is
  \code{"pearson"}.}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{20}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}

  \item{index}{Whether to print the number of the
  construct.}
}
\value{
  \code{matrix} of element correlations
}
\description{
  Note that simple element correlations as a measure of
  similarity are flawed as they are not invariant to
  construct reflection (Mackay, 1992; Bell, 2010). A
  correlation index invariant to construct reflection is
  Cohen's rc measure (1969), which can be calculated using
  the argument \code{rc=TRUE} which is the default option.
}
\examples{
elementCor(mackay1992)                      # Cohen's rc
   elementCor(mackay1992, rc=FALSE)            # PM correlation
   elementCor(mackay1992, rc=FALSE, method="spearman")  # Spearman correlation

   # format output
   elementCor(mackay1992, trim=6)
   elementCor(mackay1992, index=FALSE, trim=6)

   # save as object for further processing
   r <- elementCor(mackay1992)
   r

   # change output of object
   print(r, digits=5)
   print(r, col.index=FALSE)
   print(r, upper=FALSE)

   # accessing elements of the correlation matrix
   r[1,3]
}
\author{
  Mark Heckmann
}
\references{
  Bell, R. C. (2010). A note on aligning constructs.
  \emph{Personal Construct Theory & Practice}, (7), 42-48.

  Cohen, J. (1969). rc: A profile similarity coefficient
  invariant over variable reflection. \emph{Psychological
  Bulletin, 71}(4), 281-284.

  Mackay, N. (1992). Identification, Reflection, and
  Correlation: Problems In The Bases Of Repertory Grid
  Measures. \emph{International Journal of Personal
  Construct Psychology, 5}(1), 57-75.
}
\seealso{
  \code{\link{constructCor}}
}

