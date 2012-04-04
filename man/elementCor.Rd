\name{elementCor}
\alias{elementCor}
\title{Calculate the correlations between elements.}
\usage{
  elementCor(x, rc = TRUE,
    method = c("pearson", "kendall", "spearman"),
    trim = 20, index = FALSE, col.index = TRUE, digits = 2,
    output = 1, upper = T)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{rc}{Use Cohen's rc which is invariant to construct
  reflection (see notes). It is used as default.}

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

  \item{col.index}{Logical. Whether to add an extra index
  column so the column names are indexes instead of
  construct names. This option renders a neater output as
  long construct names will stretch the output (default is
  \code{FALSE}).}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{output}{The type of output printed to the console.
  \code{output=0} will supress printing of the output.}

  \item{upper}{Whether to display upper triangle of
  correlation matrix only (default is \code{TRUE}).}
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
\dontrun{

   elementCor(mackay1992)                      # Cohen's rc
   elementCor(mackay1992, rc=F)                # PM correlation
   elementCor(mackay1992, rc=F, meth="spear")  # Spearman correlation

   # format output
   elementCor(mackay1992, upper=F)
   elementCor(mackay1992, col.index=F, trim=6)
   elementCor(mackay1992, index=T, col.index=F, trim=6)

   # save as object for further processing.
   # no visible output.
   r <- elementCor(mackay1992, trim=6, out=0)
   r

}
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

