\name{constructCor}
\alias{constructCor}
\title{Calculate the correlations between constructs.}
\usage{
  constructCor(x,
    method = c("pearson", "kendall", "spearman"),
    trim = 20, index = FALSE, col.index = TRUE, digits = 2,
    output = 1, upper = T)
}
\arguments{
  \item{x}{\code{repgrid} object.}

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
  \code{output=0} will supress printing of the output.
  \code{output=1} will print results to the screen.
  \code{output=2} will surpress printing but return a
  matrix ready for printing.}

  \item{upper}{Whether to display upper triangle of
  correlation matrix only (default is \code{TRUE}).}
}
\value{
  Prints results to the console and invisibly returns a
  matrix of construct correlations.
}
\description{
  Different types of correlations can be requested: PMC,
  Kendall tau rank correlation, Spearman rank correlation.
}
\examples{
\dontrun{

   # three different types of correlations
   constructCor(mackay1992)
   constructCor(mackay1992, meth="kend")
   constructCor(mackay1992, meth="spea")

   # format output
   constructCor(mackay1992, upper=F)
   constructCor(mackay1992, col.index=F, trim=6)
   constructCor(mackay1992, index=T, col.index=F, trim=6)

   # save as object for further processing.
   # no visible output.
   r <- constructCor(mackay1992, trim=6, out=0)
   r

}
}
\author{
  Mark Heckmann
}
\seealso{
  \code{\link{elementCor}}
}

