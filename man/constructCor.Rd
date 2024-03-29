% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc.r
\name{constructCor}
\alias{constructCor}
\title{Calculate correlations between constructs.}
\usage{
constructCor(
  x,
  method = c("pearson", "kendall", "spearman"),
  trim = 20,
  index = FALSE
)
}
\arguments{
\item{x}{\code{repgrid} object.}

\item{method}{A character string indicating which correlation coefficient
is to be computed. One of \code{"pearson"} (default),
\code{"kendall"} or \code{"spearman"}, can be abbreviated.
The default is \code{"pearson"}.}

\item{trim}{The number of characters a construct is trimmed to (default is
\code{20}). If \code{NA} no trimming occurs. Trimming
simply saves space when displaying correlation of constructs
with long names.}

\item{index}{Whether to print the number of the construct.}
}
\value{
Returns a matrix of construct correlations.
}
\description{
Different types of correlations can be requested:
PMC, Kendall tau rank correlation, Spearman rank correlation.
}
\examples{

# three different types of correlations
constructCor(mackay1992)
constructCor(mackay1992, method = "kendall")
constructCor(mackay1992, method = "spearman")

# format output
constructCor(mackay1992, trim = 6)
constructCor(mackay1992, index = TRUE, trim = 6)

# save correlation matrix for further processing
r <- constructCor(mackay1992)
r
print(r, digits = 5)

# accessing the correlation matrix
r[1, 3]

}
\seealso{
\code{\link[=elementCor]{elementCor()}}
}
