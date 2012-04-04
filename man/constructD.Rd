\name{constructD}
\alias{constructD}
\title{Calculate Somers' d for the constructs.}
\usage{
  constructD(x, dependent = "c", trim = 30, index = T,
    col.index = F, digits = 1, output = 1)
}
\arguments{
  \item{x}{\code{repgrid} object}

  \item{dependent}{A string denoting the direction of
  dependency in the output table (as d is assymetrical).
  Possible values are \code{"c"} (the default) for setting
  the columns as dependent, \code{"r"} for setting the rows
  as the dependent variable and \code{"s"} for the
  symmetrical Somers' d measure (the mean of the two
  directional values for code{"c"} and \code{"r"}).}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{30}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}

  \item{index}{Whether to print the number of the construct
  (default is \code{TRUE}).}

  \item{col.index}{Logical. Wether to add an extra index
  column so the column names are indexes instead of
  construct names. This option renders a neater output as
  long construct names will stretch the output (default is
  \code{FALSE}).}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{output}{The type of output printed to the console.
  \code{output=0} will supress printing of the output.
  \code{output=1} (default) will print results to the
  screen.}
}
\value{
  \code{matrix} of construct correlations.
}
\description{
  Somer'ss d is an assymetric association measure as it
  depends on which variable is set as dependent and
  independent. The direction of dependency needs to be
  specified.
}
\note{
  Thanks to Marc Schwartz for supplying the code to
  calculate Somers' d.
}
\examples{
\dontrun{

   constructD(fbb2003)       # columns as dependent (default)
   constructD(fbb2003, "c")  # row as dependent
   constructD(fbb2003, "s")  # symmetrical index

   # surpress printing
   d <- constructD(fbb2003, out=0, trim=5)
   d

   # more digits
   constructD(fbb2003, dig=3)

   # add index column, no trimming
   constructD(fbb2003, col.index=TRUE, index=F, trim=NA)

}
}
\author{
  Mark Heckmann
}
\references{
  Somers, R. H. (1962). A New Asymmetric Measure of
  Association for Ordinal Variables. \emph{American
  Sociological Review, 27}(6), 799-811.
}

