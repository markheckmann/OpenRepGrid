\name{distance}
\alias{distance}
\title{Distance measures (between constructs or elements).}
\usage{
  distance(x, along = 1, dmethod = "euclidean", p = 2,
    trim = 20, index = TRUE, col.index = FALSE, digits = 2,
    output = 1, upper = T, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{along}{Whether to calculate distance for 1 =
  constructs (default) or for 2= elements.}

  \item{dmethod}{The distance measure to be used. This must
  be one of "euclidean", "maximum", "manhattan",
  "canberra", "binary" or "minkowski". Any unambiguous
  substring can be given.  For additional information on
  the different types type \code{?dist}.}

  \item{p}{The power of the Minkowski distance, in case
  \code{"minkowski"} is used as argument for
  \code{dmethod}.}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{20}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs with long names.}

  \item{index}{Whether to print the number of the construct
  (default is \code{TRUE}).}

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

  \item{...}{Additional parameters to be passed to function
  \code{dist}.  Type \code{dist} for further information.}
}
\value{
  \code{matrix} object.
}
\description{
  Various distance measures between elements or constructs
  are calculated.
}
\examples{
\dontrun{

   # between constructs
   distance(bell2010, along=1)
   # between elements
   distance(bell2010, along=2)

   # several distance methods
   distance(bell2010, dm="man")         # manhattan distance
   distance(bell2010, dm="mink", p=3)   # minkowski metric to the power of 3

   # to save the results without printing to the console
   d <- distance(bell2010, out=0, trim=7)
   d

}
}
\author{
  Mark Heckmann
}

