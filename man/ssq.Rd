\name{ssq}
\alias{ssq}
\title{Calculate SSQ (accuracy) of biplot representation for elements
and constructs.}
\usage{
  ssq(x, along = 2, cum = T, center = 1, normalize = 0,
    g = 0, h = 1 - g, col.active = NA, col.passive = NA,
    digits = 1, print = TRUE, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{along}{Numeric. Table of sum-of-squares (SSQ) for
  1=constructs, 2=elements.  Note that currently these
  calculations only make sense for biplot reperesentations
  with \code{g=1} and \code{h=1} respectively.}

  \item{cum}{Logical. Return a cumulated table of
  sum-of-squares?  (default is \code{TRUE}).}

  \item{center}{Numeric. The type of centering to be
  performed.  0= no centering, 1= row mean centering
  (construct), 2= column mean centering (elements), 3=
  double-centering (construct and element means), 4=
  midpoint centering of rows (constructs).  The default is
  \code{1} (row centering).}

  \item{normalize}{A numeric value indicating along what
  direction (rows, columns) to normalize by standard
  deviations. \code{0 = none, 1= rows, 2 = columns}
  (default is \code{0}).}

  \item{g}{Power of the singular value matrix assigned to
  the left singular vectors, i.e. the constructs.}

  \item{h}{Power of the singular value matrix assigned to
  the right singular vectors, i.e. the elements.}

  \item{col.active}{Columns (elements) that are no
  supplementary points, i.e. they are used in the SVD to
  find principal components. default is to use all
  elements.}

  \item{col.passive}{Columns (elements) that are
  supplementary points, i.e. they are NOT used in the SVD
  but projecte into the component space afterwards. They do
  not determine the solution. Default is \code{NA}, i.e. no
  elements are set supplementary.}

  \item{digits}{Number of digits to round the output to.}

  \item{print}{Whether to print the oputut to the console
  (default \code{TRUE}).}
}
\value{
  \code{dataframe} containing the explained (cumulated)
  sum-of-squares for each construct or element on each
  principal component.
}
\description{
  Each construct and element are vectors in a
  multidimensional space. When reducing the representation
  to a lower dimensional space, a loss of information
  (sum-of-squares) will usually occur. The output of the
  function shows the proportion of sum-of-squares (SSQ)
  explained for the elements (constructs) and the amount
  explained by each principal component. This allows to
  assess which elements (construct) are represented how
  well in the current representation. Also it shows how
  much of the total variation is explained.
}
\note{
  TODO: if g or h is not equal to 1 the SSQ does not
  measure accuracy of representation as currently the ssq
  of each point are set in constrast with the
  pre-transformed matrix.
}
\author{
  Mark Heckmann
}
\keyword{internal}

