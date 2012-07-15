\name{print.distance}
\alias{print.distance}
\title{Print method for class distance.}
\usage{
  \method{print}{distance} (x, digits = 2,
    col.index = TRUE, upper = TRUE, ...)
}
\arguments{
  \item{x}{Object of class distance.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{col.index}{Logical. Whether to add an extra index
  column so the column names are indexes instead of
  construct names. This option renders a neater output as
  long construct names will stretch the output (default is
  \code{TRUE}).}

  \item{upper}{Whether to display upper triangle of
  correlation matrix only (default is \code{TRUE}).}

  \item{...}{Not evaluated.}
}
\description{
  Print method for class distance.
}
\keyword{internal}

