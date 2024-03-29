% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repgrid-basicops.r
\name{addAvgElement}
\alias{addAvgElement}
\title{Add a new average element}
\usage{
addAvgElement(x, name = "avg", i, digits = Inf)
}
\arguments{
\item{x}{A \code{repgrid} object.}

\item{name}{Name of new element.}

\item{i}{Indexes of elements to be averaged across. Negative indexes can be
used to exclude elements from the complete set. Duplicate indexes are
allowed but a warning is issued.}

\item{digits}{Digits to round mean value to. By default no rounding is used
(\code{digits = Inf}). Use \code{digits = 0} to round to nearest integer, i.e. only
using original integer rating scores.}
}
\value{
A \code{repgrid} object with additional new element.
}
\description{
A new element is added to the grid. The scores are the arithmetic means
across all selected elements.
}
\examples{
addAvgElement(feixas2004, "others", i = 2:12)
addAvgElement(feixas2004, "others", i = 2:12, digits = 0) # integers

# exluding elements via negative indexes
addAvgElement(feixas2004, "others", i = c(-1, -13))

}
\keyword{internal}
