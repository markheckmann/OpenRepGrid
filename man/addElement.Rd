% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repgrid-basicops.r
\name{addElement}
\alias{addElement}
\title{Add an element to an existing grid.}
\usage{
addElement(
  x,
  name = NA,
  scores = NA,
  abbreviation = NA,
  status = NA,
  position = NA,
  side = "pre"
)
}
\arguments{
\item{x}{\code{repgrid} object.}

\item{name}{Name of the new element (character string).}

\item{scores}{Numerical ratings for the new element column
(length must match number of constructs in the grid).}

\item{abbreviation}{Abbreviation for element name.}

\item{status}{Element status (not yet in use).}

\item{position}{An integer at which column the element will be added.
TODO: Does not work properly yet.}

\item{side}{Not yet in use.}
}
\value{
\code{repgrid} object
}
\description{
Add an element to an existing grid.
}
\examples{
\dontrun{

bell2010
addElement(bell2010, "new element", c(1, 2, 5, 4, 3, 6, 5, 2, 7))
}

}
\seealso{
\code{\link[=addConstruct]{addConstruct()}}
}
\keyword{internal}
