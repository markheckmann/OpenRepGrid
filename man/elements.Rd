% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repgrid-elements.r
\name{elements}
\alias{elements}
\alias{elements<-}
\title{Get or replace element names}
\usage{
elements(x)

elements(x, position) <- value
}
\arguments{
\item{x}{A repgrid object.}

\item{position}{Index where to insert element.}

\item{value}{Character vector of element names.}
}
\description{
Allows to get and set element names.
Replaces the older functions \code{getElementNames}, \code{getElementNames2},
and \code{eNames} which are deprecated.
}
\examples{

# copy Boeker grid to x
x <- boeker

## get element names
e <- elements(x)
e

## replace element names
elements(x) <- rev(e) # reverse all element names
elements(x)[1] <- "Hannes" # replace name of first element

# replace names of elements 1 and 3
elements(x)[c(1, 3)] <- c("element 1", "element 3")

}
