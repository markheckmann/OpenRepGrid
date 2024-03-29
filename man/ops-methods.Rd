% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repgrid-basicops.r
\docType{methods}
\name{+,repgrid,repgrid-method}
\alias{+,repgrid,repgrid-method}
\alias{+,list,repgrid-method}
\alias{+,repgrid,list-method}
\title{Concatenate repgrid objects.}
\usage{
\S4method{+}{repgrid,repgrid}(e1, e2)

\S4method{+}{list,repgrid}(e1, e2)

\S4method{+}{repgrid,list}(e1, e2)
}
\arguments{
\item{e1, e2}{A \code{repgrid} object.}
}
\description{
Simple concatenation of repgrid objects or list containing
repgrid objects using the '+' operator.
}
\details{
Methods for \code{"+"} function.
}
\examples{

x <- bell2010
x + x
x + list(x, x)
list(x, x) + x

}
