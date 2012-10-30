\docType{methods}
\name{+}
\alias{+}
\alias{+,list,repgrid-method}
\alias{+,repgrid,list-method}
\alias{+,repgrid,repgrid-method}
\title{Concatenate repgrid objects.}
\description{
  Simple concatenation of repgrid objects or list
  containing repgrid objects using the '+' operator.
}
\details{
  Methods for \code{"+"} function.
}
\examples{
x <- bell2010
x + x
x + list(x,x)
list(x,x) + x
}
\author{
  Mark heckmann
}

