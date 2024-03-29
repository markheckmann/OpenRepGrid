% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{recycle2}
\alias{recycle2}
\title{variation of recycle that recycles one vector x or y to the length of the
longer one}
\usage{
recycle2(x, y, na.fill = FALSE)
}
\arguments{
\item{x}{vector to be recycled if shorter than y}

\item{y}{vector to be recycled if shorter than x}

\item{na.fill}{Use NAs for filling up to given length (default=FALSE)}
}
\value{
list     a list containing the recycled x vector as first and
the recycled y vector as second element
}
\description{
variation of recycle that recycles one vector x or y to the length of the
longer one
}
\examples{
recycle2(1:10, 1:3)
recycle2(1, 1:5)
recycle2(1, 1:5, na.fill = TRUE)
recycle2(1:5, 5:1) # vectors unchanged
}
\keyword{internal}
