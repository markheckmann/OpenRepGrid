\name{strReverse}
\alias{strReverse}
\title{reverse a string}
\usage{
  strReverse(x)
}
\arguments{
  \item{x}{a string or a vector of strings}
}
\value{
  vector a string or vector of strings with reversed chars
}
\description{
  reverses the strings of a vector, i.e. c("ABC", "abc")
  becomes c("CBA", "cba")
}
\examples{
strReverse(c("ABC", "abc"))
}
\references{
  \url{http://www.mail-archive.com/r-help@r-project.org/msg102759.html}
}
\keyword{internal}
