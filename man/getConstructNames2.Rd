\name{getConstructNames2}
\alias{getConstructNames2}
\title{Retrieve construct names in needed format.}
\usage{
  getConstructNames2(x, mode = 1, trim = 20, index = F,
    sep = " - ", pre = "(", post = ") ")
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{mode}{Type of output. 1= left and right pole
  seperated by \code{sep}), 2= only left pole, 3 = only
  right pole.}

  \item{trim}{Number of characters to trim the construct
  names to (default \code{NA}). \code{NA} will surpress
  trimming.  The length of \code{index} is not included in
  the trimming.}

  \item{index}{Logical. Whether to add a index number
  before the construct names (default \code{TRUE}).}

  \item{sep}{Seperator string between poles.}

  \item{pre}{String before index number (default
  \code{(}).}

  \item{post}{String after index number (default \code{)
  }).}
}
\value{
  Vector with construct names.
}
\description{
  Retrieves the construct names from a \code{repgrid}.
}
\details{
  Different features like trimming, indexing and choices of
  seperators allow to return the kind of format that is
  needed.
}
\examples{
\dontrun{

  getConstructNames2(bell2010)
  getConstructNames2(bell2010, mode=2)
  getConstructNames2(bell2010, index=T)
  getConstructNames2(bell2010, index=T, mode=3)

}
}
\author{
  Mark Heckmann
}
\keyword{internal}

