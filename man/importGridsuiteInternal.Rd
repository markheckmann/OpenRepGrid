\name{importGridsuiteInternal}
\alias{importGridsuiteInternal}
\title{Internal parser for Gridsuite data files}
\usage{
  importGridsuiteInternal(file, dir = NULL)
}
\arguments{
  \item{file}{filename including path if file is not in
  current working directory. File can also be a complete
  URL. The fileformat is .dat.}

  \item{dir}{alternative way to supply the directory where
  the file is located (default \code{NULL}).}
}
\description{
  Internal parser for Gridsuite data files
}
\note{
  The developers of Gridsuite have proposed to use an XML
  scheme as a standard exchange format for repertory grid
  data (Walter, Bacher & Fromm, 2004). This approach is
  also embraced by the \code{OpenRepGrid} package.

  TODO: The element and construct IDs are not used yet.
  Thus, if the output should be in different order the
  current mechanism will cause false assignments.
}
\examples{
\dontrun{

# supposing that the data file gridsuite.xml is in the current directory
file <- "gridsuite.xml"
imp <- importGridsuite(file)

# specifying a directory (arbitrary example directory)
dir <- "/Users/markheckmann/data"
imp <- importGridsuite(file, dir)

# using a full path
imp <- importGridsuite("/Users/markheckmann/data/gridsuite.xml")

# load Gridsuite data from URL
imp <- importGridsuite("http://www.openrepgrid.uni-bremen.de/data/gridsuite.xml")
}
}
\author{
  Mark Heckmann
}
\references{
  \url{http://www.gridsuite.de/}

  Walter, O. B., Bacher, A., & Fromm, M. (2004). A proposal
  for a common data exchange format for repertory grid
  data.  \emph{Journal of Constructivist Psychology,
  17}(3), 247.  doi:10.1080/10720530490447167
}
\keyword{internal}

