\name{importGridstat}
\alias{importGridstat}
\title{Import Gridstat data files.}
\usage{
  importGridstat(file, dir = NULL, min = NULL, max = NULL)
}
\arguments{
  \item{file}{Filename including path if file is not in
  current working directory. File can also be a complete
  URL. The fileformat is \code{.dat}. If no file is
  supplied a selection pop up menu is opened to select the
  files.}

  \item{dir}{Alternative way to supply the directory where
  the file is located (default \code{NULL}).}

  \item{min}{Optional argument (\code{numeric}, default
  \code{NULL}) for minimum rating value in grid.}

  \item{max}{Optional argument (\code{numeric}, default
  \code{NULL}) for maximum rating value in grid.}
}
\value{
  A single \code{repgrid} object in case one file and a
  list of \code{repgrid} objects in case multiple files are
  imported.
}
\description{
  Reads the file format that is used by the latest version
  of the grid program gridstat (Bell, 1998).
}
\note{
  Note that the gridstat data format does not contain
  explicit information about the range of the rating scale
  used (minimum and maximum). By default the range is
  inferred by scanning the ratings and picking the minimal
  and maximal values as rating range. You can set the
  minimal and maximal value by hand using the \code{min}
  and \code{max} arguments or by using the
  \code{setScale()} function.  Note that if the rating
  range is not set, it may cause several functions to not
  work properly. A warning will be issued if the range is
  not set explicitly when using the importing function.

  The function only reads data from the latest GridStat
  version.  The latest version allows the seperation of the
  left and right pole by using on of the following symbols
  \code{/:-} (hyphene, colon and dash). Older versions may
  not seperate the left and right pole. This will cause all
  labels to be assigned to the left pole only when
  importing. You may fix this by simply entering one of the
  construct seperator symbols into the GridStat file
  between each left and right construct pole.

  The third line of a GridStat file may contain a no labels
  statement (i.e. a line containing any string of 'NOLA',
  'NO L', 'NoLa', 'No L', 'Nola', 'No l', 'nola' or 'no
  l'). In this case only ratings are supplied, hence,
  default names are assigned to elements and constructs.
}
\examples{
\dontrun{

# using the pop-up selection menu
rg <- importGridstat()

# supposing that the data file gridstat.dat is in the current working directory
file <- "gridstat.dat"
rg <- importGridstat(file)

# specifying a directory (example)
dir <- "/Users/markheckmann/data"
rg <- importGridstat(file, dir)

# using a full path (example)
rg <- importGridstat("/Users/markheckmann/data/gridstat.dat")

# load gridstat data from URL
rg <- importGridstat("http://www.openrepgrid.uni-bremen.de/data/gridstat.dat")

# setting rating scale range
rg <- importGridstat(file, dir, min=1, max=6)
}
}
\author{
  Mark Heckmann
}
\references{
  Bell, R. C. (1998) GRIDSTAT: A program for analysing the
  data of a repertory grid. Melbourne: Author.
}
\seealso{
  \code{\link{importGridcor}},
  \code{\link{importGridstat}},
  \code{\link{importScivesco}},
  \code{\link{importGridsuite}}, \code{\link{importTxt}}
}
