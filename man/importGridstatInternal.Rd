% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.r
\name{importGridstatInternal}
\alias{importGridstatInternal}
\title{Parser for Gridstat data files.}
\usage{
importGridstatInternal(file, dir = NULL, min = NULL, max = NULL)
}
\arguments{
\item{file}{filename including path if file is not in current working
directory. File can also be a complete URL. The fileformat
is .dat.}

\item{dir}{alternative way to supply the directory where the file is located
(default \code{NULL}).}

\item{min}{optional argument (\code{numeric}, default \code{NULL})
for minimum rating value in grid.}

\item{max}{optional argument (\code{numeric}, default \code{NULL})
for maximum rating value in grid.}
}
\value{
a list with imported parameters
}
\description{
Parse the file format that is used by the latest version of grid program
gridstat (Bell, 1998).
}
\note{
\if{html}{\out{<div class="sourceCode">}}\preformatted{     Note that the gridstat data format does not contain explicit
           information about the range of the rating scale (minimum and
           maximum). By default the range is inferred by scanning
           the ratings and picking the minimal and maximal values as rating
           range. You can set the minimal and maximal value by hand using the `min` and
           `max` arguments or by using the `setScale()` function.
           Note that if the rating range is not set, it may cause several
           functions to not work properly. A warning will be issued if the range is
           not set explicitly when using the importing function.

           The function only reads data from the latest GridStat version.
           The latest version allows the separation of the left and right pole
           by using on of the following symbols `/:-` (hyphen, colon and dash). Older versions may not
           separate                              the left and right pole. This will cause all labels to be assigned to
           the left pole only when importing. You may fix this by simply entering
           one of the construct separator symbols into the GridStat file between each
           left and right construct pole.

           The third line of a GridStat file may contain a no labels statement (i.e. a
           line containing any string of 'NOLA', 'NO L', 'NoLa', 'No L', 'Nola', 'No l',
           'nola' or 'no l'). In this case only ratings are supplied, hence, default
           names are assigned to elements and constructs.

     Email from Richard: The gridstat file has a fixed format with a title line, number
     of constructs and elements on second line. The third line can say No labels
     (actually it looks at the first 4 characters which can be any of 'NOLA','NO L',
     'NoLa','No L','Nola','No l','nola','no l') in which case it skips to the data and
     creates dummy labels for elements and constructs, otherwise it reads the construct
     labels then the element labels, then the data. Construct labels were originally
     stored as one, hence it didn't matter what the separator between left and right
     pole labels was, but in the latest version where constructs can be reversed, it
     looks for a fixed separator - one of slash(/), dash(-), or colon(:). Some of my
     old data files might not conform.
}\if{html}{\out{</div>}}
}
\examples{
\dontrun{

# supposing that the data file gridstat.dat is in the current working directory
file <- "gridstat.dat"
imp <- importGridstatInternal(file)

# specifying a directory (example)
dir <- "/Users/markheckmann/data"
imp <- importGridstatInternal(file, dir)

# using a full path (example)
imp <- importGridstatInternal("/Users/markheckmann/data/gridstat.dat")

# setting rating scale range
imp <- importGridstatInternal(file, dir, min = 1, max = 6)
}

}
\references{
Bell, R. C. (1998)  GRIDSTAT: A program for analyzing the data of a
repertory grid. Melbourne: Author.
}
\keyword{internal}
