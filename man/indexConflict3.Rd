\name{indexConflict3}
\alias{indexConflict3}
\title{Conflict or inconsistenciy measure for grids (Bell, 2004) based on distances.}
\usage{
  indexConflict3(x, p = 2, output = 1, e.out = NA,
    e.threshold = NA, c.out = NA, c.threshold = NA,
    trim = 20, digits = 1, discrepancies = TRUE)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{p}{The power of the Minkowski distance. \code{p=2}
  will result in euclidean distances, \code{p=1} in city
  block distances. Any other Minkowski metric can be used
  as well.}

  \item{output}{Type of output. \code{output=1} will print
  all results to the console, \code{output=2} will only
  print the detailed statistics for elements and
  constructs, \code{0} will surpress all console output.}

  \item{e.out}{Numeric. A vector giving the indexes of the
  elements for which detailed stats (number of conflicts
  per element, discrepancies for triangles etc.) are
  promted (default code{NA}, i. e. no detailed stats).}

  \item{e.threshold}{Numeric. Detailed stats for elements
  with a an attributable percentage to the overall
  conflicts higher than the supplied value are printed
  (default \code{NA}).}

  \item{c.out}{Numeric. A vector giving the indexes of the
  constructs for which detailed stats (discrepancies for
  triangles etc.)  are promted (default code{NA}, i. e. no
  detailed stats).}

  \item{c.threshold}{Numeric. Detailed stats for constructs
  with a an attributable percentage to the overall
  conflicts higher than the supplied value are printed
  (default \code{NA}).}

  \item{trim}{The number of characters a construct
  (element) is trimmed to (default is \code{10}). If
  \code{NA} no trimming is done. Trimming simply saves
  space when displaying the output.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}

  \item{discrepancies}{Logical. Whether to show matrices of
  discrepancies in detailed element and construct stats
  (default \code{TRUE}).}
}
\value{
  A list (invisibly) containing containing: \cr
  \item{potential}{number of potential conflicts}
  \item{actual}{count of actual conflicts}
  \item{overall}{percentage of conflictive relations}
  \item{e.count}{number of involvements of each element in
  conflictive relations} \item{e.perc}{percentage of
  involvement of each element in total of conflictive
  relations} \item{e.count}{number of involvements of each
  construct in conflictive relation}
  \item{c.perc}{percentage of involvement of each construct
  in total of conflictive relations}
  \item{e.stats}{detailed statistics for prompted elements}
  \item{c.stats}{detailed statistics for prompted
  constructs}
}
\description{
  Conflict measure as proposed by Bell (2004).
}
\details{
  Measure of conflict or inconsistency as proposed by Bell
  (2004). The identification of conflict is based on
  distances rather than correlations as in other measures
  of conflict \code{\link{indexConflict1}} and
  \code{\link{indexConflict2}}. It assesses if the
  distances between all components of a triad, made up of
  one element and two constructs, satisfies the "triangle
  inequality" (cf. Bell, 2004). If not, a triad is regarded
  as conflictive. An advantage of the measure is that it
  can be interpreted not only as a global measure for a
  grid but also on an element, construct, and element by
  construct level making it valuable for detailed feedback.
  Also, differences in conflict can be submitted to
  statistical testing procedures.

  Status: working; output for euclidean and manhattan
  distance checked against Gridstat output. \cr TODO:
  standardization and z-test for discrepancies; Index of
  Conflict Variation.
}
\examples{
\dontrun{

 ### TODO ###
}
}
\author{
  Mark Heckmann
}
\references{
  Bell, R. C. (2004). A new approach to measuring
  inconsistency or conflict in grids. Personal Construct
  Theory & Practice, (1), 53-59.
}
\seealso{
  See \code{\link{indexConflict1}} and
  \code{\link{indexConflict2}} for conflict measures based
  on triads of correlations.
}

