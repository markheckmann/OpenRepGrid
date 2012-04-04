\docType{package}
\name{OpenRepGrid-internal}
\alias{OpenRepGrid-internal}
\alias{OpenRepGrid-internal-package}
\title{\pkg{OpenRepGrid}: internal functions overview for developers.}
\description{
  This page is a help for developers: these functions are
  usually not needed by the casual user. The internal
  functions have a twofold goal 1) to provide means for
  advanced numerical grid analysis and 2) to facilitate
  function development. The function for these purposes are
  internal, i.e. they are not visible in the package
  documentation. Nonetheless they do have a documentation
  that can be accesses in the same way as for other
  functions. More in the details section.
}
\details{
  \bold{Functions for advanced grid analysis} \cr \cr The
  package provides functions to facilitate numerical
  research for grids. These comprise the generation of
  random data, permutation of grids etc. to facilitate
  Monte Carlo simulations, batch analysis of grids and
  other methods. With R as an underlying framework, the
  results of grid analysis easily lend themselves to
  further statistical processing and analysis within R.
  This is one of the central advantages for researchers
  compared to other standard grid software. The following
  table lists several functions for these purposes.

  \tabular{ll}{ \code{\link{randomGrid}} \tab \cr
  \code{\link{randomGrids}} \tab \cr
  \code{\link{permuteConstructs}} \tab \cr
  \code{\link{permuteGrid}} \tab \cr
  \code{\link{quasiDistributionDistanceSlater}} \tab \cr }

  \bold{Modules for function development} \cr \cr Beside
  the advanced analysis feature the developer's functions
  comprise low-level modules to create new functions for
  grid analysis. Though the internal structure of a repgrid
  object in R is simple (type e.g. \code{str(bell2010, 2)}
  to get an impression), it is convenient to not have to
  deal with access on this level. Several function like
  e.g. \code{getElementNames} are convenient wrappers that
  perform standard tasks needed when implementing new
  functions. The following table lists several functions
  for these purposes.

  \tabular{ll}{ \code{\link{getRatingLayer}} \tab \cr
  \code{\link{getNoOfConstructs}} \tab \cr
  \code{\link{getNoOfElements}} \tab \cr
  \code{\link{getScale}} \tab \cr
  \code{\link{getScaleMidpoint}} \tab \cr
  \code{\link{getConstructNames}} \tab \cr
  \code{\link{getConstructNames2}} \tab \cr
  \code{\link{getElementNames}} \tab \cr
  \code{\link{bindConstructs}} \tab \cr
  \code{\link{doubleEntry}} \tab \cr }

  \bold{Other internal functions} \cr

  \tabular{ll}{ \code{\link{importTxtInternal}} \tab \cr }
}
\author{
  Currently the \pkg{OpenRepGrid} development team is Mark
  Heckmann.  Everyone who is interested in developing the
  package is invited to join.

  The \pkg{OpenRepGrid} package is developed at R-Forge
  (\url{http://openrepgrid.r-forge.r-project.org}).  The
  R-Forge site provides information and mailing lists for
  help queries and bug reports.  Bug reports can also be
  emailed to the package maintainer or issued under
  \url{http://www.openrepgrid.org}. The maintainer at
  R-Forge is Mark Heckmann <heckmann(at)uni-bremen.de>.
}
\keyword{package}

