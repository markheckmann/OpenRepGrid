\docType{package}
\name{OpenRepGrid-overview}
\alias{OpenRepGrid-overview}
\alias{OpenRepGrid-overview-package}
\title{\pkg{OpenRepGrid}: Annotated overview of package functions.}
\description{
  This documentation page contains an overview over the
  package functions ordered by topics. The best place to
  start learning OpenRepGrid will be the package website
  \url{http://www.openrepgrid.org} though.
}
\section{Functions sorted by topic}{
  \bold{Manipulating grids} \cr

  \tabular{ll}{ \code{\link{left}} \tab Move construct(s)
  to the left \cr \code{\link{right}} \tab Move
  construct(s) to the right \cr \code{\link{up}} \tab Move
  construct(s) upwards \cr \code{\link{down}} \tab Move
  construct(s) downwards \cr }

  \bold{Loading and saving data} \cr

  \bold{Analyzing constructs} \cr

  \bold{Analyzing elements} \cr

  \bold{Visual representation} \cr

  \bold{Index measures} \cr
}

\section{Grid datasets}{
  \pkg{OpenRepGrid} already contains some ready to use grid
  data sets. Most of the datasets are taken from the
  literature. To output the data simply type Type the name
  of the dataset to the console and press enter. \cr

  \emph{Single grids} \cr

  \tabular{ll}{ \code{\link{bell2010}} \tab Grid data from
  a study by Haritos et al. (2004) on role titles; used for
  demonstration of construct alignment in Bell (2010, p.
  46). \cr \code{\link{bellmcgorry1992}} \tab Grid from a
  psychotic patient used in Bell (1997, p. 6). Data
  originated from a study by Bell and McGorry (1992). \cr
  \code{\link{boeker}} \tab Grid from seventeen year old
  female schizophrenic patient undergoing last stage of
  psychoanalytically oriented psychotherapy (Boeker, 1996,
  p. 163). \cr \code{\link{fbb2003}} \tab Dataset used in
  \emph{A manual for Repertory Grid Technique} (Fransella,
  Bell, & Bannister, 2003b, p. 60). \cr
  \code{\link{feixas2004}} \tab Grid from a 22 year old
  Spanish girl suffering self-worth problems (Feixas &
  Saul, 2004, p. 77). \cr \code{\link{mackay1992}} \tab
  Dataset \emph{Grid C} used in Mackay's paper on
  inter-element correlation (1992, p. 65). \cr
  \code{\link{leach2001a}}, \code{\link{leach2001b}} \tab
  Pre- (a) and post-therapy (b) dataset from sexual child
  abuse survivor (Leach, Freshwater, Aldridge, &
  Sunderland, 2001, p. 227). \cr \code{\link{raeithel}}
  \tab Grid data to demonstrate the use of Bertin diagrams
  (Raeithel, 1998, p. 223). The context of its
  administration is unknown. \cr \code{\link{slater1977a}}
  \tab Drug addict grid dataset from (Slater, 1977, p. 32).
  \cr \code{\link{slater1977b}} \tab Grid dataset (ranked)
  from a seventeen year old female psychiatric patient
  (Slater, 1977, p. 110) showing depression, anxiety and
  self-mutilation.  The data was originally reported by
  Watson (1970).\cr }

  \emph{Multiple grids} \cr

  NOT YET AVAILABLE \cr
}

\section{Functions for developers}{
  \pkg{OpenRepGrid}: internal functions overview for
  developers. \cr

  Below you find a guide for developers: these functions
  are usually not needed by the casual user. The internal
  functions have a twofold goal 1) to provide means for
  advanced numerical grid analysis and 2) to facilitate
  function development. The function for these purposes are
  internal, i.e. they are not visible in the package
  documentation. Nonetheless they do have a documentation
  that can be accesses in the same way as for other
  functions. More in the details section.

  \bold{Functions for advanced grid analysis} \cr

  The package provides functions to facilitate numerical
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

  \bold{Modules for function development} \cr

  Beside the advanced analysis feature the developer's
  functions comprise low-level modules to create new
  functions for grid analysis. Though the internal
  structure of a repgrid object in R is simple (type e.g.
  \code{str(bell2010, 2)} to get an impression), it is
  convenient to not have to deal with access on this level.
  Several function like e.g. \code{getElementNames} are
  convenient wrappers that perform standard tasks needed
  when implementing new functions. The following table
  lists several functions for these purposes.

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
  Current members of the \pkg{OpenRepGrid} development
  team: Mark Heckmann.  Everyone who is interested in
  developing the package is invited to join.

  The \pkg{OpenRepGrid} package development is hosted on
  github
  (\url{http://github.com/markheckmann/OpenRepGrid}).  The
  github site provides information and allows to file bug
  reports or feature requests.  Bug reports can also be
  emailed to the package maintainer or issued on
  \url{http://www.openrepgrid.org} under section
  \emph{Suggestions/Issues}.  The package maintainer at
  github is Mark Heckmann <heckmann(at)uni-bremen.de>.
}
\keyword{package}

