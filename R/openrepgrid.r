#' \pkg{OpenRepGrid}: an R package for the analysis of repertory grids. 
#'
#' The \pkg{OpenRepGrid} package provides tools for the analysis of repertory grid data.
#' The repertory grid is a method devised by George Alexander Kelly
#' in his seminal work "The Psychology of Personal Constructs" published in 1955.
#' The repertory grid has been used in and outside the context of Personal Construct 
#' Psychology (PCP) in a broad range of fields. For an introduction into the 
#' technique see e.g. Fransella, Bell and Bannister (2003).
#' 
#'
#' @author    Currently the \pkg{OpenRepGrid} development team is Mark Heckmann. 
#'            Everyone who is interested in developing the package is invited to join.
#'
#'            The \pkg{OpenRepGrid} package is developed at R-Forge (\url{http://r-forge.r-project.org/projects/openrepgrid/}).
#'            The R-Forge site provides information and mailing lists for help queries and bug reports. 
#'            Bug reports can also be emailed to the package maintainer or issued on 
#'            \url{http://www.openrepgrid.org} under section \emph{Suggestions/Issues}.
#'            The package maintainer at R-Forge is Mark Heckmann  <heckmann(at)uni-bremen.de>.
#'
#' @note      To get started with \pkg{OpenRepGrid} visit the project's home under \url{www.openrepgrid.org}. 
#'            On this site you will find tutorials, explanation about the theory, methods of analysis and 
#'            the according R code.
#'
#'            To see the preferable citation of the \pkg{OpenRepGrid} package, type 
#'            \code{citation("OpenRepGrid")} into the R console.
#'
#'            Disclaimer: Note that the package is distributed under the 
#'            \href{http://www.gnu.org/licenses/gpl-2.0.html}{GPL 2 license}.
#'            It is work in progress and is continuously being improved by hopefully 
#'            numerous contributors. It may contain bugs and errors.
#'            There is no warranty whatsoever for the program.
#'
#' @references  Fransella, F., Bell, R. C., & Bannister, D. (2003). \emph{A Manual for Repertory 
#'                  Grid Technique (2. Ed.)}. Chichester: John Wiley & Sons.
#'
#'              Kelly, G. A. (1955). \emph{The psychology of personal constructs. Vol. I, II.} 
#'                  New York: Norton, (2nd printing: 1991, Routledge, London, New York).
#'
#' @keywords package repgrid
#' @name OpenRepGrid
#' @docType package
#'
NULL


###############################################################################


#' \pkg{OpenRepGrid}: internal functions overview for developers.
#'
#' This page is a help for developers: these functions are usually 
#' not needed by the casual user. The internal functions have a twofold goal
#' 1) to provide means for advanced numerical grid analysis and 2) 
#' to facilitate function development. The function for these purposes
#' are internal, i.e. they are not visible in the package documentation.
#' Nonetheless they do have a documentation that
#' can be accesses in the same way as for other functions.
#' More in the details section.
#' 
#' \bold{Functions for advanced grid analysis} \cr \cr
#' The package provides functions to facilitate numerical research for grids. 
#' These comprise the generation of random data, permutation of grids etc. 
#' to facilitate Monte Carlo simulations, batch analysis of grids and other methods. 
#' With R as an underlying framework, the results of grid analysis easily lend 
#' themselves to further statistical processing and analysis within R. 
#' This is one of the central advantages for researchers compared to other 
#' standard grid software. The following table lists several functions for these purposes.
#'
#' \tabular{ll}{
#' \code{\link{randomGrid}}                       \tab  \cr
#' \code{\link{randomGrids}}                      \tab  \cr
#' \code{\link{permuteConstructs}}                \tab  \cr
#' \code{\link{permuteGrid}}                      \tab  \cr
#' \code{\link{quasiDistributionDistanceSlater}}  \tab  \cr
#' }
#'
#' \bold{Modules for function development} \cr \cr
#' Beside the advanced analysis feature the developer's functions comprise 
#' low-level modules to create new functions for grid analysis. 
#' Though the internal structure of a repgrid object in R is simple 
#' (type e.g. \code{str(bell2010, 2)} to get an impression), it is convenient 
#' to not have to deal with access on this level. Several function like e.g. 
#' \code{getElementNames} are convenient wrappers that perform standard tasks 
#' needed when implementing new functions. The following table lists several 
#' functions for these purposes.
#'
#' \tabular{ll}{
#' \code{\link{getRatingLayer}}       \tab    \cr
#' \code{\link{getNoOfConstructs}}    \tab    \cr
#' \code{\link{getNoOfElements}}      \tab    \cr
#' \code{\link{getScale}}             \tab    \cr
#' \code{\link{getScaleMidpoint}}     \tab    \cr
#' \code{\link{getConstructNames}}    \tab    \cr
#' \code{\link{getConstructNames2}}   \tab    \cr
#' \code{\link{getElementNames}}      \tab    \cr
#' \code{\link{bindConstructs}}       \tab    \cr
#' \code{\link{doubleEntry}}          \tab    \cr
#' }
#'
#' \bold{Other internal functions} \cr
#'
#' \tabular{ll}{
#' \code{\link{importTxtInternal}}  \tab  \cr
#' }
#'
#' @author    Currently the \pkg{OpenRepGrid} development team is Mark Heckmann. 
#'            Everyone who is interested in developing the package is invited to join.
#'
#'            The \pkg{OpenRepGrid} package is developed at R-Forge (\url{http://openrepgrid.r-forge.r-project.org}).
#'            The R-Forge site provides information and mailing lists for help queries and bug reports. 
#'            Bug reports can also be emailed to the package maintainer or issued under 
#'            \url{http://www.openrepgrid.org}. The maintainer at R-Forge is Mark Heckmann 
#'            <heckmann(at)uni-bremen.de>.
#'
#' @name OpenRepGrid-internal
#' @keywords package
#' @docType package
#'
NULL
