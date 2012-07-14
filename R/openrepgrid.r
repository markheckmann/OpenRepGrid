
############################### Package description ###########################

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
#'            The \pkg{OpenRepGrid} package development is hosted on github (\url{http://github.com/markheckmann/OpenRepGrid}).
#'            The github site provides information and allows to file bug reports or feature requests.
#'            Bug reports can also be emailed to the package maintainer or issued on 
#'            \url{http://www.openrepgrid.org} under section \emph{Suggestions/Issues}.
#'            The package maintainer at github is Mark Heckmann  <heckmann(at)uni-bremen.de>.
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


############################ Package internal functions #######################

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


############################ Package topic overview ###########################

#' \pkg{OpenRepGrid}: Annotated overview of package functions.
#'
#' This doumentation page contains an overview over the package functions
#' ordered by topics. The best place to start learning OpenRepGrid will
#' be the package website \url{http://www.openrepgrid.org} though. 
#' 
#' @section Functions by topic: 
#' \bold{Manipulating grids} \cr \cr
#' 
#' \tabular{ll}{
#' \code{\link{left}}         \tab Move construct(s) to the left  \cr
#' \code{\link{right}}        \tab Move construct(s) to the right \cr
#' \code{\link{up}}           \tab Move construct(s) upwards \cr
#' \code{\link{down}}         \tab Move construct(s) downwards \cr
#' }
#'
#' \bold{Loading and saving data} \cr \cr
#' 
#' \bold{Analyzing constructs} \cr \cr
#' 
#' \bold{Analyzing elements} \cr \cr
#' 
#' \bold{Visual representation} \cr \cr
#' 
#' \bold{Index measures} \cr \cr
#' 
#' \bold{Package Data} \cr \cr
#' 
#' Type the name of the dataset to the console and press enter to see the grid. \cr
#' 
#' \emph{Single grids} \cr
#' 
#' \tabular{ll}{
#' \code{\link{bell2010}}         \tab Grid data from a study by Haritos et al. (2004) 
#'                                     on role titles; used for demonstration of 
#'                                     construct alignment in Bell (2010, p. 46). \cr
#' \code{\link{bellmcgorry1992}}  \tab Grid from a psychotic patient used in Bell 
#'                                     (1997, p. 6). Data originated from a study 
#'                                     by Bell and McGorry (1992). \cr
#' \code{\link{boeker}}           \tab Grid from seventeen year old female schizophrenic 
#'                                     patient undergoing last stage of psychoanalytically 
#'                                     oriented psychotherapy (Boeker, 1996, p. 163). \cr
#' \code{\link{fbb2003}}          \tab Dataset used in \emph{A manual for Repertory Grid 
#'                                     Technique} (Fransella, Bell, & Bannister, 2003b, p. 60). \cr
#' \code{\link{feixas2004}}	      \tab Grid from a 22 year old Spanish girl suffering 
#'                                     self-worth problems (Feixas & Saul, 2004, p. 77). \cr
#' \code{\link{mackkay1992}}	    \tab Dataset \emph{Grid C} used in Mackay's paper on inter-element 
#'                                     correlation (1992, p. 65). \cr
#' \code{\link{leach2001a, leach2001b}} \tab	Pre- (a) and post-therapy (b) dataset from 
#'                                     sexual child abuse survivor (Leach, Freshwater, 
#'                                     Aldridge, & Sunderland, 2001, p. 227). \cr
#' \code{\link{raeithel}}         \tab Grid data to demonstrate the use of Bertin diagrams 
#'                                     (Raeithel, 1998, p. 223). The context of its 
#'                                     administration is unknown. \cr
#' \code{\link{slater1997a}}      \tab Drug addict grid dataset from (Slater, 1977, p. 32). \cr
#' \code{\link{slater1977b}}      \tab grid dataset (ranked) from a seventeen year old 
#'                                     female psychiatric patient (Slater, 1977, p. 110) 
#'                                     showing depression, anxiety and self-mutilation. 
#'                                     The data was originally reported by Watson (1970). \cr
#' }
#' 
#' \emph{Multiple grids} \cr
#' 
#' NOT YET AVAILABLE \cr
#'
#'
#'
#' @name OpenRepGrid-overview
#' @keywords package
#' @docType package
#'
NULL