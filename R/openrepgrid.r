############################### Package description ###########################

#' `OpenRepGrid`: an R package for the analysis of repertory grids.
#'
#' \if{html}{\figure{logo.png}{options: style='float: right; padding-left: 10px;' alt='logo' width='120'}} The
#' `OpenRepGrid` package provides tools for the analysis of repertory grid data. The repertory grid is a method devised
#' by George Alexander Kelly in his seminal work "The Psychology of Personal Constructs" published in 1955. The
#' repertory grid has been used in and outside the context of Personal Construct Psychology (PCP) in a broad range of
#' fields. For an introduction into the technique see e.g. Fransella, Bell and Bannister (2003).
#'
#' @author
#'
#'   - Maintainer: Mark Heckmann ([@markheckmann](https://markheckmann.de))
#'
#'   - Contributors: Richard C. Bell, Alejandro García Gutiérrez (@j4n7), Diego Vitali (@artoo-git), José Antonio
#' González Del Puerto (@MindCartographer), Jonathan D. Raskin
#'
#'   - How to contribute: You can [contribute in various ways](https://docs.openrepgrid.org/CONTRIBUTING.html).
#' The `OpenRepGrid` code is hosted on [GitHub](https://github.com/markheckmann/OpenRepGrid), where you can issue bug
#' reports or feature requests. You may email your request to the package maintainer.
#'
#' @note To get started with `OpenRepGrid` visit the project's home under [openrepgrid.org](https://openrepgrid.org).
#'   On this site you will find tutorials, explanation about the theory, the analysis methods and the corresponding R
#'   code.
#'
#'   To see how to cite the `OpenRepGrid` package, type `citation("OpenRepGrid")` into the R console.
#'
#' @references Fransella, F., Bell, R. C., & Bannister, D. (2003). *A Manual for Repertory Grid Technique (2. Ed.).*
#'   Chichester: John Wiley & Sons.
#'
#'   Kelly, G. A. (1955). *The psychology of personal constructs. Vol. I, II.* New York: Norton, (2nd printing: 1991,
#'   Routledge, London, New York).
#'
#' @keywords package repgrid
#' @name OpenRepGrid
#' @docType package
#' @import methods graphics grid utils grDevices stringr abind psych XML pvclust dplyr
#' @rawNamespace import(stats, except=c(lag,filter))
#' @rawNamespace import(plyr, except = c(failwith,id,count,mutate,desc,rename,summarize,summarise,filter,arrange))
#' @importFrom colorspace HSV diverge_hcl hex hex2RGB
#' @importFrom crayon bold black red green yellow blue magenta cyan white silver
#'
"_PACKAGE"


#############################  Package overview  ##############################

#' \pkg{OpenRepGrid}: Annotated overview of package functions.
#'
#' This documentation page contains an overview over the package functions
#' ordered by topics. The best place to start learning OpenRepGrid will
#' be the package website <https://openrepgrid.org> though.
#'
#' @section Functions sorted by topic:
#'
#' **Manipulating grids** \cr
#'
#' \tabular{ll}{
#'    [left()]   \tab Move construct(s) to the left  \cr
#'    [right()]  \tab Move construct(s) to the right \cr
#'    [up()]     \tab Move construct(s) upwards \cr
#'    [down()]   \tab Move construct(s) downwards \cr
#' }
#'
#' **Loading and saving data** \cr
#'
#' \tabular{ll}{
#' [importGridcor()]  \tab Import GRIDCOR data files \cr
#' [importGridstat()]	\tab Import Gridstat data files \cr
#' [importGridsuite()] \tab	Import Gridsuite data files \cr
#' [importScivesco()]	\tab Import sci:vesco data files \cr
#' [importTxt()]	\tab Import grid data from a text file \cr
#'  \tab \cr
#' [saveAsTxt()] \tab Save grid in a text file (txt) \cr
#' }
#'
#' **Analyzing constructs** \cr
#'
#' Descriptive statistics of constructs
#' Construct correlations
#' distance
#' Root mean square of inter-construct correlations
#' Somers' D
#' Principal component analysis (PCA) of construct correlation matrix
#' Cluster analysis of constructs
#'
#' **Analyzing elements** \cr
#'
#' **Visual representation** \cr
#'
#' \tabular{ll}{
#' *Bertin plots* \tab \cr
#'   \tab \cr
#'   [bertin()]             \tab  Make Bertin display of grid data \cr
#'   [bertinCluster()]      \tab	Bertin display with corresponding cluster analysis \cr
#'   \tab \cr
#'   \tab \cr
#' *Biplots* \tab \cr
#'   \tab \cr
#' [biplot2d()]             \tab Draw a two-dimensional biplot \cr
#' [biplotEsa2d()]          \tab Plot an eigenstructure analysis (ESA) biplot in 2D \cr
#' [biplotSlater2d()]       \tab Draws Slater's INGRID biplot in 2D \cr
#'    \tab \cr
#' [biplotPseudo3d()]       \tab See 'biplotPseudo3d' for its use. Draws a biplot of the grid in 2D with depth impression (pseudo 3D) \cr
#' [biplotEsaPseudo3d()]    \tab Plot an eigenstructure analysis (ESA) in 2D grid with 3D impression (pseudo 3D) \cr
#' [biplotSlaterPseudo3d()] \tab Draws Slater's biplot in 2D with depth impression (pseudo 3D) \cr
#'    \tab \cr
#' [biplot3d()]	            \tab Draw grid in rgl (3D device) \cr
#' [biplotEsa3d()]	        \tab Draw the eigenstructure analysis (ESA) biplot in rgl (3D device) \cr
#' [biplotSlater3d()]	      \tab Draw the Slater's INGRID biplot in rgl (3D device) \cr
#'    \tab \cr
#' [biplotSimple()]         \tab A graphically unsophisticated version of a biplot \cr
#' }
#'
#' **Index measures** \cr
#'
#' \tabular{ll}{
#' [indexConflict1()]	  \tab Conflict measure for grids (Slade & Sheehan, 1979) based on correlations \cr
#' [indexConflict2()]	  \tab Conflict measure for grids (Bassler et al., 1992) based on correlations \cr
#' [indexConflict3()]	  \tab Conflict or inconsistency measure for grids (Bell, 2004) based on distances \cr
#' [indexDilemma()]	    \tab Detect implicative dilemmas (conflicts) \cr
#'    \tab \cr
#' [indexIntensity()]	  \tab Intensity index \cr
#' [indexPvaff()]	      \tab Percentage of Variance Accounted for by the First Factor (PVAFF) \cr
#'    \tab \cr
#' [indexBias()]        \tab Calculate 'bias' of grid as defined by Slater (1977) \cr
#' [indexVariability()]	\tab Calculate 'variability' of a grid as defined by Slater (1977) \cr
#' }
#'
#' **Special features** \cr
#'
#' \tabular{ll}{
#' [alignByIdeal()]     \tab  Align constructs using the ideal element to gain pole preferences \cr
#' [alignByLoadings()]  \tab	Align constructs by loadings on first principal component \cr
#' [reorder2d()]        \tab Order grid by angles between construct and/or elements in 2D \cr
#' }
#'
#' @section Settings:
#'
#' \pkg{OpenRepGrid} uses several default settings e.g. to determine
#' how many construct characters to display by default when displaying a grid.
#' The function `settings` can be used to show and change these settings.
#' Also it is possible to store the settings to a file and load the settings
#' file to restore the settings.
#'
#' \tabular{ll}{
#' [settings()]      \tab Show and modify global settings for OpenRepGrid \cr
#' [settingsSave()]  \tab Save OpenRepGrid settings to file \cr
#' [settingsLoad()]  \tab Load OpenRepGrid settings from file\cr
#' }
#'
#' @section Grid datasets:
#'
#' \pkg{OpenRepGrid} already contains some ready to use grid data sets. Most of
#' the datasets are taken from the literature. To output the data simply type
#' Type the name of the dataset to the console and press enter. \cr
#'
#' *Single grids* \cr
#'
#' \tabular{ll}{
#' [bell2010()]         \tab Grid data from a study by Haritos et al. (2004)
#'                                     on role titles; used for demonstration of
#'                                     construct alignment in Bell (2010, p. 46). \cr
#' [bellmcgorry1992()]  \tab Grid from a psychotic patient used in Bell
#'                                     (1997, p. 6). Data originated from a study
#'                                     by Bell and McGorry (1992). \cr
#' [boeker()]           \tab Grid from seventeen year old female schizophrenic
#'                                     patient undergoing last stage of psychoanalytically
#'                                     oriented psychotherapy (Boeker, 1996, p. 163). \cr
#' [fbb2003()]          \tab Dataset used in *A manual for Repertory Grid
#'                                     Technique* (Fransella, Bell, & Bannister, 2003b, p. 60). \cr
#' [feixas2004()]       \tab Grid from a 22 year old Spanish girl suffering
#'                                     self-worth problems (Feixas & Saul, 2004, p. 77). \cr
#' [mackay1992()]	      \tab Dataset *Grid C* used in Mackay's paper on inter-element
#'                                     correlation (1992, p. 65). \cr
#' [leach2001a()], [leach2001b()] \tab	Pre- (a) and post-therapy (b) dataset from
#'                                     sexual child abuse survivor (Leach, Freshwater,
#'                                     Aldridge, & Sunderland, 2001, p. 227). \cr
#' [raeithel()]         \tab Grid data to demonstrate the use of Bertin diagrams
#'                                     (Raeithel, 1998, p. 223). The context of its
#'                                     administration is unknown. \cr
#' [slater1977a()]      \tab Drug addict grid dataset from (Slater, 1977, p. 32). \cr
#' [slater1977b()]      \tab Grid dataset (ranked) from a seventeen year old
#'                                     female psychiatric patient (Slater, 1977, p. 110)
#'                                     showing depression, anxiety and self-mutilation.
#'                                     The data was originally reported by Watson (1970).\cr
#' }
#'
#' *Multiple grids* \cr
#'
#' NOT YET AVAILABLE \cr
#'
#'
#' @section Functions for developers:
#'
#' \pkg{OpenRepGrid}: internal functions overview for developers. \cr
#'
#' Below you find a guide for developers: these functions are usually
#' not needed by the casual user. The internal functions have a twofold goal
#' 1) to provide means for advanced numerical grid analysis and 2)
#' to facilitate function development. The function for these purposes
#' are internal, i.e. they are not visible in the package documentation.
#' Nonetheless they do have a documentation that
#' can be accesses in the same way as for other functions.
#' More in the details section.
#'
#' **Functions for advanced grid analysis** \cr
#'
#' The package provides functions to facilitate numerical research for grids.
#' These comprise the generation of random data, permutation of grids etc.
#' to facilitate Monte Carlo simulations, batch analysis of grids and other methods.
#' With R as an underlying framework, the results of grid analysis easily lend
#' themselves to further statistical processing and analysis within R.
#' This is one of the central advantages for researchers compared to other
#' standard grid software. The following table lists several functions for these purposes.
#'
#' \tabular{ll}{
#' [randomGrid()]                       \tab  \cr
#' [randomGrids()]                      \tab  \cr
#' [permuteConstructs()]                \tab  \cr
#' [permuteGrid()]                      \tab  \cr
#' [quasiDistributionDistanceSlater()]  \tab  \cr
#' }
#'
#' **Modules for function development** \cr
#'
#' Beside the advanced analysis feature the developer's functions comprise
#' low-level modules to create new functions for grid analysis.
#' Though the internal structure of a repgrid object in R is simple
#' (type e.g. `str(bell2010, 2)` to get an impression), it is convenient
#' to not have to deal with access on this level. Several function like e.g.
#' `getElementNames` are convenient wrappers that perform standard tasks
#' needed when implementing new functions. The following table lists several
#' functions for these purposes.
#'
#' \tabular{ll}{
#' [getRatingLayer()]       \tab Retrieve grid scores from grid object. \cr
#' [getNoOfConstructs()]    \tab Get the number of constructs in a grid object.    \cr
#' [getNoOfElements()]      \tab Get the number of elements in a grid object.   \cr
#' [dim()]                  \tab Get grid dimensions, i.e. constructs x elements.    \cr
#' [getScale()]             \tab Get minimum and maximum scale value used in grid.  \cr
#' [getScaleMidpoint()]     \tab Get midpoint of the grid rating scale.    \cr
#' [getConstructNames()]    \tab Get construct names.                       \cr
#' [getConstructNames2()]   \tab Get construct names (another newer version).      \cr
#' [getElementNames()]      \tab Retrieve element names of repgrid object.  \cr
#' [bindConstructs()]       \tab Concatenate the constructs of two grids.   \cr
#' [doubleEntry()]          \tab  Join the constructs of a grid with the same reversed constructs.  \cr
#' }
#'
#' **Other internal functions** \cr
#'
#' \tabular{ll}{
#' [importTxtInternal()]  \tab  \cr
#' }
#'
#' @author    Current members of the \pkg{OpenRepGrid} development team: Mark Heckmann.
#'            Everyone who is interested in developing the package is invited to join.
#'
#'            The \pkg{OpenRepGrid} package development is hosted on github (<https://github.com/markheckmann/OpenRepGrid>).
#'            The github site provides information and allows to file bug reports or feature requests.
#'            Bug reports can also be emailed to the package maintainer or issued on
#'            <https://openrepgrid.org> under section *Suggestions/Issues*.
#'            The package maintainer is Mark Heckmann <heckmann(dot)mark(at)gmail(dot)com>.
#'
#' @name OpenRepGrid-overview
#' @keywords package
#'
"_PACKAGE"
