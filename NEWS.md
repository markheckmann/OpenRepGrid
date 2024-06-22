# OpenRepGrid 0.1.15 (dev version)

* add `LICENSE.md` file
* `rgl` installation is now optional (close #41)
* rename `ward` method to `ward.D` in `cluster` (fix #36) 
* `indexDDI` and `indexUncertainty`, two dispersion indexes for dependency (e.g., situation-resource) grids added. Jon Raskin kindly helped with the documentation.

# OpenRepGrid 0.1.14

* remove TCLTK import. No interactive windows any more.
* move CI/CD from Travis to github actions
* resolve conflicts while loading packages
* update docs for indexPolarity
* reverse as alternative to swapPoles
* indexBieri: Bieri index of cognitive complexity
* matches: count rating matches in pairs of constructs and elements 
* reorder method to invert construct and/or element order
* dilemmatic construct (indexDilemmatic)
* polarization measure (indexPolarization)
* new print option for indexIntensity
* normalize argument in distance and indexSelfConstruction
* plot method for indexDilemma to produce network graphs
* add logo to documentation
* cognitive self construction: self vs. ideal vs. others (indexSelfConstruction)
* addAvgElement to add an average element
* rep method for regrid objects creates gridlist 
* summary measures for indexDilemma (PID, IID, PICID)
* midpoint function (alias for getScaleMidpoint)
* print method for indexDilemma
* fix indexDilemma output bug (#17, thanks to José Antonio González Del Puerto aka @MindCartographer)
* perturbation of grid ratings by perturbate and grids_perturbate
* indexPvaff now uses PCA of construct centered raw data in line with biplot (and Gridcor) 
* allow blanks at end of line after tags in importTxt
* grids_leave_n_out and grids_bootstrap for resampling constructs
* gridlist class, a simple list of repgrid objects

# OpenRepGrid 0.1.13 

  * indexDilemma was improved and fixed (thanks to Diego Vitali aka @artoo-git)
  * biplot2d does now hide construct points as default setting (cex=0)
  * setting a rating value outside the defined scale range now throws an error
  * 'ratings' to access and replace grid ratings added
  * 'elements' added to get and set element names replaces 'getElementNames' and 'eNames' which have become deprecated.
  * 'constructs', 'leftpoles', and 'rightpoles' added to get and set construct poles replace 'getConstructNames' and 'cNames' which have become  deprecated.

# OpenRepGrid 0.1.11

  * saveAsExcel to save grids as Microsoft Excel files
  * replace xlsx by openxlsx to import Excel files to get rid of JRE dependency
  
# OpenRepGrid 0.1.10

  * indexDilemma: improved implicative dilemmas (thanks to Alejandro García, (#24, @j4n7)
  * changelog file as place for documenting changes removed. All changes now in NEWS
  
# OpenRepGrid 0.1.9

  * align parameter added to cluster
  * importTxt will now erase empty lines
  * changed default settings for implicative dilemmas
  * dependency on xlsx removed (issue #15)

# OpenRepGrid 0.1.8

  * importGridstat can now import multigrid files
  * bug fix in importTxt: negative values are read in again

# OpenRepGrid 0.1.7

* print functions for most grid indexes added, former output argument no longer exists
* settings function added. Settings can now be modified, saved and loaded
* clusterBoot function for p-values in cluster analysis
* functions by topic overview documentation
* bindConstructs can now take many grids and/or lists of grids as arguments. Element names are now matched and reordered automatically
* '+' can now be used to concatenated two grids or list of grids
* importExcel function added
* importTxt: ratings and range can now have an arbitrary number of blanks or tabs between the values. The characters - (minus) and ? (question mark) are read in as missing values (NA). The file extensions .TXT (capitals) is now recognized
* clusterBoot added:implementation of pvclust functions

# OpenRepGrid 0.1.6

* biplot functions fixed. They work again.

# OpenRepGrid 0.1.5

* First release version available on CRAN.
* method dim to return size of a grid. A simple wrapper around getNoOfConstructs and getNoOfElements
* package description updated
* prob argument in distanceXXX functions added
* bug in makeRepgrid corrected: ratings now read by row

# OpenRepGrid 0.1.4
  
* demo added
* bug corrected in importScivesco
* changed package description
* prob parameter: randomGrid(s), quasiDistributionDistanceSlater (dev)
* distanceNormalized

# OpenRepGrid 0.1.3
  
* permuteConstructs (dev)
* update openrepgrid-internal
* permuteGrid (dev)
* update zzz
* corrected COLLATION order

# OpenRepGrid 0.1.2
  
* polishing of console output for several functions:
* constructCor
* elementCor
* statsConstructs
* statsElements
* distance
* alignByLoadings
* some others might have been changed (undocumented)

# OpenRepGrid 0.1.1

New Features

* indexConflict1: unbalanced triads (Slade & Sheehan)
* indexConflict2: unbalanced triads (Bassler et al.)
* indexConflict3: triangle inequalities
* indexDilemma: detect implicative dilemmas
* data leach2001a and leach2001b

Internal changes

* getConstructNames2
* getElementNames2
* formatMatrix

# OpenRepGrid 0.1.0

New Features

* Slater distance
* Hartmann distance

# OpenRepGrid 0.0.9

New Features

* zoom argument in biplots
* variance explained options in biplots
* color controls in biplots for points and labels
* no overplotting with high zoom values
* PVAFF index
* frames for biplot3d
* intensity index

# OpenRepGrid 0.0.8

New Features

* construct lines in drawBiplot2d
* saveAsTxt
* new examples added

# OpenRepGrid 0.0.7

New Features

* updated documentation files
* changes in addVarianceExplainedToBiplot2d
* alignByIdeal

# OpenRepGrid 0.0.6

New Features

* biplot3d (new version)
* biplotEsa2d
* biplotEsa3d
* biplotSlater2d
* biplotSlater3d
* home

# OpenRepGrid 0.0.5

New Features
  
* center
* normalize

# OpenRepGrid 0.0.4

New Features
  
* not documented

# OpenRepGrid 0.0.3

New Features:
  
* distance
* importGridsuite
* importScivesco
* importGridstat
* importGridcor
* importTxt
* constructSomers i.e. Somers' d
* getRatingLayers (internal function)
* cluster (for constructs)
* data feixas2004
* data fbb2003
* constructRmsCor
* constructPca

# OpenRepGrid 0.0.2

* Several new Features

# OpenRepGrid 0.0.1

* First version (2010-12-03)
