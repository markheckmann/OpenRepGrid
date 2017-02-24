# OpenRepGrid 0.1.10 (released February 24, 2017) 

  * indexDilemma: improved implicative dilemmas (thanks to Alejandro García, (#24, @j4n7)
  * changelog file as place for documenting changes removed. All changes now in NEWS
  
# OpenRepGrid 0.1.9 (released September 06, 2014) 

  * align parameter added to cluster
  * importTxt will now erase empty lines
  * changed default settings for implicative dilemmas
  * dependency on xlsx removed (issue #15)

# OpenRepGrid 0.1.8 (released June 13, 2013)

  * importGridstat can now import multigrid files
  * bug fix in importTxt: negative values are read in again

# OpenRepGrid 0.1.7 (released May 25, 2013)

  * print functions for most grid indexes added, former output argument no longer exists
  * settings function added. Settings can now be modified, saved and loaded
  * clusterBoot function for p-values in cluster analysis
  * functions by topic overview documentation
  * bindConstructs can now take many grids and/or lists of grids as arguments. Element names are now matched and reordered automatically
  * '+' can now be used to concatenated two grids or list of grids
  * importExcel function added
  * importTxt: ratings and range can now have an arbitrary number of blanks or tabs between the values. The characters - (minus) and ? (question mark) are read in as missing values (NA). The file extensions .TXT (capitals) is now recognized
  * clusterBoot added:implementation of pvclust functions

# OpenRepGrid 0.1.6 (released April 5, 2012)

  * biplot functions fixed. They work again.

# OpenRepGrid 0.1.5 (released July 17, 2011)

	* First public version release version available on CRAN.
	
# OpenRepGrid 0.1.5 (2011-17-07) 
  * method dim to return size of a grid. A simple wrapper around getNoOfConstructs and getNoOfElements
  * package description updated
  * prob argument in distanceXXX functions added
  * bug in makeRepgrid corrected: ratings now read by row

#OpenRepGrid 0.1.4 (2011-05-31) 
  
  * demo added
  * bug corrected in importScivesco
  * changed package description
  * prob parameter: randomGrid(s), quasiDistributionDistanceSlater (dev)
  * distanceNormalized

# OpenRepGrid 0.1.3 (2011-05-27)
  
  * permuteConstructs (dev)
  * update openrepgrid-internal
  * permuteGrid (dev)
  * update zzz
  * corrected COLLATION order

# OpenRepGrid 0.1.2 (2011-05-21) 
  
  * polishing of console output for several functions:
  * constructCor
  * elementCor
  * statsConstructs
  * statsElements
  * distance
  * alignByLoadings
  * some others might have been changed (undocumented)

# OpenRepGrid 0.1.1 (2011-05-15) 

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

# OpenRepGrid 0.1.0 (2011-05-08) 

  New Features
  * Slater distance
  * Hartmann distance

# OpenRepGrid 0.0.9 (2011-05-05) 

  New Features
  * zoom argument in biplots
  * variance explained options in biplots
  * color controls in biplots for points and labels
  * no overplotting with high zoom values
  * PVAFF index
  * frames for biplot3d
  * intensity index

# OpenRepGrid 0.0.8 (2011-05-03) 

  New Features
  * construct lines in drawBiplot2d
  * saveAsTxt
  * new examples added

# OpenRepGrid 0.0.7 (2011-04-28) 

  New Features
  * updated documentation files
  * changes in addVarianceExplainedToBiplot2d
  * alignByIdeal

# OpenRepGrid 0.0.6 (2011-04-27) 

  New Features
  
  * biplot3d (new version)
  * biplotEsa2d
  * biplotEsa3d
  * biplotSlater2d
  * biplotSlater3d
  * home

# OpenRepGrid 0.0.5 (2011-04-20) 

  New Features
  * center
  * normalize

# OpenRepGrid 0.0.4 (2011-03-03) 

  New Features
  
  * not documented

# OpenRepGrid 0.0.3 (2011-03-03) 

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

# OpenRepGrid 0.0.2 (2010-01-06) 

  Several new Features

# OpenRepGrid 0.0.1 (2010-12-03) 

  * First version

