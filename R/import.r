################### 	import repgrid data from other (grid) programs ############
#
# programs currently supported:
#
# programs planned to be supported:
# - gridstat
# - Sci:Vesco
# - Gridsuite
# - Gridcor
# - idiogrid
#
# other supported formats:
# .txt
#
# Not supported:
# - Excel is not supported as it is no genuine grid program. It can be easily
#   used though to produce grid data that can be imported.
#



#' convertImportObjectToRepGridObject.
#'
#' Convert the returned object from an import function into a `repgrid`
#' object. Works for all importXInternal functions (except scivesco).
#'
#' @param import List with relevant entries.
#' @keywords internal
#' @export
#'
convertImportObjectToRepGridObject <- function(import) {
  # structure of import object:
  #
  # List of 9
  #  $ elements     :List of 3
  #  $ constructs   :List of 4
  #  $ emergentPoles:List of 4
  #  $ contrastPoles:List of 4
  #  $ ratings      :List of 4
  #  $ noConstructs : int 4
  #  $ noElements   : int 3
  #  $ minValue     : num 1
  #  $ maxValue     : num 1

  args <- list(
    name = unlist(import$elements), # elements
    l.name = unlist(import$emergentPoles), # left poles
    r.name = unlist(import$contrastPoles), # right poles
    scores = sapply(import$ratings, I)
  ) # ratings
  x <- makeRepgrid(args) # make repgrid
  x <- setScale(x, import$minValue, import$maxValue) # set scale range
  x
}


############################ GRIDSTAT #########################################

# gridstat output has the following form.
# 1) first line:  some description elements.
# 2) second line: number of constructs and elements
# 3) next n lines: constructs, elements
# 4) matrix of ratings
# These have to be parsed. For this purpose they are read in line by line.

# Heinz Boeker in Scheer & Catina (1996, p.163)
# 14 15
# balanced - get along with conflicts
# isolated - sociable
# closely integrated - excluded
# discursive - passive
# open minded - indifferent
# dreamy - dispassionate
# practically oriented - depressed
# playful - serious
# socially minded - selfish
# quarrelsome peaceful
# artistic - technical
# scientific - emotional
# introvert - extrovert
# wanderlust - home oriented
# self
# ideal self
# mother
# father
# kurt
# karl
# george
# martin
# elizabeth
# therapist
# irene
# childhood self
# self before illness
# self with delusion
# self as dreamer
# 1 4 2 2 3 5 2 5 4 2 6 2 2 3 3
# 3 6 3 5 5 4 5 4 5 4 4 4 2 2 3
# 2 2 2 3 5 3 2 3 2 3 3 4 4 5 3
# 4 1 3 1 2 4 2 3 3 2 3 3 3 5 4
# 2 1 2 1 2 4 4 2 4 2 6 3 2 2 3
# 4 5 3 5 4 5 4 5 4 4 6 3 3 3 2
# 2 1 3 2 3 3 3 2 2 3 2 3 3 3 3
# 4 5 4 3 4 3 2 3 4 4 5 3 2 4 3
# 2 1 3 2 4 5 4 1 3 2 6 3 3 3 3
# 5 5 5 5 5 2 5 2 4 4 1 6 5 5 5
# 5 1 2 4 3 5 3 2 4 3 3 4 4 4 4
# 2 1 5 3 4 4 5 3 4 1 6 4 2 3 3
# 4 5 4 6 5 3 5 3 5 2 5 2 2 2 3
# 1 1 4 2 4 5 2 5 5 3 6 1 1 2 1


#' Parser for Gridstat data files.
#'
#' Parse the file format that is used by the latest version of grid program
#' gridstat (Bell, 1998).
#'
#' @param file	  filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @param min	    optional argument (`numeric`, default `NULL`)
#'                for minimum rating value in grid.
#' @param max	    optional argument (`numeric`, default `NULL`)
#'                for maximum rating value in grid.
#' @return  a list with imported parameters
#'
#' @note          Note that the gridstat data format does not contain explicit
#'                information about the range of the rating scale (minimum and
#'                maximum). By default the range is inferred by scanning
#'                the ratings and picking the minimal and maximal values as rating
#'                range. You can set the minimal and maximal value by hand using the `min` and
#'                `max` arguments or by using the `setScale()` function.
#'                Note that if the rating range is not set, it may cause several
#'                functions to not work properly. A warning will be issued if the range is
#'                not set explicitly when using the importing function.
#'
#'                The function only reads data from the latest GridStat version.
#'                The latest version allows the separation of the left and right pole
#'                by using on of the following symbols `/:-` (hyphen, colon and dash). Older versions may not
#'                separate                              the left and right pole. This will cause all labels to be assigned to
#'                the left pole only when importing. You may fix this by simply entering
#'                one of the construct separator symbols into the GridStat file between each
#'                left and right construct pole.
#'
#'                The third line of a GridStat file may contain a no labels statement (i.e. a
#'                line containing any string of 'NOLA', 'NO L', 'NoLa', 'No L', 'Nola', 'No l',
#'                'nola' or 'no l'). In this case only ratings are supplied, hence, default
#'                names are assigned to elements and constructs.
#'
#'          Email from Richard: The gridstat file has a fixed format with a title line, number
#'          of constructs and elements on second line. The third line can say No labels
#'          (actually it looks at the first 4 characters which can be any of 'NOLA','NO L',
#'          'NoLa','No L','Nola','No l','nola','no l') in which case it skips to the data and
#'          creates dummy labels for elements and constructs, otherwise it reads the construct
#'          labels then the element labels, then the data. Construct labels were originally
#'          stored as one, hence it didn't matter what the separator between left and right
#'          pole labels was, but in the latest version where constructs can be reversed, it
#'          looks for a fixed separator - one of slash(/), dash(-), or colon(:). Some of my
#'          old data files might not conform.
#'
#' @export
#' @keywords internal
#' @references    Bell, R. C. (1998)  GRIDSTAT: A program for analyzing the data of a
#'                repertory grid. Melbourne: Author.
#' @examples \dontrun{
#'
#' # supposing that the data file gridstat.dat is in the current working directory
#' file <- "gridstat.dat"
#' imp <- importGridstatInternal(file)
#'
#' # specifying a directory (example)
#' dir <- "/Users/markheckmann/data"
#' imp <- importGridstatInternal(file, dir)
#'
#' # using a full path (example)
#' imp <- importGridstatInternal("/Users/markheckmann/data/gridstat.dat")
#'
#' # setting rating scale range
#' imp <- importGridstatInternal(file, dir, min = 1, max = 6)
#' }
#'
importGridstatInternal <- function(file, dir = NULL, min = NULL, max = NULL) {
  if (!is.null(dir)) {
    file <- paste(dir, file, sep = "/", collapse = "")
  }

  # read meta info
  l <- list() # list object to store all data in
  datainfo <- scan(
    file = file, what = "raw", # Read information line (first line)
    skip = 0, nlines = 1, quiet = TRUE
  )
  l$datainfo <- joinString(datainfo) # join single items to long string
  noConstructAndElements <- scan(
    file = file, what = "integer",
    skip = 1, nlines = 1, quiet = TRUE
  ) # Read number of elements and constructs
  l$noConstructs <- as.numeric(noConstructAndElements)[1] # no of constructs
  l$noElements <- as.numeric(noConstructAndElements)[2] # no of elements
  l$minValue <- NA
  l$maxValue <- NA

  # third line may contain a "no labels" statement in the first four characters.
  # In that case default labels are used and only the data is read (see email Richard).
  # No labels statement may be one of c('NOLA','NO L','NoLa','No L','Nola','No l','nola','no l').
  thirdLine <- scan(
    file = file, what = "character",
    skip = 2, nlines = 1, quiet = TRUE
  ) # read third line of file
  thirdLine <- joinString(thirdLine)
  firstFourChars <- substr(thirdLine, 1, 4) # extract first four chracters
  noLabels <- firstFourChars %in% c(
    "NOLA", "NO L", "NoLa", "No L",
    "Nola", "No l", "nola", "no l"
  ) # does third line have a no labels statement?

  # read constructs
  l$constructs <- list()
  l$emergentPoles <- list()
  l$contrastPoles <- list()
  if (!noLabels) { # read constructs if no labels statement is absent
    for (i in 1:l$noConstructs) {
      tmp <- scan(
        file = file, what = "character",
        skip = 2 + i - 1, nlines = 1, quiet = TRUE
      ) # read construct line by line
      l$constructs[[i]] <- joinString(tmp) # make one string
      poles <- strsplit(l$constructs[[i]], "[/:-]") # separate emergent and contrast pole by splitting at hyphen, colon or slash (see email from Richard)
      l$emergentPoles[[i]] <- trimBlanksInString(poles[[1]][1]) # save emergent pole
      l$contrastPoles[[i]] <- trimBlanksInString(poles[[1]][2]) # save contrast pole
    }
  } else { # make default constructs if no labels statement given
    constructs.left <- paste("construct left", seq_len(l$noConstructs))
    constructs.right <- paste("construct right", seq_len(l$noConstructs))
    l$constructs <- as.list(paste(constructs.left, constructs.right, sep = " - "))
    l$emergentPoles <- as.list(constructs.left)
    l$contrastPoles <- as.list(constructs.right)
  }

  # read elements
  l$elements <- list()
  if (!noLabels) { # read element names in the default case where  labels are supplied
    for (i in 1:l$noElements) {
      tmp <- scan(
        file = file, what = "character", # read elements line by line
        skip = 2 + l$noConstructs + (i - 1),
        nlines = 1, quiet = TRUE
      )
      l$elements[[i]] <- trimBlanksInString(joinString(tmp))
    }
  } else { # make default element names if no labels statement given
    l$elements <- as.list(paste("element", seq_len(l$noElements)))
  }

  # read ratings
  if (!noLabels) { # default case (labels supplied)
    skipLines.ratings <- 2 + l$noElements + l$noConstructs
  } else { # different starting position for reading of ratings in case of no labels statement
    skipLines.ratings <- 3 # skipping 1) info, 2) number of e and c and 3) no labels statement lines.
  }

  l$ratings <- list()
  for (i in 1:l$noConstructs) {
    tmp <- scan(
      file = file, what = "character", quiet = TRUE,
      skip = skipLines.ratings + (i - 1), # read ratings line by line
      nlines = 1
    )
    l$ratings[[i]] <- as.numeric(tmp)
  }

  # infer maximum rating value if not provided in max argument
  if (is.null(min)) {
    l$minValue <- min(unlist(l$ratings), na.rm = TRUE)
  } else {
    l$minValue <- min
  }

  if (is.null(max)) {
    l$maxValue <- max(unlist(l$ratings), na.rm = TRUE)
  } else {
    l$maxValue <- max
  }

  if (is.null(min) | is.null(max)) {
    warning("the minimum and/or the maximum value of the rating scale have not been set explicitly.",
      "The scale range was thus inferred by scanning the available ratings and may be wrong.",
      "See ?importGridstat for more information",
      call. = FALSE
    )
  }
  l
}

# Richards file
# file <- "/Users/unimitarbeiter/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridstat.dat"
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridstat.dat"

# "No labels" file
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridstat_nolabels.dat"
# file <- "/Users/unimitarbeiter/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridstat_nolabels.dat"


#' Converts a Gridstat multigrid file into temporary single grid files and
#' returns their path
#'
#' The format for a multigrid file resembles the single Gridstat data file. The
#' lines of the single files are simply placed below each other without any
#' blank lines in between. The function reads in a file and tests if it is a
#' multigrid file. Multigrid files are separated into single Gridstat temp
#' files. The file path for the temp files is returned. If the file is a single
#' grid files the path is left unaltered.
#'
#' @param file    Filenames of Gridstat file
#' @return  A vector containing the paths to the temp files
#' @export
#' @keywords internal
#'
multigridFileToSinglegridFiles <- function(file) {
  l <- readLines(file)
  r <- grepl("^[ \t]*[0-9]+[ \t]+[0-9]+ *$", l) # lines with number of c and e, i.e. two digits separated by space
  is.multigrid.file <- sum(r) > 1 # check if it is a multi-grid file
  if (is.multigrid.file) {
    pos <- which(r) - 1 # subtract 1 as it is preceeded by an info line, i.e. where the grid starts
    pos.ext <- c(pos, length(l) + 1) # add last position
    tmp.files <- vector("character")
    for (i in seq_along(pos)) { # save single grids to temp files
      lines <- l[pos.ext[i]:(pos.ext[i + 1] - 1)] # read info for single grid
      tmp.file <- tempfile("importGridstat_",
        fileext = ".dat"
      ) # generate temp file
      writeLines(lines, tmp.file) # write grid to temp file
      tmp.files <- c(tmp.files, tmp.file) # vector of temp file names
    }
    return(tmp.files)
  } else {
    return(file)
  }
}


#' Import Gridstat data files.
#'
#' Reads the file format that is used by the latest version of the grid
#' program gridstat (Bell, 1998).
#'
#' @param file	  Filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is `.dat`.
#' @param dir	    Alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @param min	    Optional argument (`numeric`, default `NULL`)
#'                for minimum rating value in grid.
#' @param max	    Optional argument (`numeric`, default `NULL`)
#'                for maximum rating value in grid.
#' @return  A single `repgrid` object in case one file and a list of `repgrid` objects in case multiple files are
#'   imported.
#'
#' @note  Note that the gridstat data format does not contain explicit information about the range of the rating scale
#'   used (minimum and maximum). By default the range is inferred by scanning the ratings and picking the minimal and
#'   maximal values as rating range. You can set the minimal and maximal value by hand using the `min` and `max`
#'   arguments or by using the `setScale()` function. Note that if the rating range is not set, it may cause several
#'   functions to not work properly. A warning will be issued if the range is not set explicitly when using the
#'   importing function.
#'
#'   The function only reads data from the latest GridStat version. The latest version allows the separation of the
#'   left and right pole by using on of the following symbols `/:-` (hyphen, colon and dash). Older versions may not
#'   separate the left and right pole. This will cause all labels to be assigned to the left pole only when importing.
#'   You may fix this by simply entering one of the construct separator symbols into the GridStat file between each
#'   left and right construct pole.
#'
#'   The third line of a GridStat file may contain a no labels statement (i.e. a line containing any string of 'NOLA',
#'   'NO L', 'NoLa', 'No L', 'Nola', 'No l', 'nola' or 'no l'). In this case only ratings are supplied, hence, default
#'   names are assigned to elements and constructs.
#'
#' @export
#' @references  Bell, R. C. (1998)  GRIDSTAT: A program for analyzing the data of a repertory grid. Melbourne: Author.
#'
#' @seealso [importGridcor()], [importGridstat()], [importScivesco()], [importGridsuite()], [importTxt()],
#'   [importExcel()]
#'
#' @examples \dontrun{
#'
#' # supposing that the data file gridstat.dat is in the current working directory
#' file <- "gridstat.dat"
#' rg <- importGridstat(file)
#'
#' # specifying a directory (example)
#' dir <- "/Users/markheckmann/data"
#' rg <- importGridstat(file, dir)
#'
#' # using a full path (example)
#' rg <- importGridstat("/Users/markheckmann/data/gridstat.dat")
#'
#' # setting rating scale range
#' rg <- importGridstat(file, dir, min = 1, max = 6)
#' }
#'
importGridstat <- function(file, dir = NULL, min = NULL, max = NULL) {
  tmp.files <- unlist(lapply(
    as.list(file), # convert multigrid files to single grid files
    multigridFileToSinglegridFiles
  ))
  imps <- lapply(as.list(tmp.files), importGridstatInternal, # make import objects for each .txt file
    dir = dir, min = min, max = max
  )
  rgs <- lapply(imps, convertImportObjectToRepGridObject) # make repgrid object from import object
  if (length(tmp.files) == 1) {
    return(rgs[[1]]) # return a single repgrid opbject if a single file is prompted
  } else {
    return(rgs) # return a list of repgrid objects
  }
}



############################# GRIDCOR #########################################

# gridcor outpout has the following form:
# "As you can see in this sample file, the first line contains the number of constructs (10),
# of elements (13) and the maximum scale range (7), separated by two spaces each. The second
# line is the title of the analysis. Next is the data matrix itself, and the labels: first,
# the element labels, then the labels of the right poles of the constructs, and finally the
# left pole ones."
# as retrieved from http://www.terapiacognitiva.net/record/manualgri/man3.htm on 07/Sep/2010

#   14  15   6
# Heinz Boeker in Scheer & Catina (1996, p.163)
# 122352542622334
# 335545454442236
# 223532323344532
# 431242332333541
# 221244242632231
# 435454544633325
# 232333223233331
# 443432344532435
# 232454132633331
# 555525244165555
# 524353243344441
# 253445341642331
# 446535352522235
# 142452553611211
# self
# mother
# father
# kurt
# karl
# george
# martin
# elizabeth
# therapist
# irene
# childhood self
# self before illness
# self with delusion
# self as dreamer
# ideal self
# balanced
# isolated
# closely integrated
# discursive
# open minded
# dreamy
# practically oriented
# playful
# socially minded
# quarrelsome
# artistic
# scientific
# introvert
# wanderlust
# get along with conflicts
# sociable
# excluded
# passive
# indifferent
# dispassionate
# depressed
# serious
# selfish
# peaceful
# technical
# emotional
# extrovert
# home oriented


#' Internal parser for GRIDCOR data files.
#'
#' Parse the file format that is used by the grid program GRIDCOR (Feixas & Cornejo).
#'
#' @param file	  filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @note          Note that the GRIDCOR data sets the minimum ratings scale range to 1.
#'                The maximum value can differ and is defined in the data file.
#' @references <https://repertorygrid.net/en/>
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#' # supposing that the data file gridcor.dat is in the current directory
#' file <- "gridcor.dat"
#' imp <- importGridcorInternal(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' imp <- importGridcorInternal(file, dir)
#'
#' # using a full path
#' imp <- importGridcorInternal("/Users/markheckmann/data/gridcor.dat")
#' }
#'
importGridcorInternal <- function(file, dir = NULL) {
  if (!is.null(dir)) {
    file <- paste(dir, file, sep = "/", collapse = "")
  }
  l <- list()
  # read meta info
  datainfo <- scan(
    file = file, what = "raw", skip = 1, # Read information line (2nd line)
    nlines = 1, quiet = TRUE
  )
  l$datainfo <- joinString(datainfo) # join single items to long string
  meta <- scan(
    file = file, what = "integer",
    skip = 0, nlines = 1, quiet = TRUE
  ) # Read number of elements and constructs
  l$noConstructs <- as.numeric(meta)[1] # no of constructs
  l$noElements <- as.numeric(meta)[2] # no of elements
  l$minValue <- 1 # minimum value for Likert scale (min is 1)
  l$maxValue <- as.numeric(meta)[3] # maximum value for Likert scale

  # read elements
  l$elements <- list()
  for (i in 1:l$noElements) {
    tmp <- scan(
      file = file, what = "character", skip = 2 + l$noConstructs + (i - 1),
      nlines = 1, quiet = TRUE
    ) # read elements line by line
    l$elements[[i]] <- trimBlanksInString(joinString(tmp))
  }

  # read constructs
  l$constructs <- NA # poles come separately in gridcor files, no need to separate them
  l$emergentPoles <- list()
  for (i in 1:l$noConstructs) {
    tmp <- scan(
      file = file, what = "character", skip = 2 + l$noConstructs + l$noElements + (i - 1),
      nlines = 1, quiet = TRUE
    ) # read construct line by line
    l$emergentPoles[[i]] <- joinString(tmp) # save emergent pole
  }

  l$contrastPoles <- list()
  for (i in 1:l$noConstructs) {
    tmp <- scan(file = file, what = "character", skip = 2 + l$noConstructs + l$noElements +
      l$noConstructs + (i - 1), nlines = 1, quiet = TRUE) # read construct line by line
    l$contrastPoles[[i]] <- joinString(tmp) # save contrast pole
  }

  # read ratings
  l$ratings <- list()
  for (i in 1:l$noConstructs) {
    tmp <- scan(
      file = file, what = "character", # read ratings line by line
      quiet = TRUE, skip = 2 + (i - 1), nlines = 1
    )
    l$ratings[[i]] <- strsplit(tmp, split = "")
  }
  l$ratings <- lapply(l$ratings, function(x) as.numeric(unlist(x)))
  l
}


#' Import GRIDCOR data files.
#'
#' Reads the file format that is used by the grid program
#' GRIDCOR (Feixas & Cornejo, 2002).
#'
#' @param file	  filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @return  a single `repgrid` object in case one file and
#'                a list of `repgrid` objects in case multiple files are imported.
#' @note Note that the GRIDCOR data sets the minimum ratings scale range to 1. The maximum value can differ and is
#'   defined in the data file.
#'
#'   Also note that both Gridcor and Gridstat data files do have the same suffix `.dat`. Make sure not to mix them up.
#' @export
#' @references  Feixas, G., & Cornejo, J. M. (2002). GRIDCOR: Correspondence Analysis
#' for Grid Data (version 4.0). Barcelona: Centro de Terapia Cognitiva.
#' Retrieved from <https://repertorygrid.net/en/>.
#'
#' @seealso [importGridcor()],
#'                [importGridstat()],
#'                [importScivesco()],
#'                [importGridsuite()],
#'                [importTxt()],
#'                [importExcel()]
#'
#' @examples \dontrun{
#'
#' # supposing that the data file gridcor.dat is in the current directory
#' file <- "gridcor.dat"
#' rg <- importGridcor(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' rg <- importGridcor(file, dir)
#'
#' # using a full path
#' rg <- importGridcor("/Users/markheckmann/data/gridcor.dat")
#' }
#'
importGridcor <- function(file, dir = NULL) {
  imps <- lapply(as.list(file), importGridcorInternal, # make import objects for each .txt file
    dir = dir
  )
  rgs <- lapply(imps, convertImportObjectToRepGridObject) # make repgrid object from import object
  if (length(file) == 1) {
    return(rgs[[1]]) # return a single repgrid opbject if a single file is prompted
  } else {
    return(rgs) # return a list of repgrid objects
  }
}




############################# GRIDSUITE #######################################

#
# On www.gridsuite.de there are example files and XSD schemes available.
# the developers propose XML as the standard grid format
# TODO: the element and construct IDs are not used. Thus, if the output should be in different order
# the current mechanism will cause false assignments
#
#' Internal parser for Gridsuite data files
#'
#' @param file	  filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @note          The developers of Gridsuite have proposed to use an XML scheme as
#'                a standard exchange format for repertory grid data (Walter,
#'                Bacher & Fromm, 2004). This approach is also embraced by the
#'                `OpenRepGrid` package.
#'
#' @references <http://www.gridsuite.de/>
#'
#'                Walter, O. B., Bacher, A., & Fromm, M. (2004). A proposal
#'                for a common data exchange format for repertory grid data.
#'                *Journal of Constructivist Psychology, 17*(3), 247.
#'                doi:10.1080/10720530490447167
#' @note          TODO: The element and construct IDs are not used yet. Thus,
#'                if the output should be in different order the current mechanism
#'                will cause false assignments.
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#' # supposing that the data file gridsuite.xml is in the current directory
#' file <- "gridsuite.xml"
#' imp <- importGridsuite(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' imp <- importGridsuite(file, dir)
#'
#' # using a full path
#' imp <- importGridsuite("/Users/markheckmann/data/gridsuite.xml")
#' }
#'
importGridsuiteInternal <- function(file, dir = NULL) {
  if (!is.null(dir)) {
    file <- paste(dir, file, sep = "/", collapse = "")
  }
  l <- list()
  xmlFile <- xmlTreeParse(file) # parse XML file
  root <- xmlRoot(xmlFile) # get root node

  ### header node
  level.1.children <- xmlChildren(root) # get header node
  level.1.header.children <- xmlChildren(level.1.children$header) # get header child nodes
  l$topic <- xmlValue(level.1.header.children$topic) # topic
  l$name <- xmlValue(level.1.header.children$name) # name
  l$interviewer <- xmlValue(level.1.header.children$interviewer) # interviewer
  l$date <- xmlValue(level.1.header.children$date) # date
  l$comment <- xmlValue(level.1.header.children$comment) # comment

  ### elements node
  level.1.elements.children <- xmlChildren(level.1.children$elements) # get elements node
  tmp <- lapply(level.1.elements.children, xmlValue) # get element names (values)
  l$elements <- tmp[names(tmp) %in% "element"]
  l$noElements <- length(l$elements) # number of elements

  ### constructs node
  level.1.constructs.children <- xmlChildren(level.1.children$constructs)
  l$noConstructs <- length(level.1.constructs.children) # number of constructs
  l$emergentPoles <- lapply(
    level.1.constructs.children,
    function(x) xmlValue(xmlChildren(x)[[1]])
  )
  l$contrastPoles <- lapply(
    level.1.constructs.children,
    function(x) xmlValue(xmlChildren(x)[[2]])
  )
  l$constructComments <- lapply(
    level.1.constructs.children,
    function(x) xmlValue(xmlChildren(x)$comment)
  )

  ### ratings
  ratingMeta <- xmlAttrs(level.1.children$ratings)
  l$minValue <- as.numeric(ratingMeta["min"])
  l$maxValue <- as.numeric(ratingMeta["max"])
  l$scaleLevel <- ratingMeta["scale"]

  # get ratings for each e and c
  # ratings are saved constructwise as a vector in a list
  level.1.ratings.children <- xmlChildren(level.1.children$ratings)
  ratings <- matrix(NA, ncol = l$noElements, nrow = l$noConstructs) # matrix to save ratings
  for (i in seq_along(level.1.ratings.children)) {
    x <- level.1.ratings.children[[i]]
    attrs <- xmlAttrs(x)
    col <- as.numeric(attrs["ele_id"])
    row <- as.numeric(attrs["con_id"])
    ratings[row, col] <- as.numeric(xmlValue(x))
  }
  l$ratings <- split(ratings, row(ratings)) # convert to list
  l
}
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridsuite.xml"
# file <- "/Users/unimitarbeiter/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridsuite.xml"

# suite <- importGridsuite(file)


#' Import Gridsuite data files.
#'
#' @param file	  Filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    Alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @return  A single `repgrid` object in case one file and
#'                a list of `repgrid` objects in case multiple files are imported.
#' @note The developers of Gridsuite have proposed to use an XML scheme as a standard exchange format for repertory
#'   grid data (Walter, Bacher & Fromm, 2004).
#'
#' @references <http://www.gridsuite.de/>
#'
#' Walter, O. B., Bacher, A., & Fromm, M. (2004). A proposal  for a common data exchange format for repertory grid
#' data.*Journal of Constructivist Psychology, 17*(3), 247. \doi{doi:10.1080/10720530490447167}
#'
#' @note  TODO: The element and construct IDs are not used yet. Thus, if the output should be in different order the
#'   current mechanism will cause false assignments.
#'
#' @export
#' @seealso [importGridcor()], [importGridstat()], [importScivesco()], [importGridsuite()], [importTxt()],
#'   [importExcel()]
#'
#' @examples \dontrun{
#'
#' # supposing that the data file gridsuite.xml is in the current directory
#' file <- "gridsuite.xml"
#' rg <- importGridsuite(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' rg <- importGridsuite(file, dir)
#'
#' # using a full path
#' rg <- importGridsuite("/Users/markheckmann/data/gridsuite.xml")
#' }
#'
importGridsuite <- function(file, dir = NULL) {
  imps <- lapply(as.list(file), importGridsuiteInternal, # make import objects for each .txt file
    dir = dir
  )
  rgs <- lapply(imps, convertImportObjectToRepGridObject) # make repgrid object from import object
  if (length(file) == 1) {
    return(rgs[[1]]) # return a single repgrid opbject if a single file is prompted
  } else {
    return(rgs) # return a list of repgrid objects
  }
}


############################# sci:vesco #######################################

# scivesco saves single grids in .scires files which have an XML structure.
# Note: not all nodes are imported by importScivesco()
# Overview of file structure:

# <Interview>
# 	<Expert>
# 	<Interviewer>
# 	<Notes>
# 	<InterviewSettingData>
# 		<Description>
# 		<Language>
# 		<ReceiveConstructType>
# 		<RatingType>
# 		<Elements>
# 			<ElementsCollection>
# 				<Element>
# 				<Element>
# 				<...>
# 	<DoneCombinations>
# 	... TODO
# 	<InterviewAdditionalQuestionAnswerResults>
# 	... TODO
# 	<InterviewResultTurnsCollection>
# 		<InterviewResultTurn>
# 			<UsedElementCombination>
# 				<ElementCombination>
# 					<Element>
# 					<Element>
# 					<Element>          # max three elements in case of triad method (also allows monadic and dyadic)
# 			<Pole1>
# 		 	<Pole2>
# 		 	<RatingPole1>				# note that sci:vesco uses decoupled ratings for the poles
# 				<Rating>
# 				<Rating>
# 				...
# 			<RatingPole2>				# due to Tetralemma field rating type
# 				<Rating>
# 				<Rating>
# 				...


# file <- "/Users/markheckmann/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/data/scivesco/20100625_170217_01_MH.scires"
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/scivesco.scires"
# file <- "/Users/unimitarbeiter/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/scivesco.scires"
# a <- importScivesco(file)

#' Internal parser for sci:vesco files (suffix `scires`).
#'
#' @param file	  filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @return  a list with extracted parameters.
#'
#' @note          Sci:Vesco offers the options to rate the construct poles separately or using
#'                a bipolar scale. The separated rating is done using the "tetralemma" field.
#'                The field is a bivariate plane on which each of the four (tetra) corners
#'                has a different meaning in terms of rating. Using this approach also allows ratings
#'                like: "both poles apply", "none of the poles apply" and all intermediate ratings
#'                can be chosen. This relaxes the bipolarity assumption often assumed in grid theory and
#'                allows for deviation from a strict bipolar rating if the constructs are not applied
#'                in a bipolar way. Using the tetralemma field for rating requires to analyze
#'                each construct separately though. This means we get a double entry grid where the
#'                emergent and contrast pole ratings might not simply be a reflection of on another.
#'                If a tetralemma field has been used for rating, `OpenRepGrid` offers the option
#'                to transform the scores into "normal" grid ratings (i.e. restricted to bipolarity)
#'                by projecting the ratings from the bivariate tetralemma field onto the diagonal
#'                of the tetralemma field and thus forcing a bipolar rating type. This option is
#'                not recommended due to the fact that the conversion is susceptible to error
#'                when both ratings are near to zero.
#' @note          TODO: The element IDs are not used yet. This might cause wrong assignments.
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#' # supposing that the data file scivesco.scires is in the current directory
#' file <- "scivesco.scires"
#' imp <- importScivescoInternal(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' imp <- importScivescoInternal(file, dir)
#'
#' # using a full path
#' imp <- importScivescoInternal("/Users/markheckmann/data/scivesco.scires")
#' }
#'
importScivescoInternal <- function(file, dir = NULL) {
  if (!is.null(dir)) {
    file <- paste(dir, file, sep = "/", collapse = "")
  }
  xmlFile <- xmlTreeParse(file) # parse XML file
  root <- xmlRoot(xmlFile) # get root node

  l <- list()

  ### interview node
  level.1.children <- xmlChildren(root) # get interview top node
  level.1.children$Interview
  l$id <- xmlAttrs(level.1.children$Interview)["Id"] # interview ID
  l$date <- xmlAttrs(level.1.children$Interview)["StartDateTime"] # interview time

  node.2.interview <- xmlChildren(level.1.children$Interview)
  l$interviewee <- xmlValue(node.2.interview$Expert) # name of expert/interviewee
  l$interviewer <- xmlValue(node.2.interview$Interviewer) # name of interviewer
  l$interviewNotes <- xmlValue(node.2.interview$Notes) # interview notes

  # InterviewSettingData node
  level.3.InterviewSettingData <-
    xmlChildren(node.2.interview$InterviewSettingData)
  l$interviewDescription <- xmlValue(level.3.InterviewSettingData$Description)
  l$ratingType <- xmlAttrs(level.3.InterviewSettingData$RatingType)["Default"]

  level.4.Elements <- xmlChildren(level.3.InterviewSettingData$Elements) # get Elements child nodes
  l$noElements <-
    as.numeric(xmlAttrs(level.4.Elements$ElementsCollection)["Count"]) # get number of elements
  level.5.Elements <- xmlChildren(level.4.Elements$ElementsCollection) # get number of elements

  getElementIdAndName <- function(x) {
    id <- xmlAttrs(x)["Id"]
    children <- xmlChildren(x)
    name <- xmlValue(children$Name)
    c(id, name = name)
  }
  l$elements <- lapply(level.5.Elements, getElementIdAndName)

  # InterviewResultTurnsCollection
  level.3.InterviewResultTurnsCollection <-
    xmlChildren(node.2.interview$InterviewResultTurnsCollection)
  getEmergentPole <- function(x) {
    xmlValue(xmlChildren(x)$Pole1)
  }
  getContrastPole <- function(x) {
    xmlValue(xmlChildren(x)$Pole2)
  }
  l$emergentPoles <- lapply(level.3.InterviewResultTurnsCollection, getEmergentPole)
  l$contrastPoles <- lapply(level.3.InterviewResultTurnsCollection, getContrastPole)
  names(l$emergentPoles) <- rep("emergentPole", length(l$emergentPoles)) # cosmetic surgery to get nicer list names
  names(l$contrastPoles) <- rep("contrastPole", length(l$contrastPoles)) # cosmetic surgery to get nicer list names

  # get ratings for both poles
  # Here, for each construct/comparison two separate ratings are collected
  # resulting in two ratings list(ratings1 and ratings2).

  l$ratingsEmergent <- NA
  l$ratingsContrast <- NA

  # The InterviewResultsReturnCollection has several InterviewResultTurn as
  # children. Each of these contains the ratings for one construct pair.
  no.turns <- length(level.3.InterviewResultTurnsCollection) # no of construct pairs

  # get ratings for pole1 and pole 2 from each construct pair. And return a
  # vector of ratings.
  #
  # x       a InterviewResultTurn
  # digits  number of digits to be rounded to
  getRatingsFromTurn <- function(x, pole = 1, digits = 2) {
    if (pole == 1) {
      selectPole <- "RatingPole1"
    } else {
      selectPole <- "RatingPole2"
    }
    ratingsList <- xmlChildren(xmlChildren(x)[[selectPole]])
    ratings <- lapply(ratingsList, xmlAttrs)

    ## old plyr code removed due to ::: in v0.1.9
    # df.ratings <- plyr:::list_to_dataframe(ratings)[-1]
    # round(as.numeric(df.ratings$Value), digits)

    # new code
    df.ratings.2 <- list_to_dataframe(ratings)[-1]
    names(df.ratings.2) <- "Value"
    round(as.numeric(as.character(df.ratings.2$Value)), digits)
  }
  l$ratings <- NA
  l$ratings1 <- lapply(level.3.InterviewResultTurnsCollection,
    getRatingsFromTurn,
    pole = 1, digits = 2
  )
  l$ratings2 <- lapply(level.3.InterviewResultTurnsCollection,
    getRatingsFromTurn,
    pole = 2, digits = 2
  )

  # project tetralemma ratings onto bipolar diagonal. Simply preserving
  # the relation bewteen the ratings linearly projected onto the diagonal
  # TODO: when tow values are near zero small deviations make big differences
  # maybe it is better to not to biploar reduction but only except grids
  # that have been elicited in a bipolar way!
  projectToDiagonal <- function(pole1, pole2, res = 1, SIMPLIFY = F) {
    rating.sum <- pole1 + pole2
    if (res == 1) {
      list(pole1 / rating.sum)
    } else {
      list(pole2 / rating.sum)
    }
  }
  l$ratings <- mapply(projectToDiagonal, l$ratings1, l$ratings2)

  # overwrite projection as not yet in good shape
  l$ratings <- l$ratings2

  # getElementNameFromId <- function(id, l){
  #   x["ElementId"]
  # }
  # lapply(getElementNameFromId)

  # TODO. what happens if an element is not rated? What values does it take?
  l
}


#' Convert the returned object from the sci:vesco import function into a `repgrid`
#' object.
#'
#' @param import List with relevant entries.
#' @keywords internal
#' @export
convertScivescoImportObjectToRepGridObject <- function(import) {
  # structure of import object:
  # List of 16
  #  $ id                  : Named chr "dbd819e8-e8db-4acd-8fe2-355be1744929"
  #   ..- attr(*, "names")= chr "Id"
  #  $ date                : Named chr "Tue, 07 Sep 2010 17:02:59 GMT"
  #   ..- attr(*, "names")= chr "StartDateTime"
  #  $ interviewee         : chr "name interviewee"
  #  $ interviewer         : chr "name interviewer"
  #  $ interviewNotes      : chr "interview notes"
  #  $ interviewDescription: chr "Description of interview"
  #  $ ratingType          : Named chr "Tetralemma"
  #   ..- attr(*, "names")= chr "Default"
  #  $ noElements          : num 5
  #  $ elements            :List of 5
  #  $ emergentPoles       :List of 5
  #  $ contrastPoles       :List of 5
  #  $ ratingsEmergent     : logi NA
  #  $ ratingsContrast     : logi NA
  #  $ ratings1            :List of 5
  #  $ ratings2            :List of 5
  #  $ ratings             :List of 5

  # difference between tetralemma and bipolar rating grid
  if (import$ratingType == "Tetralemma") { # Tetralemma
    stop(
      "Tetralemma field ratings are not yet supported.\n",
      "Currently only bipolar rating scales are supported."
    )
  } else { # bipolar Rating scales
    element.names <- lapply(import$elements, function(x) x[2]) # get Element names
    ratings <- round(t(sapply(import$ratings, I)), 1) * 10
    # transform into by-row input for makeRepgrid
    # browser()
    args <- list(
      name = unlist(element.names), # elements
      l.name = unlist(import$emergentPoles), # left poles
      r.name = unlist(import$contrastPoles), # right poles
      scores = as.vector(t(ratings))
    ) # ratings ... or t(ratings) ???
    # When sourced t is wrong, when build t is needed WTF???
    x <- makeRepgrid(args) # make repgrid
    x <- setScale(x, 0, 1 * 10) # set scale range
    x
  }
  x
}


#' Import sci:vesco data files.
#'
#' @param file	  Filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is .dat.
#' @param dir	    Alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @return  A single `repgrid` object in case one file and
#'                a list of `repgrid` objects in case multiple files are imported.
#' @note  Sci:Vesco offers the options to rate the construct poles separately or using a bipolar scale. The separated
#'   rating is done using the "tetralemma" field. The field is a bivariate plane on which each of the four (tetra)
#'   corners has a different meaning in terms of rating. Using this approach also allows ratings like: "both poles
#'   apply", "none of the poles apply" and all intermediate ratings can be chosen. This relaxes the bipolarity
#'   assumption often assumed in grid theory and allows for deviation from a strict bipolar rating if the constructs
#'   are not applied in a bipolar way. Using the tetralemma field for rating requires to analyze each construct
#'   separately though. This means we get a double entry grid where the emergent and contrast pole ratings might not
#'   simply be a reflection of on another. The tetralemma field is not yet supported and importing will fail. Currently
#'   only bipolar ratings are supported.
#'
#'   If a tetralemma field has been used for rating, `OpenRepGrid` will offer the option to transform the scores into
#'   "normal" grid ratings (i.e. restricted to bipolarity) by projecting the ratings from the bivariate tetralemma
#'   field onto the diagonal of the tetralemma field and thus forcing a bipolar rating type. This option is not
#'   recommended due to the fact that the conversion is susceptible to error when both ratings are near to zero.
#'
#' @note  TODO: For developers: The element IDs are not used yet. This might cause wrong assignments.
#'
#' @export
#' @references    Menzel, F., Rosenberger, M., Buve, J. (2007). Emotionale, intuitive und
#'                rationale Konstrukte verstehen. *Personalfuehrung, 4*(7), 91-99.
#'
#' @seealso [importGridcor()], [importGridstat()], [importScivesco()], [importGridsuite()], [importTxt()],
#'   [importExcel()]
#'
#' @examples \dontrun{
#'
#' # supposing that the data file scivesco.scires is in the current directory
#' file <- "scivesco.scires"
#' rg <- importScivesco(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' rg <- importScivesco(file, dir)
#'
#' # using a full path
#' rg <- importScivesco("/Users/markheckmann/data/scivesco.scires")
#' }
importScivesco <- function(file, dir = NULL) {
  imps <- lapply(as.list(file), importScivescoInternal, # make import objects for each .txt file
    dir = dir
  )
  rgs <- lapply(imps, convertScivescoImportObjectToRepGridObject) # make repgrid object from import object

  if (length(file) == 1) {
    return(rgs[[1]]) # return a single repgrid opbject if a single file is prompted
  } else {
    return(rgs) # return a list of repgrid objects
  }
}


############################# IMPORT .TXT #######################################

#' ImportTxtInternal is the parser for importTxt.
#'
#' ImportTxtInternal is the parser for importTxt that constructs an import object. The `.txt` file has to be in a fixed
#' format. There are three mandatory blocks each starting and ending with a predefined tag in uppercase letters. The
#' first block starts with `ELEMENTS` and ends with `END ELEMENTS` and contains one element in each line. The other
#' mandatory blocks contain the constructs and ratings (see below). In the block containing the constructs the left and
#' right pole are separated by a colon (:). To define missing values use `NA` like in the example below. One optional
#' block contains the range of the rating scale used defined by two numbers. The order of the blocks is arbitrary. All
#' text not contained within the blocks is discarded and can thus be used for comments.
#'
#' ```
#' ---------------- sample .txt file -------------------
#'
#'     Note: anything outside the tag pairs is discarded
#'
#' ELEMENTS
#' element 1
#' element 2
#' element 3
#' END ELEMENTS
#'
#' CONSTRUCTS
#' left pole 1 : right pole 1
#' left pole 2 : right pole 2
#' left pole 3 : right pole 3
#' left pole 4 : right pole 4
#' END CONSTRUCTS
#'
#' RATINGS
#' 1 3 2
#' 4 1 1
#' 1 4 4
#' 3 1 1
#' END RATINGS
#'
#' RANGE
#' 1 4
#' END RANGE
#'
#' ------------------ end of file ------------------
#' ```
#'
#' Note that the maximum and minimum value has to be defined using the `min` and
#' `max` arguments if no `RANGE` block is contained in the data file.
#' Otherwise the scaling range is inferred from the available data and a warning
#' is issued as the range may be erroneous. This may effect other functions that
#' depend on knowing the correct range and it is thus strongly recommended to
#' set the scale range correctly.
#'
#' Question marks (?) in the ratings are treated as missing data.
#'
#' @param file	  Filename including path if file is not in current working
#'                directory. File can also be a complete URL. The fileformat
#'                is `.txt`.
#' @param dir	    Alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @param min	    Optional argument (`numeric`, default `NULL`)
#'                for minimum rating value in grid.
#' @param max	    Optional argument (`numeric`, default `NULL`)
#'                for maximum rating value in grid.
#' @return  List of relevant data.
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#' # supposing that the data file sample.txt is in the current directory
#' file <- "sample.txt"
#' imp <- importTxtInternal(file)
#'
#' # specifying a directory (arbitrary example directory)
#' dir <- "/Users/markheckmann/data"
#' imp <- importTxtInternal(file, dir)
#'
#' # using a full path
#' imp <- importTxtInternal("/Users/markheckmann/data/sample.txt")
#' }
#'
importTxtInternal <- function(file, dir = NULL, min = NULL, max = NULL) {
  if (!is.null(dir)) {
    file <- paste(dir, file, sep = "/", collapse = "")
  }

  data <- readLines(file) # read txt file line by line
  data <- gsub("\t", " ", data) # replace tabulators by simple blank
  data <- data[str_trim(data) != ""] # remove all empty lines

  d <- str_trim(data) # remove blanks for better matching
  line.elements <- which(d == "ELEMENTS")
  line.elements.end <- which(d == "END ELEMENTS")
  line.constructs <- which(d == "CONSTRUCTS")
  line.constructs.end <- which(d == "END CONSTRUCTS")
  line.ratings <- which(d == "RATINGS")
  line.ratings.end <- which(d == "END RATINGS")
  line.range <- which(d == "RANGE")
  line.bipolar.implications <- which(d == "BIPOLAR IMPLICATIONS")
  line.bipolar.implications.end <- which(d == "END BIPOLAR IMPLICATIONS")

  l <- list()

  # read elements and trim blanks
  l$elements <- as.list(data[(line.elements + 1):(line.elements.end - 1)])
  l$elements <- lapply(l$elements, function(x) trimBlanksInString(x))

  # read constructs and trim blanks
  l$constructs <- as.list(data[(line.constructs + 1):(line.constructs.end - 1)])
  l$constructs <- lapply(l$constructs, function(x) trimBlanksInString(x))
  tmp <- lapply(l$constructs, function(x) {
    strsplit(x, ":")[[1]] # separate emergent and contrast pole by splitting at hyphen, colon or slash (see email from Richard)
  })
  l$emergentPoles <- lapply(tmp, function(x) trimBlanksInString(x[1]))
  l$contrastPoles <- lapply(tmp, function(x) trimBlanksInString(x[2]))

  # read ratings and convert to numeric
  op <- options()$warn
  options(warn = -1)
  l$ratings <- as.list(data[(line.ratings + 1):(line.ratings.end - 1)])
  l$ratings <- lapply(l$ratings, function(x) {
    tmp <- trimBlanksInString(x) # trim blanks at beginning and end of string
    tmp <- strsplit(tmp, "[ \t]+")[[1]] # split at one or more tabs or blanks
    tmp <- gsub("[?]", "NA", tmp) # replace missing indicator (?) by "NA"
    as.numeric(tmp)
  })
  options(warn = op)

  # read range if available
  if (!identical(line.range, integer(0))) {
    d <- data[line.range + 1] # get line with scale range data
    tmp <- trimBlanksInString(d) # trim blanks at beginning and end of string
    tmp <- strsplit(tmp, "[ \t]+")[[1]] # split at one or more tabs or blanks
    range <- as.numeric(tmp)
  } else {
    range <- c(
      min(unlist(l$ratings), na.rm = T),
      max(unlist(l$ratings), na.rm = T)
    )
    if (is.null(min) | is.null(max)) {
      warning("the minimum and/or the maximum value of the rating scale have not been set explicitly.",
        "The scale range was thus inferred by scanning the available ratings and may be wrong.",
        "See ?importTxt for more information",
        call. = FALSE
      )
    }
  }

  l$noConstructs <- length(l$constructs) # no of constructs
  l$noElements <- length(l$elements) # no of elements
  l$minValue <- range[1] # minimum value for Likert scale
  l$maxValue <- range[2] # maximum value for Likert scale

  if (is.null(min)) {
    l$minValue <- range[1]
  } else {
    l$minValue <- min
  }

  if (is.null(max)) {
    l$maxValue <- range[2]
  } else {
    l$maxValue <- max
  }

  # read bipolar implications if available
  if (!identical(line.bipolar.implications, integer(0)) &
    !identical(line.bipolar.implications.end, integer(0))) {
    l$bipolar.implications <- as.list(data[(line.bipolar.implications + 1):(line.bipolar.implications.end - 1)])
    l$bipolar.implications <- lapply(l$bipolar.implications, function(x) trimBlanksInString(x))
  }
  l
}


#' Import grid data from a text file.
#'
#' You can define a grid using a standard text editor and saving it as a `.txt` file.
#' The *Details* section describes the required format of the `.txt` file. However, you may also
#' consider using the Excel format instead, as it has a more intuitive format (see [importExcel()]).
#'
#' @param file	  A vector of filenames including the full path if file is not in current working
#'                directory. File can also be a complete URL. The file suffix
#'                has to be `.txt`.
#' @param dir	    Alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @param min	    Optional argument (`numeric`, default `NULL`)
#'                for minimum rating value in grid.
#' @param max	    Optional argument (`numeric`, default `NULL`)
#'                for maximum rating value in grid.
#' @return  A single `repgrid` object in case one file and
#'                a list of `repgrid` objects in case multiple files are imported.
#'
#' @details
#' The `.txt` file has to be in a fixed format. There are *three mandatory blocks* each starting and ending
#' with a predefined tag in uppercase letters. The first block starts with `ELEMENTS` and ends with `END ELEMENTS` and
#' contains one element in each line. The other mandatory blocks contain the constructs and ratings (see below). In the
#' block containing the constructs the left and right pole are separated by a colon (:). To define missing values use
#' `NA` like in the example below. One optional block contains the range of the rating scale used defined by two
#' numbers. The order of the blocks is arbitrary. All text not contained within the blocks is discarded and can thus be
#' used for comments.
#'
#' The content of a sample `.txt` file is shown below. The package also contains a sample file (see *Examples*).
#'
#' ```
#' ---------------- sample .txt file -------------------
#'
#'     Note: anything outside the tag pairs is discarded
#'
#' ELEMENTS
#' element 1
#' element 2
#' element 3
#' END ELEMENTS
#'
#' CONSTRUCTS
#' left pole 1 : right pole 1
#' left pole 2 : right pole 2
#' left pole 3 : right pole 3
#' left pole 4 : right pole 4
#' END CONSTRUCTS
#'
#' RATINGS
#' 1 3 2
#' 4 1 1
#' 1 4 4
#' 3 1 1
#' END RATINGS
#'
#' RANGE
#' 1 4
#' END RANGE
#'
#' ------------------ end of file ------------------
#' ```
#'
#' Note that the maximum and minimum value has to be defined using the `min` and `max` arguments if no `RANGE` block is
#' contained in the data file. Otherwise the scaling range is inferred from the available data and a warning is issued
#' as the range may be erroneous. This may effect other functions that depend on knowing the correct range and it is
#' thus strongly recommended to set the scale range correctly.
#'
#' @export
#' @seealso [importGridcor()], [importGridstat()], [importScivesco()], [importGridsuite()], [importTxt()],
#'   [importExcel()]
#' @examples
#' # Import a .txt file delivered along with the package
#' file <- system.file("extdata", "grid_01.txt", package = "OpenRepGrid")
#' rg <- importTxt(file)
#'
#' \dontrun{
#' # To see the structure of the Excel file try to open it as follows.
#' # May not work on all systems.
#' file.show(file)
#' }
#'
#' # Import more than one .txt file
#' files <- system.file("extdata", c("grid_01.txt", "grid_02.txt"), package = "OpenRepGrid")
#' rgs <- importTxt(files)
importTxt <- function(file, dir = NULL, min = NULL, max = NULL) {
  imps <- lapply(as.list(file), importTxtInternal, # make import objects for each .txt file
    dir = dir, min = min, max = max
  )
  rgs <- lapply(imps, convertImportObjectToRepGridObject) # make repgrid object from import object
  if (length(file) == 1) {
    return(rgs[[1]]) # return a single repgrid opbject if a single file is prompted
  } else {
    return(rgs) # return a list of repgrid objects
  }
}


############################# IMPORT EXCEL ####################################


#' workhorse function (parser) for importExcel.
#'
#' @inheritParams importExcel
#' @export
#' @keywords      internal
importExcelInternal <- function(file, dir = NULL, sheetIndex = 1,
                                min = NULL, max = NULL) {
  if (!is.null(dir)) {
    file <- paste(dir, file, sep = "/", collapse = "")
  }

  # read in Excel file
  x <- openxlsx::read.xlsx(file, sheet = sheetIndex, colNames = F) # read .xlxs or .xls file

  # remove NA lines when too many rows in Excel
  na.rows <- apply(x, 1, function(x) all(is.na(unlist(x))))
  x <- x[!na.rows, ]

  nc <- nrow(x) - 1 # number of constructs
  ne <- ncol(x) - 2 # number of elements

  l <- list()

  # read elements
  l$elements <- as.list(as.character((unlist(x[1, 2:(1 + ne)])))) # list of element names

  # read constructs and trim blanks
  l$emergentPoles <- as.list(as.character(x[2:(nc + 1), 1]))
  l$contrastPoles <- as.list(as.character(x[2:(nc + 1), ne + 2]))

  # read ratings and convert to numeric
  ratings <- x[-1, c(-1, -(ne + 2))]
  ratings <- sapply(ratings, function(x) as.numeric(as.character(x))) # convert to numerics
  l$ratings <- split(ratings, 1:nrow(ratings)) # convert df to list row-wise
  # names(l$ratings) <- NULL

  # read range info if available
  rmin <- as.numeric(as.vector(x[1, 1]))
  rmax <- as.numeric(as.vector(x[1, ne + 2]))

  # if not availabe infer range data and issue warning
  if (identical(rmin, numeric(0)) | identical(rmax, numeric(0))) {
    warning("the minimum and/or the maximum value of the rating scale have not been set explicitly.",
      "The scale range was thus inferred by scanning the available ratings and may be wrong.",
      "See ?importExcel for more information",
      call. = FALSE
    )
    rmin <- min(ratings, na.rm = TRUE) # infer rating range
    rmax <- max(ratings, na.rm = TRUE)
  }

  # overwrite scale range if given in arguments
  if (!is.null(min)) {
    rmin <- min
  }
  if (!is.null(max)) {
    rmax <- max
  }

  l$noConstructs <- nc # no of constructs
  l$noElements <- ne # no of elements
  l$minValue <- rmin # minimum value for Likert scale
  l$maxValue <- rmax # maximum value for Likert scale
  l
}


#' Import grid data from an Excel file.
#'
#' You can define a grid using Microsoft Excel and by saving it as a
#' `.xlsx` file. The `.xlsx` file has to be in a specified fixed
#' format (see section Details).
#'
#' Excel file structure: The first row contains the minimum of the rating scale,
#' the names of the elements and the maximum of the rating scale. Below every
#' row contains the left construct pole, the ratings and the right construct
#' pole.
#'
#' \tabular{lccccr}{
#' `1`           \tab `E1`  \tab `E2` \tab `E3` \tab `E4`  \tab `5`        \cr
#' `left pole 1` \tab `1`   \tab `5`  \tab `3`  \tab `4`   \tab `right pole 1` \cr
#' `left pole 2` \tab `3`   \tab `1`  \tab `1`  \tab `3`   \tab `right pole 2` \cr
#' `left pole 3` \tab `4`   \tab `2`  \tab `5`  \tab `1`   \tab `right pole 3` \cr
#' }
#'
#' Note that the maximum and minimum value has to be defined using the
#' `min` and `max` arguments if no values are supplied at the
#' beginning and end of the first row. Otherwise the scaling range is inferred
#' from the available data and a warning is issued as the range may be
#' erroneous. This may effect other functions that depend on knowing the correct
#' range and it is thus strongly recommended to set the scale range correctly.
#'
#' @param file    A vector of filenames including the full path if file is not in current working
#'                directory. The file suffix has to be `.xlsx` (used since Excel 2007).
#' @param dir	    Alternative way to supply the directory where the file is located
#'                (default `NULL`).
#' @param sheetIndex  The number of the Excel sheet that contains the grid data.
#' @param min	    Optional argument (`numeric`, default `NULL`)
#'                for minimum rating value in grid.
#' @param max	    Optional argument (`numeric`, default `NULL`)
#'                for maximum rating value in grid.
#' @return  A single `repgrid` object in case one file and
#'                a list of `repgrid` objects in case multiple files are imported.
#' @export
#' @seealso [importGridcor()],
#'                [importGridstat()],
#'                [importScivesco()],
#'                [importGridsuite()],
#'                [importTxt()]
#'
#' @examples \dontrun{
#'
#' # Open Excel file delivered along with the package
#' file <- system.file("extdata", "grid_01.xlsx", package = "OpenRepGrid")
#' rg <- importExcel(file)
#'
#' # To see the structure of the Excel file try to open it as follows.
#' # Requires Excel to be installed.
#' system2("open", file)
#'
#' # Import more than one Excel file
#' files <- system.file("extdata", c("grid_01.xlsx", "grid_02.xlsx"), package = "OpenRepGrid")
#' rg <- importExcel(files)
#' }
#'
importExcel <- function(file, dir = NULL, sheetIndex = 1, min = NULL, max = NULL) {
  imps <- lapply(as.list(file), importExcelInternal, # make import objects for each .txt file
    dir = dir, sheet = sheetIndex,
    min = min, max = max
  )
  rgs <- lapply(imps, convertImportObjectToRepGridObject) # make repgrid object from import object
  if (length(file) == 1) {
    return(rgs[[1]]) # return a single repgrid opbject if a single file is prompted
  } else {
    return(rgs) # return a list of repgrid objects
  }
}



############################# IMPORTING ###################################


guessDataSource <- function(file) {
  ending <- tail(strsplit(file, "\\.")[[1]], n = 1) # look at post fix
  if (ending == "scires") {
    cat("Trying to load Sci:Vesco file: ", file)
    importScivesco(file)
  } else if (ending == "xml") {
    cat("Trying to load Gridsuite file: ", file)
    importGridsuite(file)
  } else if (ending == "dat") {
    cat("Trying to load gridstat file: ", file)
    importGridstat(file)
  } else {
    stop("Please specify format by hand as", ending, "id not known")
  }
}

# d <- guessDataSource(loadgrid())
