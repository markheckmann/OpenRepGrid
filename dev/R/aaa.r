###############################################################################

rm(list=ls(all=T))  # delete for package but keep first to discover errors
home <- T
if (home)
  setwd("/Users/markheckmann/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/scripts/") else
  setwd("/Users/unimitarbeiter/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/scripts/")


# copy R files into subdirectory R in package directory
library(MHmisc)
library(tools)
library(roxygen)
options(width=66)   # used in usage section to determine line breaks 76 is exact

# be careful here not to delete the wrong thing
delete <- dir("../OpenRepGrid")        # delete all files in OpenRepGrid
unlink(paste("../OpenRepGrid", delete, sep="/"), recursive = TRUE)
Sys.sleep(2)    # wait until everything is deleted

copyMetafiles(to="../OpenRepGrid") 
files <- c(
  "openrepgrid.r", 
  "utils.r",
  "utils-import.r", 
  "gmMain.r", 
  "repgrid.r", 
  "repgrid-elements.r",
  "repgrid-constructs.r",
  "repgrid-ratings.r",
  "repgrid-basicops.r",
  "bertin.r",
  "repgrid-output.r",
  "calc.r",
  "rgl-3d.r",
  "repgrid-plots.r",
  "data-openrepgrid.r",
  "import.r",
  "export.r", 
  "measures.r",
  "dev-functions.r",
  "zzz.R")     
copyRfiles(files, to="../OpenRepGrid")

### Run roxygen ####
library(roxygen) 
roxygenize(	package.dir="../OpenRepGrid",             # `R CMD roxygen -d helloRoxygen' works, too.
			      roxygen.dir='../OpenRepGrid',
			      copy.package=FALSE, 
			      unlink.target=TRUE, 
			      use.Rd2=TRUE)

### citation file
copyMetafiles("CITATION", to="../OpenRepGrid/inst")   # CITATION file in inst folder
file.remove("../OpenRepGrid/inst/doc")                # remove inst/doc if no vignette available

### data files ###
dir.create("../OpenRepGrid/data")
datafiles <- c("boeker.RData", "raeithel.RData",
               "slater1977b.RData",
               "bell2010.RData", "slater1977a.RData",
               "bellmcgorry1992.RData", "mackay1992.RData",
               "fbb2003.RData", "feixas2004.RData",
               "leach2001a.RData", "leach2001b.RData")
copyMetafiles(paste("../data/", datafiles, sep=""), to="../OpenRepGrid/data")

### demo files ###
demofiles <- c("00Index", "OpenRepGrid.r")
dir.create("../OpenRepGrid/demo")
copyMetafiles(paste("../demo/", demofiles, sep=""), to="../OpenRepGrid/demo")

# TODO after building: Nothing







