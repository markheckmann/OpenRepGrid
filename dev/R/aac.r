# source all files
rm(list=ls(all=T))
home <- T
if (home)
  setwd("/Users/markheckmann/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/scripts/") else
  setwd("/Users/unimitarbeiter/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/scripts/")

files <- c(
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
  "import.r",
  "export.r",
  "measures.r",
  "dev-functions.r")
  
for (file in files)
  source(file)