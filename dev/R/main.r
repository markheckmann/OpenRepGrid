rm(list=ls(all=T))

dirProject <- "Z:/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic"
#dirProject <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic"
dirOutput <- paste(dirProject, "/output", sep="")
dirScripts <- paste(dirProject, "/scripts", sep="")
dirData <- paste(dirProject, "/data", sep="")

library(plyr)
library(grid)
library(colorspace)
library(rgl)

setwd(dirProject)
source("scripts/gmMain.r")
source("scripts/misc_functions.r")
source("scripts/def_repgrid_object_s4.r")
source("scripts/methods_calc.r")
source("scripts/package_data.r")

