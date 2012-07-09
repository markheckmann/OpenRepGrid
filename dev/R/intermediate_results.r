###############################################################################
# Presentation of intermediate results 20110215
# features to present uses v0.0.3

#load code before presentation as not all functions are yet included 
# into openrepgrid. Mock-up.
setwd("/Users/markheckmann/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/scripts/")

library(OpenRepGrid)

# import functions (Gridstat, Gridcor, Gridsuite, Sci:vesco)
# still buggy and not thoroughly untested
importGridstat()
importScivesco()
importTxt()

# basic stats
boeker
x <- boeker
x[1:5, 1:6]
constructCor(x)
constructCor(x, trim=NA, index=T)
constructRmsCor(x)

elementCor(x, rc=F)
elementCor(x, rc=T)

statsConstructs(x)
statsElements(x)

constructPca(x, nfactors=1, rotate="none")

# cluster
cluster(x)
cluster(x, dm="manhattan", cm="single")
cluster(x, 1)
bertinCluster(boeker)

# random grids
randomGrid()
r <- randomGrid(40, 70, range=c(0,1))
bertin(r)
bertinCluster(r, showvalues=FALSE)
bertinCluster(r, show=FALSE, border="lightgrey", colors=c("black", "white"))

# biplots
biplot2d(boeker)
biplotPseudo3d(boeker)
biplot(boeker, flip=c(T,T))
biplot2d(boeker, e.color="darkblue", c.color="darkgreen")
biplot2d(bell2010, e.points=c(1,3,6))
biplot2d(bell2010, e.points=c(1,3,6), c.labels.dev=15)

biplot2d(raeithel, c.labels.inside=T, mai=c(0,0,0,0))

# 3d Visualization
draw3d(boeker)

############################################################################### 