#///////////////////////////////////////////////////////
#
#           01 Create figures used in article 
#
#///////////////////////////////////////////////////////


library(OpenRepGrid)
library(pvclust)

x <- boeker    # dataset contained in package, see ?boeker
  
# 01 Clustered Bertin  -----------

png("img/01-bertin-clustered.png", width = 20, height = 15, res = 300, units = "cm")
bertinCluster(x, colors = c("white", "darkred"))
dev.off()


# 02 Biplot ----------------------

png("img/02-biplot.png", width = 20, height = 15, res = 300, units = "cm")
biplot2d(x, mai = c(0.2, 2, 0.2, 2))
dev.off()


# 03 Cluster Bootstrap ----------------------

s <- clusterBoot(boeker, along = 2, seed = 123)
png("img/03-clusterboot.png", width = 20, height = 15, res = 300, units = "cm")
par(mar = c(7,5,5,5))
plot(s)
pvrect(s, max.only = FALSE)
dev.off()


# 04 Implicitive Dilemma Plot ----------------------

id <- indexDilemma(boeker, self = 1, ideal = 2)
png("img/04-implicative-dilemmas.png", width = 20, height = 15, res = 300, units = "cm")
par(mar = c(1,1,1,1))
plot(id, node.size = 60, node.text.cex = .9, edge.label.cex = 1.2, 
     edge.label.color = grey(.3), edge.arrow.size = .7)
dev.off()

