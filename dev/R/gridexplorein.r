# read in my sample grid
setwd(dirData)
samplegrid <- read.table("boeker.dat")

# Ward Element Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(samplegrid, method.hclust="ward", method.dist="euclidean")
plot(fit) 					# dendogram with p values
pvrect(fit, alpha=.95) 		# put rectangles around groups highly supported by the data


# transpose the grid to cluster constructs
constructs <- t(samplegrid)

# Ward Construct Hierarchical Clustering with Bootstrapped p values
fit <- pvclust(constructs, method.hclust="ward",
   method.dist="euclidean")
plot(fit) # dendogram with p values
# put rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 

# find number of components by parallel test
library (psych)
ncomp <- fa.parallel(constructs,fa="pc")

pcafit <- princomp(constructs, cor=TRUE)
summary(pcafit) # print variance accounted for
loadings(pcafit) # pc loadings
fit$scores # the principal components
biplot(pcafit) 
# correspondence analysis
library (ca)
fit <- ca(constructs)
plot(fit)


## unfolding 
library(munfold)
split.screen(c(2,1))
screen(1)
grid.unfolded <- unfold(as.matrix(samplegrid), ndims=2)
biplot(grid.unfolded)
screen(2)        
grid.unfolded <- unfold(constructs, ndims=2)
biplot(grid.unfolded)
text(grid.unfolded$A, labels=rownames(grid.unfolded$A), cex=.7)
text(grid.unfolded$B, labels=rownames(grid.unfolded$B), cex=.7)


