###################################################################################
###  					    	SAMPLE ANALYSIS	 								###
###################################################################################

## Distribution of PVAFF index of n quasis ###
l <- listOfRandomGrids(100, 4,4)
res <- as.data.frame(sapply(l, pvaff))
hist(unlist(res), nclass=100)
pdf("output/PAVFF.pdf", height=5)
	hist(unlist(res), nclass=100, main="PVAFF distribution of 10^6 grids sized 7x7")
dev.off()



m <- ggplot(res, aes(x=res))
m + stat_bin(binwidth=0.002)
# l <- listOfRandomGrids(10000)
# res <- lapply(l, function(x) sum(x@data))
# hist(unlist(res), nclass=100)

### Distribution of PVAFF index of n quasis with different number of constructs ###
# skewness for different number of constructs
library(psych)
library(plyr)
l <- list()
pvaffList <- list()
for (nrow in 7:21){
	l[[nrow]] <- listOfRandomGrids(100, nrow=nrow)	
	l[[nrow]] <- unlist(lapply(l[[nrow]], pvaff))
	pvaffList[[nrow]] <- data.frame(constructs=nrow, pvaff= l[[nrow]])	
}
pvaffDf <- list_to_dataframe(pvaffList)
pvaffDf$constructs <- jitter(pvaffDf$constructs)

plot(pvaffDf)
model <- lm(pvaff ~ constructs, pvaffDf)
abline(model)
anova(model)

### Distribution of PVAFF index of n quasis with different number of elements ###
library(psych)
library(plyr)
l <- list()
pvaffList <- list()
for (ncol in 4:10){
	l[[ncol]] <- listOfRandomGrids(100, nrow=10, ncol=ncol)	
	l[[ncol]] <- unlist(lapply(l[[ncol]], pvaff))
	pvaffList[[ncol]] <- data.frame(elements=ncol, pvaff= l[[ncol]])	
}
pvaffDf <- list_to_dataframe(pvaffList)
pvaffDf$elements <- jitter(pvaffDf$elements)
plot(pvaffDf)
model <- lm(pvaff ~ elements, pvaffDf)
abline(model)
anova(model)

### Distribution of PVAFF index of n quasis with different rating scales ###
library(psych)
library(plyr)
l <- list()
pvaffList <- list()
for (scale.max in 1:9){
	l[[scale.max]] <- listOfRandomGrids(100, nrow=10, ncol=10, scale=c(0, scale.max))	
	l[[scale.max]] <- unlist(lapply(l[[scale.max]], pvaff))
	pvaffList[[scale.max]] <- data.frame(scale.max=scale.max, pvaff= l[[scale.max]])	
}
pvaffDf <- list_to_dataframe(pvaffList)
plot(1:9, ddply(pvaffDf, .(scale.max), sd)$pvaff)

pvaffDf$scale.max <- jitter(pvaffDf$scale.max)
plot(pvaffDf)
model <- lm(pvaff ~ scale.max, pvaffDf)
abline(model)
anova(model)


l <- listOfRandomGrids(9, ncol=3, nrow=3)
fg <- frameGrob(layout=grid.layout(3,3))
for(i in 1:3){
	for(j in 1:3){
		index <- 3*(i-1) + j
		fg <- placeGrob(fg, bertin(l[[index]], draw=F), row=i, col=j)
	}
}
grid.draw(fg)


bertin2PlusLegend(rg3, colors=c("darkred", "white"))

bertin2PlusLegend(rg3, sides=unit(25, "mm"), top=unit(20, "mm"), gp.cells=gpar(fontface=2))

go <- bertin2PlusLegend(randomGrid(ncol=10, nrow=12, scale=c(0,2)), colors=c("darkred", "white"), sides=unit(25, "mm"), 
						top=unit(20, "mm"), gp.constructs=gpar(fontface=3), d=F)
size <- getFrameGrobSize(go) 
pdf(height=size[2], width=size[1])
	grid.draw(go)
dev.off()





# a <- randomGrid(10,20, scale=c(0,2))
# pdf(file = "bertin_text_5.pdf", width=8, height=5.5)
# 	bertin(a)
# dev.off()



