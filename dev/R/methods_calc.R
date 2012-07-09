###################################################################################
###  						MEASURES FROM SLATER  								###
###################################################################################

#############
# center centers the rows (constructs) and/or columns around the 
# corresponding mean value
# rg		object of class repgrid
# rows		logical. Wether or not to center the rows (default FALSE)
# cols		logical. Wether or not to center the columns (default FALSE)
#
center <- function(x, rows=FALSE, cols=FALSE){
	if(!(is.data.frame(x) | is.matrix(x)))
		stop("x argument in center() must be a matrix or a dataframe")
	if(rows) x <- sweep(x, 1, apply(x, 1, mean, na.rm=TRUE))
	if(cols) x <- sweep(x, 2, apply(x, 2, mean, na.rm=TRUE))
	x
}
# center(rg1)						# no centering
# center(rg1, rows=T)				# row centering of grid 
# center(rg1, cols=T)				# column centering of grid
# center(rg1, rows=T, cols=T)		# row and column centering
##############


#############
# calculates a vector containing the variation of the grid constructs around the mean
# grid (Slater, 1977, p.77)

variation <- function(x){
	if(!(is.data.frame(x) | is.matrix(x)))
		stop("x argument in variation() must be a matrix or a dataframe")
	D <- as.matrix(center(x, rows=TRUE, cols=FALSE))	# center grid matrix				
	W <- D %*% t(D)										# co-variation Matrix W
	V <- diag(W)										# extract trace (construct variations)
	V
}

# variability(rg)
#############

#############
# calculates the total variation as the sum of variation around the mean
# for each construct. It is esentially the sum of the variation vector received
# by the variation function (Slater, 1977, p.78)
#
totalVariation <- function(x){
	sum(variation(x), na.rm=TRUE)
}
# totalVariability(rg)
#############


#############
#  bias defines how far the centre of the dispersion of the elements deviates 
# from the midpoint of the construct-space is  (Slater, 1977, p.88) 
# STATUS: working and checked against example in Slater, 1977
#
bias <- function(x, min, max){
	if(!(is.data.frame(x) | is.matrix(x)))
		stop("x argument in bias() must be a matrix or a dataframe")
	p <- min + (max - min)/2 						# scale midpoint
	q <- max - p									# distance to scale limits
	n <- nrow(x)									# number of rows (constructs)
	row.means <- apply(x, 1, mean, na.rm=TRUE)		# means of construct rows
	bias <- (sum((row.means - p)^2) / n)^.5 / q		# calculation of bias
	bias
}

# bias(slater1)
# TODO: welche Verteilung hat bias?
#############



#############
# variability is  (Slater, 1977, p.88)
# STATUS: working and checked against example in Slater, 1977 
#
variability <- function(x, min, max){
	if(!(is.data.frame(x) | is.matrix(x)))
		stop("x argument in variability() must be a matrix or a dataframe")
	V <- sum(variation(x), na.rm=TRUE)			# total variation of constructs
	p <- min + (max - min)/2 					# scale midpoint
	q <- max - p								# distance to scale limits
	n <- nrow(x)								# number of rows (constructs)
	m <- ncol(x)								# number of columns (elements)
	variability <- (V/(n*(m-1)))^.5/q			# calculate variability
	variability
}
# TODO: welche Verteilung hat variability?
#############


#############
# normalize standardizes
normalize <- function(rg){
	if(!inherits(rg, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
}



###################################################################################
###  						DISTANCE MEASURES	 								###
###################################################################################

### distance() calculates element distance measure. r is set to 2 as a default 
# leading to the euclidean form
#
distance <- function(repgrid, method = "euclidean", diag = FALSE, upper = FALSE, p = 2){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	x <- repgrid@data
	m <- dist(t(x), method = method, diag = diag, upper = upper, p = p)
	m <- as.matrix(m)
	m
}

# calculation of Slater distances (1977, p. 94) # TODO
slaterDistance <- function(repgrid, method = "euclidean", diag = FALSE, upper = FALSE, p = 2){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	x <- repgrid@data
	m <- dist(t(x), method = method, diag = diag, upper = upper, p = p)
	m <- as.matrix(m)
	m
	
}
	
###################################################################################
###  							COMPLEXITY INDICES 								###
###################################################################################

################################
### DIFFERENTIATION MEASURES ###
################################

### bieri() calculates Bieri's complexity index (Bieri, 1955)
#
bieri <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	
}

### fic() Functionally Independent Construction (FIC) (Landfield, 1977)
fic <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
}

### Percentage of Variance Accounted for by the First Factor (PVAFF) (Okeefe & Sypher, 1981)
pvaff <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	sdev <- princomp(repgrid@data)$sdev
	pvaff <- sdev[1]/sum(sdev)
	pvaff
}

############################
### INTEGRATION MEASURES ###
############################

### Intensity (Fransella & Bannister, 1977)

### Ordination (Landfield & Cannell, 1988)


#######################################################
### COMBINED INTEGRATION & DIFFERENTIATION MEASURES ###
#######################################################

### SQM - Structural quadrant method (Gallifa & Botella, 2000)

sqm <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
		
	ncol <- ncol(repgrid@data)
	
	# Step I:  calculating n*(m-1) similarity matrices (Gallifa & Botella, 2000, p.10)
	smList <- list()													# similarity matrix list
	range <- repgrid@meta$scale.max - repgrid@meta$scale.min		
	for(e in seq_len(ncol)){
		dat <- repgrid@data
		dat <- range - abs(dat - dat[,e])[,-e]
		smList[[e]] <- dat 
	}
	smList	
	
	# Step II: factor analysis of similarity matrices
	makePCAandGetLoadings <- function(x) princomp(x)$loadings[, 1:2]
	lapply(smList, makePCAandGetLoadings)
}


###################################################################################
###  							BERTIN DISPLAY	 								###
###################################################################################

bertin <- function(repgrid, draw=TRUE){
	require(grid)
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	
	# determine color range (shades of grey)
	nrow <- nrow(repgrid@data)
	ncol <- ncol(repgrid@data)
	height.element.label <- 5
	height.cell <- unit(3, "mm")
	
	height.fg.top <- unit(ncol * height.element.label, "mm")
			
	bertinCell <- function(label, fill){
		textColor <- gmSelectTextColorByLuminance(fill)
		gTree(children=gList(
					  rectGrob(width=1, height=1, 
							   gp=gpar(fill=fill, col="white")),
					  textGrob(label=label, gp=gpar(lineheight=.7, cex=.6, col=textColor))
		))		
	}
	
	# rating framegrob
	dp.fg <- frameGrob(grid.layout(nrow=nrow, ncol=ncol, respect=F))
	scale.range <- repgrid@meta$scale.max - repgrid@meta$scale.min
	for (row in seq_len(nrow)){
		for (col in seq_len(ncol)){
			score <- repgrid@data[row, col]
			rg <- bertinCell(label=score, fill=grey(score/scale.range))
			dp.fg <- placeGrob(dp.fg , rg, row = row, col = col)
		}
	}
	
	# left framegrob (initial pole)
	left.c.fg <- frameGrob(grid.layout(nrow=nrow, ncol=1))
	for(row in seq_len(nrow)){
		tg <- textGrob(label=repgrid@constructs$leftPole[row], gp=gpar(cex=.6))
		left.c.fg <- placeGrob(left.c.fg, tg, row=row)
	}
	
	# top framegrob (elements)
	top.e.fg <- frameGrob(grid.layout(ncol=ncol, nrow=ncol + 1, respect=F))
	rg <- rectGrob( gp=gpar(fill="black", col="white"), 
					vp=viewport(width=unit(1, "points")))
	for(row in seq_len(ncol)){
		tg <- textGrob(label=repgrid@elements[row], x=.4, just="left", gp=gpar(cex=.6))
		top.e.fg <- placeGrob(top.e.fg, tg, row=row, col=row)					
		top.e.fg <- placeGrob(top.e.fg, rg, row=row:ncol + 1, col=row)			
	}
	
	# combine framegrobs
	main.fg <- frameGrob(grid.layout(nrow=4, ncol=3, heights=c(.1, 2 ,2,.2), widths=c(1,2,1)))
	main.fg <- placeGrob(main.fg, top.e.fg, row= 2, col = 2)	
	main.fg <- placeGrob(main.fg, left.c.fg, row= 3, col = 1)
	main.fg <- placeGrob(main.fg, dp.fg, row= 3, col = 2)
	main.fg <- placeGrob(main.fg, left.c.fg, row=3, col = 3)
	if(draw) grid.draw(main.fg) else main.fg
}


constructCellGrob <- function(text, gp=gpar(), horiz=TRUE){
	gp <-  modifyList(gpar(fill=grey(.95)), gp)
	col <- gmSelectTextColorByLuminance(gp$fill)
	gTree(children=gList( rectGrob(width=1, height=1, 
							   		gp=gpar(fill=gp$fill, col="white")),
					  	  gmSplitTextGrob(text=text, horiz=horiz, gp=modifyList(gp, gpar(col=col)))
						 ))
}

# makeStandardRangeColorRamp() creates color ramp for supplied colors that takes 
# values between [0,1] and returns a hex color value
#
makeStandardRangeColorRamp <- function(colors, ...){
	ramp <- colorRamp(colors, ...)
	function(x){
		x <- ramp(x)
		rgb(x[, 1], x[, 2], x[, 3], maxColorValue = 255)		
	}	
}


bertin2 <- function(repgrid, ratings=TRUE, top=unit(40, "mm"), sides=unit(40, "mm") , 
					left=sides, right=sides, 
					cell=unit(6, "mm"), cell.height=cell, cell.width=cell, 
					gp.cells=gpar(), gp.constructs=gpar(), gp.elements=gpar(), 
					bg.col=grey(.95), colors=c("black", "white"), draw=TRUE){
	require(grid)
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	
	gp.cells <- modifyList(gpar(lineheight=.7, cex=.6, fill=bg.col), gp.cells) 
	gp.constructs <- modifyList(gpar(lineheight=.7, cex=.8, fill=bg.col), gp.constructs) 
	gp.elements <- modifyList(gpar(lineheight=.7, cex=.8, fill=bg.col), gp.elements) 
	
	# determine color range (shades of grey)
	nrow <- nrow(repgrid@data)
	ncol <- ncol(repgrid@data)
	
	height.top <- top
	width.left <- left
	width.right <- right
	height.cell <- cell.height
	width.cell <- cell.width
	height.body <- 	nrow * height.cell
	width.body <- ncol * width.cell
	
	bertinCell <- function(label, fill, gp=gpar(), ratings=TRUE){
		textColor <- gmSelectTextColorByLuminance(fill)
		gp <- modifyList(gp, gpar(col=textColor))
		if(ratings) tg <- textGrob(label=label, gp=gp) else tg <- nullGrob()
		gTree(children=gList(
					  rectGrob(width=1, height=1, 
							   gp=gpar(fill=fill, col="white")),
					  tg
		))		
	}
	
	# rating framegrob
	colorFun <- makeStandardRangeColorRamp(colors)
	dp.fg <- frameGrob(grid.layout(nrow=nrow, ncol=ncol, respect=F))
	scale.range <- repgrid@meta$scale.max - repgrid@meta$scale.min
	scale.min <- repgrid@meta$scale.min
	for (row in seq_len(nrow)){
		for (col in seq_len(ncol)){
			score <- repgrid@data[row, col]
			rg <- bertinCell(label=score, fill=colorFun((score-scale.min)/scale.range), gp=gp.cells, ratings=ratings)
			dp.fg <- placeGrob(dp.fg , rg, row = row, col = col)
		}
	}
	
	# left framegrob (initial pole)
	left.c.fg <- frameGrob(grid.layout(nrow=nrow, ncol=1))
	for(row in seq_len(nrow)){
		tg <- constructCellGrob(text=repgrid@constructs$leftPole[row], gp=gp.constructs)
		left.c.fg <- placeGrob(left.c.fg, tg, row=row)	
	}
	
	# right framegrob (contrast pole)
	right.c.fg <- frameGrob(grid.layout(nrow=nrow, ncol=1))
	for(row in seq_len(nrow)){
		tg <- constructCellGrob(text=repgrid@constructs$rightPole[row], gp=gp.constructs)
		right.c.fg <- placeGrob(right.c.fg, tg, row=row)	
	}
	
	# top framegrob (elements)
	top.e.fg <- frameGrob(grid.layout(ncol=ncol, nrow=1))
	for(col in seq_len(ncol)){
		tg <- constructCellGrob(text=repgrid@elements[col], horiz=FALSE, gp=gp.elements)
		top.e.fg <- placeGrob(top.e.fg, tg, row=NULL, col=col)					
	}
	
	# combine framegrobs
	main.fg <- frameGrob(grid.layout(nrow=2, ncol=3, heights=unit.c(height.top, height.body), widths=unit.c(width.left, width.body, width.right)))
	main.fg <- placeGrob(main.fg, top.e.fg, row= 1, col = 2)	
	main.fg <- placeGrob(main.fg, left.c.fg, row= 2, col = 1)
	main.fg <- placeGrob(main.fg, dp.fg, row= 2, col = 2)
	main.fg <- placeGrob(main.fg, right.c.fg, row=2, col = 3)
	if(draw) grid.draw(main.fg) else main.fg
}

bertin2PlusLegend <- function(	repgrid, ratings=TRUE, top=unit(40, "mm"), 
								sides=unit(40, "mm"), left=sides, right=sides, 
								cell=unit(6, "mm"), cell.height=cell, cell.width=cell, 
								gp.cells=gpar(), gp.constructs=gpar(), gp.elements=gpar(), 
								bg.col=grey(.95), colors=c("black", "white"), draw=TRUE,
								vspace=unit(2,"mm"), legend.just="left", legend.height=unit(10, "mm"),
								legend.width=unit(40, "mm"))
{
		fg.bertin <- bertin2(	repgrid=repgrid, ratings=ratings, top=top, 
								sides=sides, left=left, right=right, 
								cell=cell, cell.height=cell.height, cell.width=cell.width, 
								gp.cells=gp.cells, gp.constructs=gp.constructs, gp.elements=gp.elements, 
								bg.col=bg.col, colors=colors, draw=FALSE)
		
		widths <- fg.bertin$framevp$layout$widths
		heights <- fg.bertin$framevp$layout$heights
		nrow <- fg.bertin$framevp$layout$nrow
		ncol <- fg.bertin$framevp$layout$ncol

		colorFun <- makeStandardRangeColorRamp(colors)	
		lg <- gmLegend2(colorFun(c(0,1)), c("left pole", "right pole"), ncol=2, byrow=F)
		fg.legend <- frameGrob(grid.layout(widths=legend.width, just=legend.just))
		fg.legend <- placeGrob(fg.legend, lg)
		fg.main <- frameGrob(grid.layout(nrow=nrow + 2, heights=unit.c(heights, vspace, legend.height),
										 ncol=ncol, widths=widths))
		fg.main <- placeGrob(fg.main, fg.bertin, row=1:nrow)
		fg.main <- placeGrob(fg.main, fg.legend , row=nrow + 2)
		
		if(draw) grid.draw(fg.main)	else fg.main
}

# bertin2PlusLegend(rg2, colors=c("darkred", "white"))
# bertin2PlusLegend(rg2, colors=c("darkred", "white"), top=unit(4, "cm"), sides=unit(4, "cm"))


###################################################################################
###  							MISCELLANEOUS	 								###
###################################################################################
# "Slater (1976) named a grid containing random numbers a ”quasi.”" (Hartmann, 1992)
# better call a random grid a quasi as Hartmann (1992) does?
#
randomGrid <- function(nrow=7, ncol=7, scale=c(0,5), step=1){
	rg <- new("repgrid")
	rg@meta$name <- "random grid"				# Entering the grid data from the interview
	rg@meta$scale.min <- scale[1]
	rg@meta$scale.max <- scale[2]
	rg@meta$scale.steps <- step 	
	rg@elements <- paste("Element", seq_len(ncol))
	rg@constructs <- data.frame(leftPole = paste("Construct", seq_len(nrow)), 
								rightPole = paste("Construct", seq_len(nrow)),
								stringsAsFactors = FALSE )
	mat <- matrix(sample(x= seq(scale[1], scale[2], step), size=nrow*ncol, rep=TRUE), nrow=nrow, ncol=ncol)
	dat <- as.data.frame(mat)
	names(dat) <- paste("E_", seq_len(ncol), sep="")
	rg@data <- dat
	rg
}

listOfRandomGrids <- function(no, nrow=7, ncol=7, scale=c(0,5), step=1, progress=TRUE){
	pb <- txtProgressBar(min = 0, max = no, style = 3)  	       	# make progress bar
	l <- list()
	for(i in seq_len(no)){
		l[i]  <- randomGrid(nrow=nrow, ncol=ncol, scale=scale, step=step)
		setTxtProgressBar(pb, i)         	# update progress bar
	}
	close(pb)
	l
}

getRepgridData <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	df <- as.data.frame(c(	subset(repgrid@constructs, , 1), 
						  		repgrid@data,
						  		subset(repgrid@constructs, , 2) ))
	names(df) <- c("left pole", repgrid@elements, "right pole")
	df
}

centerData <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	x <- repgrid@data
	scale.range <- repgrid@meta$scale.max + repgrid@meta$scale.min
	repgrid@results[["centered"]] <- x - scale.range/2							# translate midpoint to zero
	repgrid	
}


esa <- function(repgrid){
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	x <- centerData(repgrid)@results$centered			# center data to scale midpoint
	s <- svd(x) 										# UDV(T)  Singular value decomposition will also work with less rows than columns though it is problematic by definition
	u <- s$u; d <- diag(s$d); v <- s$v;  
	constructs <- u%*%d
	rownames(constructs) <- paste(repgrid@constructs$leftPole, "-", repgrid@constructs$rightPole)
	elements <- v%*%d
	rownames(elements) <- repgrid@elements
	repgrid@results[["esa"]] <- list(e=elements, c= constructs) 
	repgrid
}

esaplot <- function(repgrid, dim=1:2, positioning=TRUE, cex=.7, col.c="lightgrey", col.e="blue", labels.c=TRUE){
	# Lit:  Raeithel, Arne (1998). Kooperative Modellproduktion von Professionellen und Klienten – erläutert 
	# 		am Beispiel des Repertory Grid. In ders., Selbstorganisation, Kooperation, Zeichenprozess. 
	#		Arbeiten zu einer kulturwissenschaftlichen, anwendungsbezogenen Psychologie (S.209-254). 
	#		Opladen: Westdeutscher Verlag. 
	require(maptools)
	if(!inherits(repgrid, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	repgrid <- esa(repgrid)
	res.esa <- repgrid@results[["esa"]]
	all.max <- max(abs(rbind(res.esa$e[,dim], res.esa$c[,dim])))
	par(mar=c(1,1,1,1))
	plot(res.esa$e, type="n", xlim = c(-all.max, all.max), ylim = c(-all.max, all.max), 
		 asp=1, xaxt="n", yaxt="n")
	abline(h=0, v=0, col="lightgrey")
	points(res.esa$e[,dim], col=col.e, pch=15)
	points(res.esa$c[,dim], col=col.c, pch=4)
	points(res.esa$c[,dim]*-1, col=col.c, pch=4)
	labels <- c(repgrid@elements, repgrid@constructs$leftPole, repgrid@constructs$rightPole)
	if(!labels.c) labels[(length(repgrid@elements) + 1):length(labels)] <- ""
	if(positioning){
		pointLabel(rbind(res.esa$e[,dim], res.esa$c[,dim], res.esa$c[,dim]*-1), 
					  labels=labels, doPlot=TRUE, cex=cex)
	} else {
		text(res.esa$e[,dim], labels=repgrid@elements, col=col.e, pch=15, cex=cex, adj=c(1,1))
		text(res.esa$c[,dim], labels=repgrid@constructs$leftPole, col=col.c, pch=4, cex=cex, adj=c(1,1))
		text(res.esa$c[,dim] * -1, labels=repgrid@constructs$rightPole, col=col.c, pch=4, cex=cex, adj=c(1,1))
	}
}


getFrameGrobSize <- function(framegrob, to.unit="inches"){
	widths <- framegrob$framevp$layout$widths
	heights <- framegrob$framevp$layout$heights
	
	widths.conv <- sapply(widths, convertUnit, to.unit, valueOnly = TRUE )
	heights.conv <- sapply(heights, convertUnit, to.unit, valueOnly = TRUE )
	
	# widths.conv <- sapply(widths, convertX, to.unit, valueOnly = TRUE )
	# heights.conv <- sapply(heights, convertY, to.unit, valueOnly = TRUE )
	# 
	c(sum(widths.conv), sum(heights.conv))
}


bootstrapConstructs <- function(repgrid){
	sample.datapool <- rg2@data[2,]
	sample.size <- 6
	sample.replace <- TRUE	
	samples <- lapply_pb(1:100000, function(x) 
				sample(x=sample.datapool, size=sample.size, replace=sample.replace))
	means <- lapply_pb(samples, function(x) mean(unlist(x)))	
	hist(unlist(means), nclass=100, col="grey")
}








