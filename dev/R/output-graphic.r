###################################################################################
###  							BERTIN DISPLAY	 								###
###################################################################################

bertin <- function(x, draw=TRUE){
	require(grid)
	if(!inherits(x, "repgrid")) 
		stop("Object must be of class 'repgrid'.")
	
	# determine color range (shades of grey)
	nrow <- nrow(x@ratings[,,1])
	ncol <- ncol(x@ratings[,,1])
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
	scale.range <- x@scale$max - x@scale$min
	for (row in seq_len(nrow)){
		for (col in seq_len(ncol)){
			score <- x@ratings[row,col,1]
			rg <- bertinCell(label=score, fill=grey(score/scale.range))
			dp.fg <- placeGrob(dp.fg , rg, row = row, col = col)
		}
	}
	
	# left framegrob (initial pole)
	left.c.fg <- frameGrob(grid.layout(nrow=nrow, ncol=1))
	for(row in seq_len(nrow)){
		tg <- textGrob(label=x@constructs$left[row], gp=gpar(cex=.6))
		left.c.fg <- placeGrob(left.c.fg, tg, row=row)
	}
	
	# right framegrob (contrast pole)
	right.c.fg <- frameGrob(grid.layout(nrow=nrow, ncol=1))
	for(row in seq_len(nrow)){
		tg <- textGrob(label=x@constructs$right[row], gp=gpar(cex=.6))
		right.c.fg <- placeGrob(right.c.fg, tg, row=row)
	}
	
	# top framegrob (elements)
	top.e.fg <- frameGrob(grid.layout(ncol=ncol, nrow=ncol + 1, respect=F))
	rg <- rectGrob( gp=gpar(fill="black", col="white"), 
					vp=viewport(width=unit(1, "points")))
	for(row in seq_len(ncol)){
		tg <- textGrob(label=unlist(x@elements)[row], x=.4, just="left", gp=gpar(cex=.6))
		top.e.fg <- placeGrob(top.e.fg, tg, row=row, col=row)					
		top.e.fg <- placeGrob(top.e.fg, rg, row=row:ncol + 1, col=row)			
	}
	
	# combine framegrobs
	main.fg <- frameGrob(grid.layout(nrow=4, ncol=3, heights=c(.1, 2 ,2,.2), widths=c(1,2,1)))
	main.fg <- placeGrob(main.fg, top.e.fg, row= 2, col = 2)	
	main.fg <- placeGrob(main.fg, left.c.fg, row= 3, col = 1)
	main.fg <- placeGrob(main.fg, dp.fg, row= 3, col = 2)
	main.fg <- placeGrob(main.fg, right.c.fg, row=3, col = 3)
	if(draw) grid.draw(main.fg) else main.fg
}
