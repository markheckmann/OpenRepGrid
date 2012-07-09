################################################################
### 				basic meta operations					 ###
################################################################

setMeta <- function(x, id=NULL, name=NULL, interviewer=NULL, date=NULL, replace=FALSE, ...){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	meta <- list(id=id, 									# meta data is saved as a list
				 date= date,
				 name=name,
				 interviewer=interviewer)										
	if(replace){
		x@meta <- meta
	} else {
		meta <- meta[sapply(meta, function(x)!is.null(x))]
		x@meta <- modifyList(x@meta, meta)					# replace new meta values
	}
	x
}

## NOT RUN
# rg1 <- setMeta(rg1, id=1, date= date(), name="John Doe", interviewer="Mark Heckmann")
# str(rg1)
# rg1 <- setMeta(rg1, replace=T) 			# delete all meta references
# rg1 <- setMeta(rg1, id=1, date= date(), name="John Doe", interviewer="Mark Heckmann")
# rg1 <- setMeta(rg1, id=2, replace=T) 	# change id, delete the rest
# 

################################################################
### 				basic scale operations					 ###
################################################################

setScale <- function(x, type=NULL, min=NULL, max=NULL, step=NULL, replace=FALSE, ...){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	scale <- list(type = type,
				  min = min,
				  max = max,
				  step = step)
	if(replace){
		x@scale <- scale
	} else {
		scale <- scale[sapply(scale, function(x)!is.null(x))]
		x@scale <- modifyList(x@scale, scale)					# replace new meta values
	}
	x
}

# rg1 <- setScale(rg1, type="rating", min=1, max=5, step=1)
# rg1 <- setScale(rg1, type="rating", min=1, max=5, step=1)


