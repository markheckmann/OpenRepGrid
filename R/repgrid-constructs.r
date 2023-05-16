#----------------------------------------------#
### 				basic construct operations		   ###
#----------------------------------------------#

# Function that start with c. operate on the constructs only.
# These functions serve for basic operations on constructs.
# In case a function needs to operate on constructs and other
# slots (e.g. elements, ratings) higher-level functions
# that perform joints operations are used. The base operations
# are not needed when using openrepgrid by the user.

## constructs
# add constructs
# delete constructs
# rename pole(s)
# change construct order
# set pole status (emerged, preferred etc.)
# reverse poles

# pole--+
#		+--name
#		+--preferred
#		+--emerged



##############   FUNCTIONS TO RETRIEVE INFORMATION FROM REPGRID OBJECTS   ##################

#' Get construct names
#'
#' @param x \code{repgrid} object.
#' @section Deprecated functions: \code{getConstructNames()},
#'   and \code{cNames()} have been deprecated.
#'   Instead use \code{constructs()}.
#' @export
#' @keywords internal
#'
getConstructNames <- function(x){
	if (!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'")

  l <- lapply(x@constructs, function(x) 
                 data.frame(leftpole=x$leftpole$name, 
                            rightpole=x$rightpole$name, stringsAsFactors=FALSE))
	list_to_dataframe(l)
}
cNames <- getConstructNames


#' Retrieves the construct names from a \code{repgrid}.
#' 
#' Different features like trimming, indexing and choices of separators
#' allow to return the kind of format that is needed.
#'
#' @title Retrieve construct names in needed format.
#'
#' @param x       \code{repgrid} object.
#' @param mode    Type of output. 1= left and right pole 
#'                separated by \code{sep}), 2= only left pole,
#'                3 = only right pole.
#' @param trim    Number of characters to trim the construct names to
#'                (default \code{NA}). \code{NA} will suppress trimming.
#'                The length of \code{index} is not included in the 
#'                trimming.
#' @param index   Logical. Whether to add a index number before the construct
#'                names (default \code{TRUE}).
#' @param sep     Separator string between poles.
#' @param pre     String before index number (default \code{(}).
#' @param post    String after index number (default \code{) }).
#' @return        Vector with construct names.
#' @export
#' @keywords internal
#' @examples \dontrun{
#'  
#'   getConstructNames2(bell2010)
#'   getConstructNames2(bell2010, mode=2)
#'   getConstructNames2(bell2010, index=T)
#'   getConstructNames2(bell2010, index=T, mode=3)
#'
#' }
#'
getConstructNames2 <- function(x, mode=1, trim=20, index=F, 
                               sep = " - ", pre="(", post=") " ){
  if (!inherits(x, "repgrid")) 							
   	 stop("Object x must be of class 'repgrid'")
  
  cnames <- constructs(x)
  cnames.l <- cnames[ ,1]
  cnames.r <- cnames[ ,2]
  
  # add numeric index in front of constructs
  if (index)
    ind <- paste(pre, seq_along(cnames.l), post, sep="") else
    ind  <- ""

  # trim names if prompted
  if (!is.na(trim)){ 
    if( mode == 1 ) # adjust trimming length if both sides are included
      trim <- ceiling(trim / 2)                             
    cnames.l <- substr(cnames.l, 1, trim)
    cnames.r <- substr(cnames.r, 1, trim)
  }
  
  if (mode == 1)    # left and right poles
    cnames.new <- paste(ind, cnames.l, sep, cnames.r, sep="") 
  if (mode == 2)    # left pole only
    cnames.new <- paste(ind, cnames.l, sep="") 
  if (mode == 3)    # right pole only
    cnames.new <- paste(ind, cnames.r, sep="") 
  cnames.new
}


#' Get or replace construct poles 
#' 
#' Allows to get and set construct poles. 
#' Replaces the older functions `getConstructNames`, `getConstructNames2`,
#' and `eNames` which are deprecated.
#' 
#' @param x A repgrid object.
#' @param i,j Row and column Index of repgrid matrix.
#' @param value Character vector of poles.
#' @param collapse Return vector with both poles instead.
#' @param sep Separator if `collapse = TRUE`, default is `" - "`.
#' @rdname constructs
#' @export
#' @md
#' @examples 
#' 
#' # shorten object name
#' x <- boeker
#' 
#' ## get construct poles
#' constructs(x)   # both left and right poles
#' leftpoles(x)    # left poles only
#' rightpoles(x)
#' constructs(x, collapse = TRUE)
#' 
#' ## replace construct poles
#' constructs(x)[1,1] <- "left pole 1"
#' constructs(x)[1,"leftpole"] <- "left pole 1"  # alternative
#' constructs(x)[1:3,2] <- paste("right pole", 1:3)
#' constructs(x)[1:3,"rightpole"] <- paste("right pole", 1:3) # alternative
#' constructs(x)[4,1:2] <- c("left pole 4", "right pole 4")
#' 
#' l <- leftpoles(x)
#' leftpoles(x) <- sample(l)             # brind poles into random order
#' leftpoles(x)[1] <- "new left pole 1"  # replace name of first left pole
#' 
#' # replace left poles of constructs 1 and 3
#' leftpoles(x)[c(1,3)] <- c("new left pole 1", "new left pole 3")
#'  
constructs <- function(x, collapse = FALSE, sep = " - ")
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get left and right pole name properties from each construct
  df <- data.frame(
    leftpole = leftpoles(x),
    rightpole = rightpoles(x), 
    stringsAsFactors = FALSE
  )
  if (collapse) {
    res <- paste0(df$leftpole, sep, df$rightpole)
    return(res)
  }
  df
}


#' @rdname constructs
#' @export
`constructs<-` <- function(x, i, j, value) 
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get dataframe and replace using bracket operator
  # this replacement type function somehow works. 
  # Other attempts falied.
  d <- constructs(x)
  d[i, j] <- value
  
  # replace left and right poles 
  leftpoles(x) <- d$leftpole
  rightpoles(x) <- d$rightpole
  
  x
}


#' @rdname constructs
#' @export
leftpoles <- function(x)
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get left pole name property from each construct
  sapply(x@constructs, function(x) x$leftpole$name)
}


#' @param position Index where to insert construct
#' @param value Character vector of construct poles names.
#' @rdname constructs
#' @export
`leftpoles<-` <- function(x, position, value) 
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get element names and replace one or more
  p <- leftpoles(x)
  p[position] <- value
  
  # replace leftpole name property of each construct
  for (i in seq_along(p)) 
    x@constructs[[i]]$leftpole$name <- p[i]
  
  x
}

#' @rdname constructs
#' @export
rightpoles <- function(x)
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get left pole name property from each construct
  sapply(x@constructs, function(x) x$rightpole$name)
}

#' @rdname constructs
#' @export
`rightpoles<-` <- function(x, position, value) 
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get element names and replace one or more
  p <- rightpoles(x)
  p[position] <- value
  
  # replace leftpole name property of each construct
  for (i in seq_along(p)) 
    x@constructs[[i]]$rightpole$name <- p[i]
  
  x
}



constructInfo <- function(x, all=TRUE){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")	
	l <- lapply(x@constructs, function(x) data.frame(
	                         leftpole=x$leftpole$name,
		 											 l.preffered=x$leftpole$preferred,
													 l.emerged=x$leftpole$emerged,
													 rightpole=x$rightpole$name,
													 r.preffered=x$rightpole$preferred,
													 r.emerged=x$rightpole$emerged, 
													 stringsAsFactors=FALSE))
  info <- list_to_dataframe(l)  #old version using lapply above
	if (all)
	  info else
	  info[c("leftpole", "rightpole")]
}
cInfo <- constructInfo
#constructInfo(x)


getNoOfConstructs <- function(x){
  if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	length(x@constructs)
}
nc <- getNoOfConstructs




# internal. c_makeNewConstruct is the constructor for construct object (simple list)
c_makeNewConstruct <- function(x=NULL, l.name=NA, l.preferred=NA, l.emerged=NA, 
									 r.name=NA, r.preferred=NA, r.emerged=NA, ...){
	list(leftpole=list(name=l.name,
		 			   preferred=l.preferred,
					   emerged=l.emerged),
		 rightpole=list(name=r.name,
						preferred=r.preferred,
						emerged=r.emerged))
}
#str(c_makeNewConstruct())



# internal: c.setConstructs sets constructs by index
c.setConstructs <- function(x, l.name=NA, l.preferred=NA, l.emerged=NA, 
						     r.name=NA, r.preferred=NA, r.emerged=NA, 
							 index=NULL, ...){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(!is.atomic(l.name) | !is.atomic(l.name))
		stop("arguments l.name and r.name must be a vector")
	if(!is.logical(c(l.preferred, l.emerged, r.preferred, r.emerged)))
		stop("arguments l.preferred, r.preferred, l.emerged and r.emerged must be a vector")
	if(is.null(index)){
		index <- seq_len(max(c(length(l.name)), c(length(r.name))))
	}
	if(!(is.na(l.name[1]) & is.na(r.name[1]))){
		newConstructs <- mapply(c_makeNewConstruct, NA, 
								l.name=l.name, l.preferred=l.preferred, l.emerged=l.emerged, 
								r.name=r.name, r.preferred=r.preferred, r.emerged=r.emerged,  
								SIMPLIFY=FALSE)  												# generate new construct list	
		x@constructs[index] <-  newConstructs
	}
	x
}
# x <- makeEmptyRepgrid()
# x <- c.setConstructs(x, l.name=c("construct left 1", "construct left 2"))
# x <- c.setConstructs(x, l.n=c("construct left 3", "construct left 4"), index=3:4)
# str(x@constructs, m=3)


# internal: c_addConstruct adds constructs at the bottom
c_addConstruct <- function(x, l.name=NA, l.preferred=NA, l.emerged=NA, 
					          r.name=NA, r.preferred=NA, r.emerged=NA, 
						      position=NA, side="pre"){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(!is.numeric(position) & !(length(position)==1 & is.na(position[1])))
		stop("position must be numeric.")
	if(position<0 | position>length(x@constructs)+1)
		stop("USERINFO: position must be between 1 and number of constructs plus 1.")
	if(length(position)==1 & is.na(position[1])) position <- length(x@constructs)+1
	constructs.old <- x@constructs
	constructs.new <- mapply(c_makeNewConstruct, NA, 												 # generate construct list
							l.name=l.name, l.preferred=l.preferred, l.emerged=l.emerged, 
							r.name=r.name, r.preferred=r.preferred, r.emerged=r.emerged,  
							SIMPLIFY=FALSE)
	index <- insertAt(seq_along(x@constructs), position, side=side)
	x@constructs[index$index.base.new] <- constructs.old[index$index.base]
	x@constructs[index$index.insert.new] <- constructs.new
	x	
}

#x <- makeEmptyRepgrid()
#x <- c.setConstruct(x, l.name=c("construct left 1"))
#x <- c_addConstruct(x, l.name="construct added at the end", r.name="test", pos=1)
#x <- c_addConstructs(x, l.name="construct left inserted at position 1", pos=1)
#x <- c_addConstructs(x, l.name="construct right inserted at position 1", pos=1)
#x <- c_addConstructs(x, l.name=c("construct 10", "element 11"), pos=10:11)
#str(x@constructs, m=3)



# internal: c_addConstructs. all elements that do not have a position specified are added at the end
c_addConstructs <- function(x, l.name=NA, l.preferred=NA, l.emerged=NA, 
						     r.name=NA, r.preferred=NA, r.emerged=NA, 
							 position=NA, side="pre"){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(!is.numeric(position) & !(length(position)==1 & is.na(position[1])))
		stop("position must be numeric.")
	len <- max(c(length(l.name), length(r.name)))
	if(length(position)==1 & is.na(position[1])){
		position <- rep(NA, len)
	}
	position[is.na(position)] <- seq_along(position[is.na(position)]) + length(x@constructs)
	constructs.old <- x@constructs
	constructs.new <- mapply(c_makeNewConstruct, NA, 												 # generate construct list
							l.name=l.name, l.preferred=l.preferred, l.emerged=l.emerged, 
							r.name=r.name, r.preferred=r.preferred, r.emerged=r.emerged,  
							SIMPLIFY=FALSE)
	index <- insertAt(seq_along(x@constructs), position, side=side)
	x@constructs[index$index.base.new] <- constructs.old[index$index.base]
	x@constructs[index$index.insert.new] <- constructs.new
	x	
}
### NOT RUN
#x <- makeEmptyRepgrid()
#x <- c.setConstructs(x, l.name=c("construct left 1", "construct left 2"))
#x <- c_addConstructs(x, l.name="construct added at the end")
#x <- c_addConstructs(x, l.name="construct left inserted at position 1", pos=1)
#x <- c_addConstructs(x, l.name="construct right inserted at position 1", pos=1)
#x <- c_addConstructs(x, l.name=c("construct 10", "element 11"), pos=10:11)
#str(x@constructs, m=3)












###  maybe unnecessary functions ###

# c.removeNullConstructs <- function(x){
#   if(!inherits(x, "repgrid"))               # check if x is repgrid object
#     stop("Object x must be of class 'repgrid'.")
#   x@constructs <- x@constructs[!sapply(x@constructs, is.null)]
#   x
# }




