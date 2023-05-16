#-------------------------------------------------------------#
#  			    	basic operations on repgrid objects       	    #	 
#-------------------------------------------------------------#


#' Test if object has class repgrid 
#' @param x Any object.
#' @export
is.repgrid <- function(x) {
  inherits(x, "repgrid")
}


#' Raise error if not is repgrid object
#' @param x Object to test. Raises an error if no `repgrid` object.
#' @param name Name of tested variable in error message.
#' @export
#' @keywords internal
#' @md
stop_if_not_is_repgrid <- function(x, name = "x") 
{
  if (!is.repgrid(x))
    stop("Object '", name, "' must have class 'repgrid'", call. = FALSE)
}


#' Raise error if min or max of rating scale are undefined
#' @param x Object to test. Raises an error the mina and max of the ratings scale are not defined
#' @export
#' @keywords internal
#' @md
stop_if_scale_not_defined <- function(x) 
{
  stop_if_not_is_repgrid(x, name)
  
  if (identical(x@scale$min, NA) | identical(x@scale$min, NULL))
    stop("No min value for the rating scale defined. To define the scale use setScale().")
  if (identical(x@scale$max, NA) | identical(x@scale$max, NULL))
    stop("No max value for the rating scale defined. To define the scale use setScale().")
}


############################# EXTRACT AND SET #################################

## S4 methods
# overloading primitive generic "[" getter
# "[" is supposed to function like always, i.e. positive integers for selection
# or reordering negative integers for deletion. These cannot be mixed
# TODO: ?keep single entry as row selection. Normally its column selection e.g. 
# in data frames.

# aliases [,repgrid-method
# docType methods

#' Extract parts of the repgrid object.
#'
#' Methods for \code{"["}, i.e., subsetting of repgrid objects. 
#' 
#' @param  x  A \code{repgrid} object.
#' @param i,j   Row and column indices.
#' @param ...   Not evaluated.
#' @param drop  Not used.
#' @rdname extract-methods
#' @aliases [,repgrid-method
#' @include repgrid.r
#' @examples 
#' 
#'    x <- randomGrid()
#'    x[1:4, ] 
#'    x[ , 1:3] 
#'    x[1:4,1:3] 
#'    x[1,1]
#'
setMethod("[", signature(x = "repgrid", i = "ANY", j="ANY"),
  function (x, i, j, ..., drop)
  {
    dots <- list(...)
		if(length(dots)==0){
			layer <- seq_along(dim(x@ratings)[3])   # 1:3
		} else if(!is.numeric(dots[[1]])){			
		  stop("... must be numeric as it is third index for 3D-array.")
		} else if(!any(dots[[1]] %in% 1:3)){
			stop("... must be an integer between from 1 to 3.")
		} else {
			layer <- dots[[1]]
		}
		if(missing(i)) 
			i <- seq_len(length(x@constructs))				
		if(missing(j)) 
			j <- seq_len(length(x@elements))
		if(!is.numeric(c(i, j)))                                         # check if i,j are numeric
			stop("All index values must be numeric")
		if(any(is.na(c(i, j))))
			stop("NA values are not allowed as indexes.")               
		if(!((all(i >=0 ) | all(i <= 0)) & (all(j >= 0) | all(j <= 0))))			# check if i and j are each only positive or only negative
			stop("Negative and positive indexes for constructs/elements must not be mixed. ",
			     "A positive index will select an element/construct a negative one will delete it.")
		if(any(i > length(x@constructs)) | any(i == 0))						# check if all indexes do not exceed numer of elements or constructs
			stop("index for constructs is out of range. ",
			     "Index must not exceed the number of constructs or equal zero.")
		if(any(j > length(x@elements)) | any(j == 0))							# check if all indexes do not exceed numer of elements or constructs
			stop("index for elements is out of range. ",
			     "Index must not exceed the number of elements or equal zero.")		
		x@constructs <- x@constructs[i]
		x@elements <- x@elements[j]
		x@ratings <- x@ratings[i, j, layer, drop=FALSE]
		x
})

# aliases [<-,repgrid-method 
# docType methods

# overloading primitive generic "[<-" setter. 
#
#' Method for "<-" assignment of the repgrid ratings. 
#'
#' It should be possible to use it for ratings on all layers.
#' 
#' @param x  A \code{repgrid} object.
#' @param i,j     Row and column indices.
#' @param value   Numeric replacement value(s).
#' @param ...     Not evaluated.
#' @rdname subassign
#' @include repgrid.r
#' @examples \dontrun{
#' x <- randomGrid()
#' x[1,1] <- 2
#' x[1, ] <- 4
#' x[ ,2] <- 3
#'    
#' # settings values outside defined rating scale 
#' # range throws an error
#' x[1,1] <- 999
#'    
#' # removing scale range allows arbitary values to be set
#' x <- setScale(x, min = NA, max=NA) 
#' x[1,1] <- 999
#' }
#'
setMethod("[<-", signature(x = "repgrid", i = "ANY", j="ANY", value="ANY"),
  function (x, i, j, ..., value)
  {
    dots <- list(...)
    if(length(dots)==0){
      layer <- 1
    } else if(!is.numeric(dots[[1]])){
      stop("... must be numeric as it is third index for 3D-array")
    } else if(!any(dots[[1]] %in% 1:3)){
      stop("... must be an integer between from 1 to 3")
    } else {
      layer <- dots[[1]]
    } 
    if (missing(i)) 
      i <- seq_len(length(x@constructs))        
    if (missing(j)) 
      j <- seq_len(length(x@elements))
    if (!is.numeric(c(i,j)))                      # check if i,j are numeric
      stop("All index values must be numeric")
    if (any(is.na(c(i,j))))
      stop("NA values are not allowed as indexes")
    if (!((all(i >= 0) | all(i <= 0)) & (all( j >= 0) | all(j <= 0))))      # check if i and j are each only positive or only negative
      stop("Negative and positive indexes for constructs/elements must not be mixed.",
           " A positive index will select an element/construct a negative one will delete it")
    if (any(i > length(x@constructs)) | any(i == 0))            # check if all indexes do not exceed numer of elements or constructs
      stop("index for constructs is out of range.", "
            Index must not exceed the number of constructs or equal zero.")
    if (any(j > length(x@elements)) | any(j == 0))              # check if all indexes do not exceed numer of elements or constructs
      stop("index for elements is out of range. Index must not",
            " exceed the number of elements or equal zero.")
    
    # prevent values outside of scale range from being set
    s <- getScale(x)
    sn <- is.null(s) | any(is.na(s))
    if ( (!sn & (any(value < s[1]) | any(value > s[2])) ) )
      stop("Setting values outside of defined scale range is not allowed.",
           " Use 'getScale' to see and 'setScale' to define the scale range. ", call. = FALSE)
    
    x@ratings[i, j, layer] <- value                               
    # to fill by rows
      #as.vector(matrix(as.vector(value), ncol=length(x@elements), byrow=TRUE))
    # another idea to fill by rows by transposing the part of importance
      # f <- t(d[1:3, 1:2])
      # f[,] <- 1:6
      # d[1:3, 1:2] <- t(f)
    x 
})


###########################  GETTER AND SETTER  ###############################


#' get rating layer
#'
#' @param   x       \code{repgrid} object.
#' @param   layer   layer to be returned.
#' @param   names   extract row and columns names (constructs and elements).
#' @param trim      the number of characters a row or column name is trimmed to 
#'                  (default is \code{10}). If \code{NA} no trimming is done. Trimming
#'                  simply saves space when displaying the output.
#' @return          a \code{matrix} 
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'      getRatingLayer(bell2010)
#' }
#'
getRatingLayer <- function(x, layer=1, names=TRUE, trim=10)
{
  scores <- x@ratings[ , , layer, drop=FALSE]       # select layer
  rm <- apply(scores, 2 , I)                        # convert array to matrix 
  if (names) {
    cnames.l <- constructs(x)$leftpole
    cnames.r <- constructs(x)$rightpole
    enames <- elements(x)
    if (!is.na(trim)){                              # trim names if prompted
      cnames.l <- substr(cnames.l, 1, trim)
      cnames.r <- substr(cnames.r, 1, trim)
      enames <- substr(enames, 1, trim)
    }                             
    rownames(rm) <- paste(cnames.l, cnames.r, sep=" - ") 
    colnames(rm) <- enames   
  }
  rm
}


# 'ratings' function replaces 'getRatingLayer' 
# as it sounds simpler

#' Extract ratings (wide or long format)
#'
#' @param x A \code{repgrid} object.
#' @param names Extract row and columns names (constructs and elements).
#' @param trim The number of characters a row or column name is trimmed to
#'   (default is \code{10}). If \code{NA} no trimming is done. Trimming simply
#'   saves space when displaying the output.
#' @param long Return as long format? (default \code{FALSE})
#' @param i,j   Row and column indices.
#' @param value   Numeric replacement value(s).
#' @return A \code{matrix}.#'
#' @export
#' @rdname ratings
#' @seealso \code{`[<--method`}
#' @examples 
#' 
#' ## store Bell's dataset in x
#' x <- bell2010
#' 
#' ## get ratings
#' ratings(x)
#' 
#' 
#' ## replace ratings   
#'        
#' ratings(x)[1,1] <- 1
#' # noet that this is even simpler using the repgrid object directly
#' x[1,1] <- 2
#' 
#' #replace several values
#' 
#' ratings(x)[1,1:5] <- 1
#' x[1,1:5] <- 2   # the same
#' 
#' ratings(x)[1:3,5:6] <- matrix(5, 3, 2)
#' x[1:3,5:6] <- matrix(5, 3, 2)   # the same
#' 
#' 
#' ## ratings as dataframe in wide or long format
#' 
#' ratings_df(x)
#' ratings_df(x, long = TRUE)
#' 
ratings <- function(x, names = TRUE, trim = 10)
{
  layer <- 1 #  get first layer of ratings array. The other two are not used
  scores <- x@ratings[ , , layer, drop=FALSE]       # select layer
  rm <- apply(scores, 2 , I)                        # convert array to matrix 
  if (names) {
    cnames.l <- constructs(x)$leftpole
    cnames.r <- constructs(x)$rightpole
    enames <- elements(x)
    if (!is.na(trim)){                              # trim names if prompted
      cnames.l <- substr(cnames.l, 1, trim)
      cnames.r <- substr(cnames.r, 1, trim)
      enames <- substr(enames, 1, trim)
    }                             
    rownames(rm) <- paste(cnames.l, cnames.r, sep=" - ") 
    colnames(rm) <- enames   
  }
  rm
}


#' @export
#' @rdname ratings
ratings_df <- function(x, long = FALSE, names = TRUE, trim = NA) 
{
  r <- ratings(x, trim = trim, names = names)
  if (!names) {
    colnames(r) <- paste0("E", 1L:ncol(x))
  }
  r <- as.data.frame(r)
  nc <- nrow(r)
  cn <- constructs(x)
  ii <- data.frame(construct_index = 1L:nc)
  df <- cbind(ii, cn, r)
  rownames(df) <- NULL
  if (long) {
    df <- tidyr::pivot_longer(df, cols = names(r), names_to = "element", values_to = "rating") 
  }
  df
}


#' @rdname ratings
#' @export
`ratings<-` <- function(x, i, j, value) 
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # fet rating matrix and replace values
  r <- ratings(x)
  r[i, j] <- value
  
  # replace all ratings
  x[,] <- r
  x
}



#' Get number of constructs
#'
#' @param x \code{repgrid} object
#' @return \code{numeric}
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'      getNoOfConstructs(bell2010)
#' }
#'
getNoOfConstructs <- function(x){
  if (!inherits(x, "repgrid"))   # check if x is repgrid object
    stop("object x and y must be of class 'repgrid'")
  length(x@constructs)
}


#' Get number of elements
#'
#' @param x \code{repgrid} object
#' @return \code{numeric} 
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'      getNoOfElements(bell2010)
#' }
#'
getNoOfElements <- function(x){
  if (!inherits(x, "repgrid")) 	# check if x is repgrid object
    stop("object x and y must be of class 'repgrid'")
  length(x@elements)
}


#' Set the scale range of a grid. 
#'
#' The scale must be known for certain 
#' operations, e.g. to swap the construct poles. If the user construes
#' a grid he should make sure that the scale range is set correctly.
#'
#' @param x       \code{repgrid} object.
#' @param min     Minimal possible scale value for ratings.
#' @param max     Maximal possible scale value for ratings.
#' @param step    Steps the scales uses (not yet in use).
#' @param ...     Not evaluated.
#' 
#' @return \code{repgrid} object
#' @export
#' @examples \dontrun{
#'
#'    x <- bell2010
#'    x <- setScale(x, 0, 8)   # not set correctly
#'    x
#'    x <- setScale(x, 1, 7)   # set correctly
#'    x
#' }
#'
setScale <- function(x, min, max, step, ...){         # ... needes for makeRepgrid call
  if(!inherits(x, "repgrid"))   						# check if x is repgrid object
    stop("Object x must be of class 'repgrid'")
  if (!missing(min)){
    if (any(x@ratings < min, na.rm=TRUE))  # any rating value smaller than min?
      stop("Some ratings are smaller than the min value you entered. ",
           "The setting of the min value in the grid was not performed. ", 
           "Please check the ratings or choose another min value.")
    x@scale$min <- min
  }
  if (!missing(max)){
    if (any(x@ratings > max, na.rm=TRUE))  # any rating value smaller than min?
      stop("Some ratings are bigger than the max value you entered. ",
           "The setting of the max value in the grid was not performed. ", 
           "Please check the ratings or choose another max value.")
    x@scale$max <- max
  }
  if (!missing(step))
    x@scale$step <- step
  x
}
# setScale(x, min=1, max=5, step=1)


#' Get minimum and maximum scale value used in grid.
#'
#' The values are returned either as a vector or a list.
#'
#' @param x         \code{repgrid} object.
#' @param output    Type of output object. 1= named vector, 2 = list.
#' @return          Vector or list (depends on \code{output} containing 
#'                  minimum and maximum scale value.
#' @keywords        internal
#' @export
getScale <- function(x, output=1){
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
    stop("Object x must be of class 'repgrid'")
  smin <- x@scale$min
  smax <- x@scale$max
  if (output == 1)
    res <- c(min=smin, max=smax) else 
      if (output == 2)
        res <- list(min=smin, max=smax)
  res
}


### TODO
#' setMeta
#'
#' set meta data of a grid (e.g. id, name of interview partner)
#'
#' @param x     repgrid object
#' @param type  type of grid in use (rating, ranked, implication)
#' @param id    id of the interview
#' @param name  name of the interview partner
#' @return \code{repgrid} object
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'    ####  TODO  ####
#' }
#'
setMeta <- function(x, type, id, name){
  if (!inherits(x, "repgrid"))   						# check if x is repgrid object
    stop("Object x must be of class 'repgrid'")
  if (!missing(type))                 # rating, rank or implication
    x@meta$type <- type
  if (!missing(name))
    x@meta$id <- id
  if (!missing(name))
    x@meta$name <- name
  x
}
#x <- setMeta(x, id=1, name="John Doe")


#' Get midpoint of the grid rating scale
#'
#' @param x     \code{repgrid} object.
#' @return      Midpoint of scale.
#'
#' @export
#' @keywords    internal
#' @examples \dontrun{
#'
#'      getScaleMidpoint(bell2010)
#'
#' }
#'
getScaleMidpoint <- function(x){
  if (!inherits(x, "repgrid"))   # check if x is repgrid object
    stop("object x and y must be of class 'repgrid'")
  (x@scale$max - x@scale$min)/2 + x@scale$min
}


#' Midpoint of the grid rating scale
#'
#' @param x    \code{repgrid} object.
#' @return    Midpoint of scale.
#'
#' @export
#' @examples 
#' midpoint(bell2010)
#'
midpoint <- getScaleMidpoint


#############################  CHANGE POSITION   ##############################

#' Swap the position of two elements in a grid.
#'
#' @param x     \code{repgrid} object.
#' @param pos1  Column number of first element to be swapped (default=1).
#' @param pos2  Column number of second element to be swapped (default=1).
#'
#' @return \code{repgrid} object.
#' @export
#' @examples \dontrun{
#'    x <- randomGrid()
#'    swapElements(x, 1, 3)       # swap elements 1 and 3
#'    swapElements(x, 1:2, 3:4)   # swap element 1 with 3 and 2 with 4
#' }
#'
swapElements <- function(x, pos1=1, pos2=1){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
  	stop("Object x must be of class 'repgrid'.")
  if(any(c(pos1, pos2) < 0) | any(c(pos1, pos2)> length(x@elements)))
  	stop("pos1 and pos2 must be bigger than 1 and have number of elements as a maximum")
	if(any(c(pos1, pos2) < 0) | any(c(pos1, pos2)> ncol(x@ratings)))
		stop("pos1 and pos2 must be bigger than 1 and have number of elements as a maximum")
	x@elements[c(pos1, pos2)] <- x@elements[c(pos2, pos1)]
	x@ratings[,c(pos1, pos2),] <- x@ratings[,c(pos2, pos1),] 
  #	x <- e.swopElementPosition(x, pos1=pos1, pos2=pos2)
  #	x <- r.swopRatingsColumns(x, pos1=pos1, pos2=pos2)
	x
}
# @aliases   swape 
# swape <- swapElements 		# alias
# swopElements <- swapElements
# swopE <- swapElements
# swopElements(rg, 1,3)


#' Swap the position of two constructs in a grid.
#'
#' @param x       \code{repgrid} object.
#' @param pos1    Row number of first construct to be swapped (default=1).
#' @param pos2    Row number of second construct to be swapped (default=1).
#' @return        \code{repgrid} object
#' @export
#' @rdname swapConstructs
#' @examples \dontrun{
#'
#'    x <- randomGrid()
#'    swapConstructs(x, 1, 3)       # swap constructs 1 and 3
#'    swapConstructs(x, 1:2, 3:4)   # swap construct 1 with 3 and 2 with 4
#' }
#'
swapConstructs <- function(x, pos1=1, pos2=1){
  if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(any(c(pos1, pos2) < 0) | any(c(pos1, pos2)> nrow(x@ratings)))
		stop("pos1 and pos2 must be bigger than 1 and have number of constructs as a maximum")
	x@constructs[c(pos1, pos2)] <- x@constructs[c(pos2, pos1)] 
	x@ratings[c(pos1, pos2),,] <- x@ratings[c(pos2, pos1),,] 
  # x <- c.swopConstructPosition(x, pos1=pos1, pos2=pos2)
  # x <- r.swopRatingsRows(x, pos1=pos1, pos2=pos2)
	x
}
# @aliases swapc
# swapc <- swapConstructs   #alias
# swopConstructs <- swapConstructs
# swopC <- swapConstructs
#swopConstructs(rg, 1,2)


#' Reverse constructs / swaps construct poles
#'
#' Constructs are bipolar. They can be reversed without a change in meaning. The
#' function swaps the constructs poles and re-adjusts ratings accordingly, i.e.
#' it reversed selected constructs.
#'
#' @param x A `repgrid` object.
#' @param pos Row indexes of constructs to reverse..
#' @return A `repgrid` object with reversed constructs.
#' @note Please note that the scale of the rating grid has to be set in order
#'   to reverse constructs. If the scale is unknown no reversal occurs and an 
#'   error is raised.
#' @export
#' @rdname reverse
#' @md
#' @examples 
#'
#' x <- boeker
#' 
#' reverse(x)        # reverse all constructs
#' reverse(x, 1)     # reverse construct 1
#' reverse(x, 1:2)   # reverse constructs 1 and 2
#' 
#' # swapPoles will become deprecated, use reverse instead
#' swapPoles(x, 1)     # swap construct poles of construct
#'
swapPoles <- function(x, pos)
{
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  if (missing(pos))  
    pos <- seq_along(x@constructs)
  if (any(pos <= 0 | pos > getNoOfConstructs(x)))
	  stop("pos must contains values greater than 0 and equal or less than number of constructs.")
	if (identical(x@scale$min, NA) | identical(x@scale$min, NULL))
	  stop("A min value for the scale has to be defined in order to swap poles.",
	       "To define the scale use setScale(). For more info type ?setScale to the console.")
 	if (identical(x@scale$max, NA) | identical(x@scale$max, NULL))
 	  stop("A min value for the scale has to be defined in order to swap poles.",
 	       "To define the scale use setScale(). For more info type ?setScale to the console.")

  # swap names of poles  
	for (i in pos) {
  		tmp <- x@constructs[[i]]$leftpole
  		x@constructs[[i]]$leftpole <- x@constructs[[i]]$rightpole
  		x@constructs[[i]]$rightpole <- tmp	
	}
  # reverse ratings
	nc <- ncol(x@ratings[pos, , , drop = FALSE])  
	if (!nc == 0) {
	  x@ratings[pos, , ] <- x@scale$max - x@ratings[pos, , , drop = FALSE] + x@scale$min   # TODO: maybe swapping not correct for layers 2 and 3???
	}
	x
}


#' @export
#' @rdname reverse
reverse <- function(x, pos = 1L:nrow(x)) 
{
  stop_if_not_is_repgrid(x)
  stop_if_scale_not_defined(x)
  nc <- nrow(x)
  if (any(pos <= 0 | pos > nc))
    stop("all 'pos' must lie in the interval [1, ", nc, "]", call. = FALSE)
  
  # swap names of poles  
  lp <- leftpoles(x)[pos]
  rp <- rightpoles(x)[pos] 
  leftpoles(x)[pos] <- rp
  rightpoles(x)[pos] <- lp

  # reverse ratings
  sc <- getScale(x)
  R <- ratings(x)
  R[pos, ] <- sc["max"] - R[pos, ] + sc["min"]
  ratings(x) <- R
  x
}


#' Move construct or element in grid to the left, right, up or down.
#'
#' @param x     \code{repgrid} object.
#' @param pos   Row (column) number of construct (element) to be moved 
#'              leftwards, rightwards, upwards or downwards.
#'              The default is \code{0}. For indexes outside the range of
#'              the grid no moving is done.
#' @return      \code{repgrid} object.
#' @export
#' @aliases left right up down
#' @examples \dontrun{
#'    x <- randomGrid()
#'    left(x, 2)    # 2nd element to the left
#'    right(x, 1)   # 1st element to the right
#'    up(x, 2)      # 2nd construct upwards
#'    down(x, 1)    # 1st construct downwards
#' }
#' @rdname move
#'
left <- function(x, pos=0){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(!(pos<=1 | pos > getNoOfElements(x) | pos > ncol(x@ratings))){    # no moving if element is in first or last column
		x <- swapElements(x, pos, pos-1)
  }
	x
}

# @param x    repgrid object
# @param pos  column number of element to be moved to the right

#' Move element to the right
#'
#' Move element in grid to the right.
#'
#' @return \code{repgrid} object
#' @export
#' @rdname move
#'
right <- function(x, pos=0){
  if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(!(pos<0 | pos >= getNoOfElements(x) | pos >= ncol(x@ratings))){    # no moving if element is in first or last column
		x <- swapElements(x, pos, pos+1)	
  }
  # x <- e.moveElementRightwards(x, pos=pos)
  # x <- r.moveRatingsColumnRightwards(x, pos=pos)
	x
}


# @param x    repgrid object
# @param pos  row number of construct to be moved upwards

#' Move construct in grid upwards.
#'
#' @return \code{repgrid} object
#' @export
#' @rdname move
#'
up <- function(x, pos=0){
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'")
	if (!(pos<=1 | pos > getNoOfConstructs(x) | pos > nrow(x@ratings))){
		x <- swapConstructs(x, pos, pos - 1)
	}
	# x <- c.moveConstructUpwards(x, pos=pos)
  # x <- r.moveRatingsRowUpwards(x, pos=pos)
	x	
}

# @param x    repgrid object
# @param pos  row number of construct to be moved downwards

#' Move construct in grid downwards.
#'
#' @return \code{repgrid} object
#' @export
#' @rdname move
#'
down <- function(x, pos=0){
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'")
	if (!(pos < 1 | pos >= getNoOfConstructs(x) | pos >= nrow(x@ratings))){
		x <- swapConstructs(x, pos, pos + 1)
	}
  # x <- c.moveConstructDownwards(x, pos=pos)
  # x <- r.moveRatingsRowDownwards(x, pos=pos)
	x
}


#' Shift construct or element to first position.
#'
#' Shifts the whole grid vertically or horizontally so that the order remains
#' the same but the prompted element or construct appears in first position.
#' 
#' @param x   \code{repgrid} object.
#' @param c   Index of construct to be shifted to first position.
#' @param e   Index of element to be shifted to first position.
#' @return    \code{repgrid} object.
#' @export
#' @examples \dontrun{
#'
#'    # shift element 13: 'Ideal self' to first position
#'    shift(feixas2004, 13)    
#'
#'    x <- randomGrid(5,10)
#'    shift(x, 3, 5)
#' }
#'
shift <- function(x, c=1, e=1){
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'")
  if (e < 1 | c < 1)
    stop("Element or construct to be shifted to first position must have",
         " a positive index")
  ne <- length(x@elements)
  nc <- length(x@constructs)
  x[ring(1:nc + c - 1, nc), ring(1:ne + e - 1, ne)] 
}



#############################      CHANGE CONTENT      #################################

# rating <- function(x, scores=NA, rows=NA, cols=NA){
#   #x <- r.setRatings(x, scores=scores, rows=rows, cols=cols, layer=1)
# }



r.setRatings <- function(x, scores=NA, rows=NA, cols=NA, layer=1, ...) 
{
	if(!inherits(x, "repgrid")) 									# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
	if(is.list(scores) & !is.data.frame(scores))
		stop("scores must not be a list.")
	if(!(is.matrix(scores) | is.data.frame(scores) | is.vector(scores)))		# check if scores is matrix, dataframe or vector
		stop("scores must be matrix, dataframe or vector.")																
	if(is.data.frame(scores))
	 	scores <- as.matrix(scores)
	if(is.na(rows[1]) & length(rows)==1)
		rows <- 1:nrow(x@ratings)
	if(is.na(cols[1]) & length(cols)==1)
		cols <- 1:ncol(x@ratings)
	if(max(rows) > nrow(x@ratings))
		stop("number of constructs does not exists.")
	if(max(cols) > ncol(x@ratings)){
		stop("number of elements does not exists.")
	}
	x@ratings[rows, cols, layer] <- scores
	x
}
rating <- r.setRatings


#' clear ratings
#'
#' set certain ratings in grid to NA (unknown)
#'
#' @param x       repgrid object
#' @param rows    rows to be set NA
#' @param cols    columns to be set NA
#' @param layer   layer of ratings to be set NA. Usually not important for the user (default = 1).
#' @return \code{repgrid} object
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'    ####  TODO  ####
#' }
clearRatings <- function(x, rows=NA, cols=NA, layer=1) {
  x[rows, cols, layer] <- NA
	x
}
#clearRatings(x, 1, 1)


#' Add an element to an existing grid.
#'
#' @param x               \code{repgrid} object.
#' @param name            Name of the new element (character string).
#' @param scores          Numerical ratings for the new element column
#'                        (length must match number of constructs in the grid).
#' @param abbreviation    Abbreviation for element name.
#' @param status          Element status (not yet in use).
#' @param position        An integer at which column the element will be added.
#'                        TODO: Does not work properly yet.
#' @param side            Not yet in use.
#' @return                \code{repgrid} object
#' @export
#' @seealso               \code{\link{addConstruct}}
#' @examples \dontrun{
#'
#'    bell2010      
#'    addElement(bell2010, "new element", c(1,2,5,4,3,6,5,2,7))
#'
#' }
#'
addElement <- function(x, name = NA, scores = NA, abbreviation = NA, status = NA, position = NA, side="pre")
{
	if (length(name) > 1 | length(abbreviation) > 1 | length(status) > 1)
		stop("USERINFO: name, abbreviation and status must be of length one")
	if (is.na(position)) 
	  position <- ncol(x@ratings) + 1
	x <- e.addElements(x, name = name, abbreviation = abbreviation, 
		                 status = status, position = position, side = side) 			# basic element operation
	x <- r.makeNewElementColumn(x, pos = position) 								          # add column to ratings array

		# add scores/ratings	
	if (length(scores) != length(x@constructs) & 
	    !is.na(scores[1]) & length(scores) != 1) {
		warning("The number of ratings you entered do not match the number of constructs.")
		scores <- scores[1:length(x@constructs)]					                  # missing scores are filled up with NAs
	}
	if (length(x@constructs) > 0)
		x <- rating(x, scores, cols = position)
	return(x)
}
# x <- makeEmptyRepgrid()
# x <- addElement(x)


#' Add a new average element
#'
#' A new element is added to the grid. The scores are the arithmetic means
#' across all selected elements.
#'
#' @param x A `repgrid` object.
#' @param name Name of new element.
#' @param i Indexes of elements to be averaged across. Negative indexes can be
#'   used to exclude elements from the complete set. Duplicate indexes are
#'   allowed but a warning is issued.
#' @param digits Digits to round mean value to. By default no rounding is used
#'   (`digits = Inf`). Use `digits = 0` to round to nearest integer, i.e. only
#'   using original integer rating scores.
#' @return A `repgrid` object with additional new element.
#' @export
#' @md
#' @examples
#' addAvgElement(feixas2004, "others", i = 2:12)
#' addAvgElement(feixas2004, "others", i = 2:12, digits = 0) # integers
#' 
#' # exluding elements via negative indexes
#' addAvgElement(feixas2004, "others", i = c(-1,-13))
#' 
#' 
addAvgElement <- function(x, name = "avg", i, digits = Inf) 
{
  if (!is.repgrid(x))
    stop("'x' must be a repgrid object", call. = FALSE)
  if (name %in% elements(x))
    stop("element name '", name, "' already exists,", call. = FALSE)
  nc <- ncol(x)
  if (!is.numeric(i) || !length(i) >= 1)  
    stop("'i' must be a numeric vector with at least one entry", call. = FALSE)
  if (!all(abs(i) >= 1) && all(abs(i) <= nc))  
    stop("'i' must range between 1 and ", nc, call. = FALSE)
  if (any(i < 0) && any(i > 0))
    stop("It is not allowed to mix positive and negative indexes", call. = FALSE)
  if (sum(duplicated(abs(i))) > 0)
    warning("duplicate indexes detected in 'i'", call. = FALSE)
  
  # convert negative indexes
  if (all(i < 0))
    i <- setdiff(1L:nc, abs(i))
  
  R <- ratings(x)
  mean_ratings <- rowMeans(R[, i, drop = FALSE])
  mean_ratings <- round(mean_ratings, digits = digits)
  x <- addElement(x, name = name, scores = mean_ratings)
  x
}


#' Add a new construct to an existing grid object.
#'
#' @param x               \code{repgrid} object.
#' @param l.name          Name of the left pole (character string).
#' @param r.name          Name of the right pole (character string).
#' @param scores          Numerical ratings for the new construct row
#'                        (length must match number of elements in the grid).
#' @param l.preferred     Is the left one the preferred pole? (logical).
#' @param r.preferred     Is the right one the preferred pole? (logical).
#' @param l.emerged       Is the left one the emergent pole? (logical).
#' @param r.emerged       Is the right one the emergent pole? (logical).
#' @param position        An integer at which row the construct will be added.
#'                        TODO. Does not work properly.
#' @param side            Not yet in use.
#' @return                \code{repgrid} object.
#' @export
#' @seealso               \code{\link{addElement}}
#'
#' @examples \dontrun{
#'
#'    # show grid
#'    bell2010                                          
#'    addConstruct(bell2010, "left pole", "pole right", c(3,1,3,2,5,4,6,3,7,1))
#'
#' }
#'
addConstruct <- function(x, l.name = NA, r.name = NA, scores = NA, 
	                        l.preferred = NA, r.preferred = NA, 
	                        l.emerged = NA, r.emerged = NA,
						              position = NA, side = "pre"){
	if (is.na(position)) position <- length(x@constructs) + 1
	x <- c_addConstruct(x, l.name = l.name, l.preferred = l.preferred, l.emerged = l.emerged, 
						   r.name = r.name, r.preferred = r.preferred, r.emerged = r.emerged, 
						   position = position, side = side)
	x <- r_makeNewConstructRow(x, pos = position)
	# add scores/ratings	
	if (length(scores) != length(x@elements) & !is.na(scores[1]) & length(scores) != 1) {
		warning("The number of ratings you entered do not match the number of elements.")
		scores <- scores[1:length(x@elements)]					# missing scores are filled up with NAs
	}
	if (length(x@elements) > 0)
		x <- rating(x, scores, rows = position)			
	return(x)
}
# x <- makeEmptyRepgrid()
# x <- addConstruct(x)



#### RENAMING ####


#' Set the attributes of an element
#'
#' Set the attributes of an element i.e. name, abbreviation, status etc.
#'
#' @param x         \code{repgrid} object.
#' @param pos       Column number of element in the grid whose attributes 
#'                  are changed.
#' @param name      New element name (optional).
#' @param abb       Abbreviation of element name (optional).
#' @param status    Status of element (e.g. ideal etc.) (optional).
#' @return          \code{repgrid} object
#'
#' @note            Currently the main purpose is to change element names. 
#'                  Future implementations will allow to set further attributes.
#' @export
#' @seealso         \code{\link{setConstructAttr}}
#' @examples \dontrun{
#'    
#'    x <- setElementAttr(boeker, 1, "new name")   # change name of first element
#'    x
#' }
#'
setElementAttr <- function(x, pos, name, abb, status){
  e <- x@elements[[pos]]
  if (! missing(name))
    e$name <- name
  if (! missing(abb))
      e$abbreviation <- abb
  if (! missing(status))
      e$status <- status
  x@elements[pos] <- list(e)
  x
}
# setElementAttr(x, 1)          # no action
# setElementAttr(x, 1, "test")  # new name
# setElementAttr(x, 1, abb="test")  # new abbreviation
# setElementAttr(x, 1, status="ideal")  # new status
# setElementAttr(x, 1, "new name", 
#                 "new abbreviation", "new status")  # all new


#' Set the attributes of a construct
#'
#' Set the attributes of a construct i.e. name, abbreviation, status etc.
#'
#' @param x               \code{repgrid} object.
#' @param pos             Row number of construct in the grid to be changed
#' @param l.name          Name of the left pole (string) (optional).
#' @param r.name          Name of the right pole (string) (optional).
#' @param l.preferred     Logical. Is the left one the preferred pole? (optional).
#' @param r.preferred     Logical. Is the right one the preferred pole? (optional).
#' @param l.emerged       Logical. Is the left one the emergent pole?  (optional). 
#' @param r.emerged       Logical. Is the right one the emergent pole? (optional).
#' @return                \code{repgrid} object
#'
#' @export
#' @seealso         \code{\link{setElementAttr}}
#' @examples \dontrun{
#'
#'    x <- setConstructAttr(bell2010, 1, 
#'                  "new left pole", "new right pole")
#'    x
#' }
#'
setConstructAttr <- function(x, pos, l.name, r.name, l.preferred, r.preferred, 
                              l.emerged, r.emerged){
	con <- x@constructs[[1]]
	if (! missing(l.name))
    con$leftpole$name <- l.name
  if (! missing(l.preferred))
    con$leftpole$preffered <- l.preferred
  if (! missing(l.emerged))
    con$leftpole$emerged <- l.emerged
  if (! missing(r.name))
    con$rightpole$name <- r.name
  if (! missing(r.preferred))
    con$rightpole$preffered <- r.preferred
  if (! missing(r.emerged))
    con$rightpole$emerged <- r.emerged   
  x@constructs[pos] <- list(con)
  x
}

# setConstructAttr(x, 1, l.n="halle")



# MAYBE OBSOLETE as setConstructAttr does the same. 
# modifyConstructs() allows to change the properties of a construct (left and 
# right pole as well as preferred and emergent property). By default the new 
# values get added to the old ones, i.e. specifying l.name only overwrites 
# l.name. If you want to reset all properties use replace=TRUE. Default 
# is NA for all properties.

#' modify a construct
#'
#' change the attributes of a construct
#'
#' @param x               repgrid object
#' @param pos             row number of construct in the grid to be changed
#' @param l.name          (optional) name of the left pole (string)
#' @param r.name          (optional) name of the right pole (string)
#' @param l.preferred     (optional) is the left one the preferred pole? (logical)
#' @param r.preferred     (optional) is the right one the preferred pole? (logical)
#' @param l.emerged       (optional) is the left one the emergent pole? (logical)
#' @param r.emerged       (optional) is the right one the emergent pole? (logical)
#' @param replace         should the sttributes be replaced if NA is provided?
#' @return \code{repgrid} object
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'    ####  TODO  ####
#' }
#'
modifyConstruct <- function(x, pos, l.name=NA, l.preferred=NA, l.emerged=NA, 
									             r.name=NA, r.preferred=NA, r.emerged=NA,
									             replace=FALSE){
	if(!inherits(x, "repgrid"))                   # check if x is repgrid object
		stop("Object x must be of class 'repgrid'")
	cs <- c_makeNewConstruct(x=NULL , 
	                         l.name=l.name, 
	                         l.preferred=l.preferred, 
	                         l.emerged=l.emerged, 
	                         r.name=r.name, 
	                         r.preferred=r.preferred, 
	                         r.emerged=r.emerged)
	if(replace){
		x@constructs[pos] <- list(modifyList(x@constructs[[pos]], cs))
	} else x@constructs[pos] <- list(modifyListNA(x@constructs[[pos]], cs))
	x
}


#' modifyElement
#'
#' change the attributes of an element i.e. name, abbreviation, status etc.
#'
#' @param x             repgrid object
#' @param pos           column number of element in the grid whose attributes are changed
#' @param name          (optional) new name
#' @param abbreviation  (optional) abbreviation of element name
#' @param status        (optional) status of element (e.g. ideal etc.)
#' @param replace       logical. whether to overwrite current settings if NA provided
#' @return \code{repgrid} object
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'    ####  TODO  ####
#' }
modifyElement <- function(x, pos, name=NA, abbreviation=NA, status=NA, 
                           replace=FALSE){
	if(!inherits(x, "repgrid")) 							# check if x is repgrid object
		stop("Object x must be of class 'repgrid'")
	e <- e.makeNewElement(x=NULL , name=name, 
	                      abbreviation=abbreviation, status=status)
  if(replace){
		x@elements[pos] <- list(modifyList(x@elements[[pos]], e))
	} else x@elements[pos] <- list(modifyListNA(x@elements[[pos]], e))
	x
}
#x <- makeEmptyRepgrid()
#x <- addElements(x, c("Element 1", "Element 2"))
#x <- modifyElement(x, pos=2, name="test")




 
#' Print scale range information to the console.
#'
#' @param x     \code{repgrid} object.
#' @return      \code{NULL}.
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'    showScale(raeithel)
#'    showScale(bell2010)
#' }
#'
showScale <- function(x){
  cat("\nSCALE INFO:\n")
  if(!is.null(x@scale$min) & !is.null(x@scale$max)) {
    cat("The grid is rated on a scale from", x@scale$min, 
  		  "(left pole) to", x@scale$max, "(right pole)\n")#,
  		#  "using steps of", x@scale$step, "\n")
  } else {
    cat("warning: the scale for this grid is not defined.",
        "Certain functions rely on the scale definition.", 
        "To define the scale use setScale().",
        "For more info type ?setScale to the console.\n")
  }
  invisible(NULL)
}
#showScale(x)


# the slot coupled can be influenced
# If a grid is changed from couled to uncoupled, the data is double but 
# with reflected scales. A scale range has to be defined for that operations
setCoupled <- function(x, coupled=TRUE){
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
	  stop("Object x must be of class 'repgrid'")
	if (isTRUE(x@coupled) & !coupled) {
	  x <- doubleEntry(x)
	}
	x
}

# x <- bell2010
# x <- setCoupled(x)




#' showMeta
#'
#' prints meta information about the grid to the console (id, name of interviewee etc.)
#'
#' @param x     repgrid object
#' @return \code{NULL} 
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'    ####  TODO  ####
#' }
#'
showMeta <- function(x){
  cat("\nMETA DATA:\n")
  if(!is.null(x@meta$type)) 
    cat("Grid type: ", x@meta$type, "\n")		  # print Meta data
  if(!is.null(x@meta$id)) 
    cat("Interview id: ", x@meta$id, "\n")		# print Meta data
  if(!is.null(x@meta$name)) 
    cat("Name of interview partner: ", x@meta$name, "\n")
  cat("Number of constructs: ", length(x@constructs), "\n")
  cat("Number of elements: ", length(x@elements), "\n")
}

#showMeta(x)



#' Make a new repgrid object. 
#'
#' The function creates a \code{repgrid}
#' object from scratch. A number of parameters have to be defined in order to
#' make a new grid (see parameters).
#'
#' @param args    Arguments needed for the construction of the grid (list).
#'                These include \code{name} followed by a vector containing 
#'                the element names. \code{l.name} followed by a vector with 
#'                the left construct poles. \code{r.name} followed by a 
#'                vector with the right construct poles. \code{scores} followed
#'                by a vector containing the rating scores row wise.
#' @return        \code{NULL} 
#' @export
#' @examples \dontrun{
#'
#'    # make list object containing the arguments
#'    args <- list( name=c("element_1", "element_2", "element_3", "element_4"),
#'		              l.name=c("left_1", "left_2", "left_3"),
#'		  	          r.name=c("right_1", "right_2", "right_3"),
#'		  	          scores=c(	1,0,1,0,
#'						                1,1,1,0,
#'						                1,0,1,0	) )
#'    # make grid object
#'    x <- makeRepgrid(args)
#'    x
#' }
#'
makeRepgrid <- function(args)
{
  x <- makeEmptyRepgrid()	
  l <- c(list(x=x), args)								# make a new repgrid object
  x <- do.call(e.setElements, l)
  l <- c(list(x=x), args)								# make a new repgrid object
  x <- do.call(c.setConstructs, l)
  x <- initRatingArray(x)								# initialize rating array
  l <- c(list(x=x), args)								# make a new repgrid object	
  x[ , ] <- matrix(args$scores, ncol=getNoOfElements(x), byrow=T)  # to fill matrix rowwise
  #x <- do.call(r.setRatings, l)        # old version
  l <- c(list(x=x), args)								# make a new repgrid object	
  x <- do.call(rg.setCoupled, l)        # if no coupled argument then coupled=TRUE
  l <- c(list(x=x), args)								# make a new repgrid object	
  x <- do.call(setScale, l)             # set scale if min and max arg is provided
  x
}
# args <- list( name=c("element_1", "element_2", "element_3", "element_4"),
#               l.name=c("left_1", "left_2", "left_3"),
#               r.name=c("right_1", "right_2", "right_3"),
#               scores=c( 1,0,1,0,
#                         1,1,1,0,
#                         1,0,1,0 ),
#               min=0, max=1, coupled=T)
# x <- makeRepgrid(args)
# x <- setScale(x, 0,1)



#' Concatenate the constructs of two grids. 
#' 
#' I.e. the constructs are combined to form one long grid.
#' This function can be used in order to analyze multiple grids
#' as one 'big grid' (eg. Slater, 1977, chap. 11).
#'
#' @param x       \code{repgrid} object
#' @param y       \code{repgrid} object
#' @param match   If the elements do not have the same order they
#'                are reordered to match the element order of the first grid 'x'
#'                (if \code{test=TRUE}, default). If set to FALSE an error occurs
#'                if the element order is not identical in both grids.
#' @param index   TODO. Logical (default \code{TRUE}). Whether to add an index at the end
#'                of each construct name so it remains clear from which grid each 
#'                construct came.                
#'
#' @return \code{repgrid} object
#'
#' @references  Slater, P. (1977). \emph{The measurement of intrapersonal space 
#'              by grid technique}. London: Wiley.
#'
#' @export
#' @keywords    internal
#' @examples \dontrun{
#'
#'    a <- randomGrid()
#'    b <- randomGrid()
#'    b@@elements <- rev(a@@elements)   # reverse elements
#'    bindConstructs(a, b)
#'  
#'    bindConstructs(a, b, m=F)       # no binding
#' }
#'
bind <- function(x, y, match=TRUE, index=TRUE)
{
  if (!inherits(x, "repgrid") | !inherits(y, "repgrid")) 	# check if x is repgrid object
		stop("object x and y must be of class 'repgrid'", call. = FALSE)
	if (getNoOfElements(x) != getNoOfElements(y))           # check if grid has same number of columns
	  stop("grids must have the same number of elements", call. = FALSE)
  if (any(getScale(x) != getScale(y)))
    stop("concatenated grids must have identical scale ranges", call. = FALSE)
  names.x <- elements(x)
  names.y <- elements(y)
  if (!all(names.x %in% names.y))
    stop("grids must have the same set of elements", call. = FALSE)
  
  if (match & !identical(names.x, names.y)){  
    #y <- y[ ,orderByString(names.x, names.y)]
    reorder.index.y <- match(names.x, names.y)  # reorder elements of y by elements of x
    y <- y[ , reorder.index.y]
  } else if (!match & !identical(names.x, names.y)){
    stop("elements are the same but dop not have the same order.",
         "choose reorder=TRUE if you want to allow matching of element positions")
  }
  x <- x[ , ]  # to counteract that decoupled arrays are dropped when using [, ]
  y <- y[ , ]
  res <- x
  res@ratings <- abind(x@ratings[ , , , drop=FALSE],  
                       y@ratings[ , , , drop=FALSE], along=1)
  res@constructs <- c(x@constructs, y@constructs)
  res
}


#' Concatenate the constructs of two or more grids. 
#' 
#' I.e. the constructs are combined to form one long grid.
#' The girds must have the same set of elements and an identical 
#' scale range. The order of the elements may differ.
#' 
#' This function can be used in order to analyze multiple grids
#' as one 'big grid' (eg. Slater, 1977, chap. 11).
#'
#' @param ...     One or more repgrid objects or a list containing
#'                \code{repgrid} object.
#' @param index   TODO. Logical (default \code{TRUE}). Whether to add an index at the end
#'                of each construct name so it remains clear from which grid each 
#'                construct came.                
#'
#' @return        \code{repgrid} object with concatenated constructs.
#'
#' @references  Slater, P. (1977). \emph{The measurement of intrapersonal space 
#'              by grid technique}. London: Wiley.
#' @export
#' @examples 
#'
#'  a <- randomGrid()
#'  b <- randomGrid()
#'  b@@elements <- rev(a@@elements)   # reverse elements
#'  bindConstructs(a, b)
#'  bindConstructs(a, b, a)
#'  
#'  # using lists of repgrid objects 
#'  bindConstructs(a, list(a, b))
#'
bindConstructs <- function(..., index=FALSE)
{
  dots <- list(...) 
  dots <- unlist(dots)        # in case list of repgrid objects are supplied
  is.grid <- sapply(dots, function(x) inherits(x, "repgrid"))
  Reduce("bind", dots[is.grid])
}


# @aliases +,repgrid,repgrid-method
# @docType methods

#' Concatenate repgrid objects.
#' 
#' Simple concatenation of repgrid objects or list containing
#' repgrid objects using the '+' operator.
#'
#' Methods for \code{"+"} function. 
#' @param e1,e2  A \code{repgrid} object.
#' @rdname ops-methods
#' @include repgrid.r
#' @export
#' @examples 
#' 
#' x <- bell2010
#' x + x
#' x + list(x,x)
#' list(x,x) + x 
#'
setMethod("+", signature(e1="repgrid", e2="repgrid"),
    function(e1, e2) bindConstructs(e1, e2))


#' @docType methods
#' @aliases +,list,repgrid-method
#' @rdname ops-methods
#' 
setMethod("+", signature(e1="list", e2="repgrid"),
    function(e1, e2) {          
      bindConstructs(e1, e2)
    })


#' @docType methods
#' @aliases +,repgrid,list-method
#' @rdname ops-methods
#'
setMethod("+", signature(e1="repgrid", e2="list"),
    function(e1, e2) {          
      bindConstructs(e1, e2)
    })


#' Join the constructs of a grid with the same reversed constructs.
#'
#'
#' @param x \code{repgrid} object
#' @return \code{repgrid} object
#'
#' @export
#' @keywords internal
#' @examples \dontrun{
#'
#'      data(bell2010)
#'      doubleEntry(bell2010)
#' }
#'
doubleEntry <- function(x){
  bindConstructs(x, swapPoles(x))
}


#' Return size of a grid. 
#'
#' \code{dim} returns a numeric vector of length
#' two containing the number of constructs and elements.
#'
#' @param x     \code{repgrid} object.
#' @return      Numeric vector of length two with the number of 
#'              constructs and elements.
#' @export
#' @keywords    internal
#' @method      dim repgrid
#' @seealso     \code{\link{getNoOfConstructs}};   \code{\link{getNoOfElements}}
#' @examples \dontrun{
#'
#'      dim(bell2010)
#'
#' }
#'
dim.repgrid <- function(x){
  if (!inherits(x, "repgrid")) 	# check if x is repgrid object
 		stop("object x and y must be of class 'repgrid'")
  c(constructs=getNoOfConstructs(x), elements=getNoOfElements(x))
}


# set status coupled equals TRIE or FALSE. Depending on the setting,
# certain functions will work differently
#
rg.setCoupled <- function(x, coupled=TRUE, ...){
  if(!inherits(x, "repgrid")) 											# check if x is repgrid object
		stop("Object x must be of class 'repgrid'.")
  x@coupled <- coupled
  x
}


#' decouple a grid
#'
#' @param x     repgrid object
#' @export
#' @keywords internal
#'
decouple <- function(x){
  if (x@coupled) {
      x <- doubleEntry(x)
      x@coupled <- FALSE
  }
  x
}


#' Invert construct and element order
#' 
#' @param x  A `repgrid` object.
#' @param what A string or numeric to indicate if constructs (`"C"`, `1`) or
#'   elements (`"C"`, `1`), or both (`"CE"`, `12`) should be reversed.
#' @param ... Ignored.
#' @export
#' @rdname reorder
#' @md
#' @examples 
#'  
#' # invert order of constructs
#' reorder(boeker, "C")
#' reorder(boeker, 1)
#' 
#' # invert order of elements
#' reorder(boeker, "E")
#' reorder(boeker, 2)
#' 
#' # invert both (default)
#' reorder(boeker)
#' reorder(boeker, "CE")
#' reorder(boeker, 12)
#' 
#' # not reordering
#' reorder(boeker, NA)
#' 
reorder.repgrid <- function(x, what = "CE", ...) 
{
  if (!is.repgrid(x))
    stop("Object 'x' must be of class 'repgrid'", call. = FALSE)
  if (is.null(what) || is.na(what))
    return(x)
  what <- toupper(as.character(what))
  if (str_detect(what, "C|1"))  
    x <- x[rev(seq_len(nrow(x))), ]
  if (str_detect(what, "E|2"))  
    x <- x[, rev(seq_len(ncol(x)))]
  x
}






