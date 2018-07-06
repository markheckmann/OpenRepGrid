
#' Get or replace construct poles 
#' 
#' Allows to get and set construct poles. 
#' Replaces the older functions \code{getConstructNames}, \code{getConstructNames2},
#' and \code{eNames} which are deprecated.
#' 
#' @param  x A repgrid object.
#' @rdname constructs
#' @export
#' @examples 
#' 
#' ## get construct poles
#' constructs(x)   # both left and right poles
#' leftpoles(x)    # left poles only
#' rightpoles(x)
#' 
#' ## replace construct poles
#' l <- leftpoles(x)
#' leftpoles(x) <- sample(l)             # brind poles into random order
#' leftpoles(x)[1] <- "new left pole 1"  # replace name of first left pole
#' 
#' # replace left poles of constructs 1 and 3
#' leftpoles(x)[c(1,3)] <- c("new left pole 1", "new left pole 3")
#'  
constructs <- function(x)
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  # get left and right pole name properties from each construct
  data.frame(
    leftpole = leftpoles(x),
    rightpole = rightpoles(x), 
    stringsAsFactors = FALSE
  )
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


#' @param value Character vector of poles.
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



#' @param value Character vector of poles.
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


#' @rdname constructs
#' @export
`constructs<-` <- function(x, i, j, value) 
{
  # check if x is a repgrid object
  if (!inherits(x, "repgrid")) 
    stop("Object x must be of class 'repgrid'.")
  
  d <- constructs(x)
  d[i, j] <- value
  
  # get element names and replace one or more
  leftpoles(x) <- d$leftpole
  rightpoles(x) <- d$rightpole

  x
}
# 
constructs(x)[1,1] <- "xxx"
# constructs(x)[1,"rightpole"] <- "kjksjkjskd"


## 1.
# simple replacement function

foo <- function(x) x

`foo<-` <- function(x, position, value) {
  x[position] <- value
  x
}

v <- 1:3
foo(v)[2] <- NA
v

## 2. 
# now with two indexes, with m being a matrix
foo2 <- function(x) x

`foo2<-` <- function(x, i, j, value) {
  x[i, j] <- value
  x
}

# works for matrix
m <- matrix(1,3,3)
foo2(m)[2,3] <- 10
m

# .. and dataframes
m <- as.data.frame(m)
foo2(m)[2,3] <- NA
m


## 3.
# now for a list things go south
foo3 <- function(x) x

`foo3<-` <- function(x, i, j, value) {
  x[j][i] <- value
  x
}

l <- list(a=1:3, b=3:1)
foo3(l)[1] <- 1
# class(l) <- c("matrix", class(l))

foo3(l)[1,1] <- 10
l




