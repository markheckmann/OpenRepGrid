# calculate a biplot a as devised by Gabriel, 1971.
# TODO: under construction
#
# biplotCoords <- function(x, g=1, h=1-g, center=TRUE)
# {
#   if (!inherits(x, "repgrid"))                    # check if x is repgrid object
#     stop("Object x must be of class 'repgrid'")
#   
#   X <- getRatingLayer(x, 1)
#   midpoint <- x@scale$min + (x@scale$max - x@scale$min)/2
#   X <- X - midpoint                 # centering by midpoint of scale to move to origin
#   X <- matrix(X, ncol=ncol(X))
# 
#   z <- svd(X)         # X = U*L*t(A)  # svd nomenclature used
#   U <- z$u
#   A <- z$v
#   L <- z$d
# 
#   # making the biplot coordinates
# 
#   C.mat <- U %*% diag(L^g)     # construct matrix:  U * L^g
#   E.mat <- A %*% diag(L^h)     # elements matrix    A * L^h
# 
#   list(constructs.coords=C.mat,
#       elements.coords=E.mat,
#       singular.values=L)
# }


#' map arbitrary numeric vector to a given range of values
#'
#' From a given numeric vector z the range is determined and the values are 
#' linearly mapped onto the interval given by \code{val.range}. This 
#' funtion can be used in order to map arbitrary vectors to a given
#' range of values.
#'
#' @param  z          numeric vector.
#' @param  val.range  numeric vector of lengths two (default \code{c(.5, 1)}).
#' @return numeric vector
#' @keywords internal
#' @export
#'
mapCoordinatesToValue <- function(z, val.range=c(.5, 1)) {
	z.range <- c(min(z, na.rm=T), max(z, na.rm=T))
	slope <-  diff(val.range) / diff(z.range)
	int <- val.range[1] - z.range[1] * slope
	vals <- int + slope * z
  vals
}


#' determine color values according to a given range of values
#'
#' From a given numeric vector z the range is determined and the values are 
#' linearly mapped onto the interval given by \code{val.range}. Then 
#' a color ramp using the colors given by \code{color} is created and 
#' the mapped values are transformed into hex color values. 
#'
#' @param  z          numeric vector.
#' @param  color      vector of length two giving color values \code{c("white", "black")}.
#' @param  val.range  numeric vector of lengths two (default \code{c(.2, .8)}).
#' @return numeric vector
#'
#' @keywords internal
#' @export
#'
mapCoordinatesToColor <- function(z, colors=c("white", "black"), val.range=c(.2,.8)){
  colorRamp <- makeStandardRangeColorRamp(colors)
  vals <- mapCoordinatesToValue(z, val.range)
  colorRamp(unlist(vals))     # unlist in case z comes as a data frame column
}


#' coordinates of a sourrounding rectangle in direction of a given vector 
#'
#' An arbitrary numeric vector in 2D is to be extented so it will 
#' end on the borders of a sourrounding rectangle of a given size.
#' Currently the vector is supposed to start in the origin \code{c(0,0)}.
#'
#' @param x      numeric vector of x coordinates x coordinates. 
#' @param y      numeric vector of y coordinates x coordinates. 
#' @param xmax   maximal x value for sourrounding rectangle (default is \code{1}).
#' @param ymax   maximal y value for sourrounding rectangle (default is \code{1}).
#' @param cx     center of retangle in x direction (not yet supported).
#' @param cy     center of retangle in x direction (not yet supported).
#' 
#' @return       a \code{dataframe} containing the x and y coordinates for the 
#'               extended vectors.
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#'   calcCoordsBorders(1:10, 10:1)
#' 
#'   x <- c(-100:0, 0:100, 100:0, 0:-100)/10
#'   y <- c(0:100, 100:0, -(0:100), -(100:0))/10
#'   xy1 <- calcCoordsBorders(x, y)
#'   xy2 <- calcCoordsBorders(x, y, xm=1.2, ym=1.2)
#'   plot(xy2[,1], xy2[,2], type="n")
#'   segments(xy1[,1],xy1[,2],xy2[,1], xy2[,2])
#' }
#'
calcCoordsBorders <- function(x, y, xmax=1, ymax=1, cx=0, cy=0)
{
  is.lr.part <- abs(x*ymax/xmax) >= abs(y)        # which are left and right parts

  # left and right part             
  sign.x <- sign(x)             # positive or negative value    
  sign.x[sign.x == 0] <- 1      # zeros in posistive direction
  a.lr <- xmax * sign(x)        # x is fix on the left and right side
  b.lr <- y/x * a.lr            
  
  # upper and lower part
  sign.y <- sign(y)
  sign.y[sign.y == 0] <- 1
  b.ul <- ymax * sign(y)
  a.ul <- x/y * b.ul
  
  a.lr[is.nan(a.lr)] <- 0       # in case one of x or y is zero Inf results ans subsequently NaN
  b.lr[is.nan(b.lr)] <- 0
  a.ul[is.nan(a.ul)] <- 0
  b.ul[is.nan(b.ul)] <- 0  
  
  # join both parts
  b <- (b.ul * !is.lr.part) + (b.lr * is.lr.part)
  a <- (a.ul * !is.lr.part) + (a.lr * is.lr.part)
  a[abs(a) > xmax] <- (xmax * sign(a))[abs(a) > xmax]
  b[abs(b) > ymax] <- (ymax * sign(b))[abs(b) > ymax]
  
  data.frame(x=a, y=b)
}



# calculate the coords for the label rectangle
#
# TODO: supply x.ext in mm and convert to usr coords
#
# @param  xy        \code{dataframe} with x and y coords.
# @param  labels    vector of strings.
# @param  cex       vector of cex values (default is \code{.7}).
# @param  x.ext     scalar giving the horizontal margin 
#                   of the rectangle in NDC coordinates
#                   (default is \code{.02}).
# @param  y.ext     scalar giving the vertical margin 
#                   of the rectangle in NDC coordinates
#                   (default is \code{.02}).
# @return \code{dataframe} with coordinates for the lower left and 
#         upper right rectangle borders (\code{x0, y0, x1, y1}).
#
calcRectanglesCoordsForLabels <- function(xy, labels, cex=.7, 
                                          x.ext=.02, y.ext=.02){
  if (length(cex) == 1)
    cex <- rep(cex, dim(xy)[1])
  
  heights <- vector()
  widths <- vector()
  
  for (i in 1:dim(xy)[1]){
    heights[i] <- strheight(labels[i], cex=cex[i])   # determine necessary height for text 
    widths[i] <- strwidth(labels[i], cex=cex[i])     # determine necessary width for text 
  }
  # make adj adjustements
  leftSide <- xy[, 1] < 0
  labelsBorders <- data.frame(x0= xy[, 1] - (widths * leftSide), 
                              y0= xy[, 2] - heights/2, 
                              x1= xy[, 1] + (widths * !leftSide), 
                              y1= xy[, 2] + heights/2)
  # extend borders for neater look
  labelsBorders$x0 <- labelsBorders$x0 - x.ext
  labelsBorders$y0 <- labelsBorders$y0 - y.ext
  labelsBorders$x1 <- labelsBorders$x1 + x.ext
  labelsBorders$y1 <- labelsBorders$y1 + y.ext
  
  labelsBorders
}


#' detect if two rectangles overlap 
#'
#' do rectangle coordinates overlap in x AND y?
#'
#' @param  a   vector with four coordinates \code{c(x0,y0,x1,y1)}.
#' @param  b   vector with four coordinates \code{c(x0,y0,x1,y1)}.
#' @return     \code{logical}. TRUE if rectangles overlap.
#'
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#'   #overlap in x and y
#'   a <- c(0,0,2,2)
#'   b <- c(1,1,4,3)
#'   plot(c(a,b), c(a,b), type="n")
#'   rect(a[1], a[2], a[3], a[4])
#'   rect(b[1], b[2], b[3], b[4])
#'   doRectanglesOverlap(a,b)
#' 
#'   # b contained in a vertically
#'   a <- c(5,0,20,20)
#'   b <- c(0, 5,15,15)
#'   plot(c(a,b), c(a,b), type="n")
#'   rect(a[1], a[2], a[3], a[4])
#'   rect(b[1], b[2], b[3], b[4])
#'   doRectanglesOverlap(a,b)
#' 
#'   # overlap only in y
#'   a <- c(0,0,2,2)
#'   b <- c(2.1,1,4,3)
#'   plot(c(a,b), c(a,b), type="n")
#'   rect(a[1], a[2], a[3], a[4])
#'   rect(b[1], b[2], b[3], b[4])
#'   doRectanglesOverlap(a,b)
#' }
#'
doRectanglesOverlap <- function(a, b, margin=0){
  overlap1D <- function(a0, a1, b0, b1){ # overlap if one of four conditions is satisfied
    (a0 <= b1 & b1 <= a1) |     # b overlaps at bottom
    (a0 <= b0 & b0 <= a1) |     # b overlaps at top
    (a0 >= b0 & a1 <= b1) |     # b overlaps at bottom and top
    (a0 <= b0 & a1 >= b1)       # b contained within a
  }
  overlap.x <- overlap1D(a[1], a[3], b[1], b[3])  # overlap in x ?
  overlap.y <- overlap1D(a[2], a[4], b[2], b[4])  # overlap in y ?
  as.logical(overlap.x & overlap.y)               # overlap in x and y, strip off vector names ?
}


# calculate angle between vector and x-y plane
# a   vector
# n   plane normal vector
degreesBetweenVectorAndPlane <- function(a, n){
    rad <- asin( abs(n %*% a) / 
                (sum(n^2)^.5 * sum(a^2)^.5))
    rad * 180/pi                # convert from radians to degrees
}





biplotSimple <- function(x, unity=T, dim=1:2, ...){
  #unity <- T
  # dim <- 1:2
  par(mar=c(1,1,1,1))
  d1 <- dim[1]
  d2 <- dim[2]
  
  #x <- calcBiplotCoords(x, ...)
  cnames <- getConstructNames(x)
  E <- x@calcs$biplot$el
  C <- x@calcs$biplot$con
  X <- x@calcs$biplot$X

  max.e <- max(abs(E[ ,dim]))
  max.c <- max(abs(C[ ,dim]))
  mv <- max(max.e, max.c)
  if (unity){
    se <- 1/max.e     # scale to unity to make E and C same size
    sc <- 1/max.c
    mv <- 1
  } else {
    se <- 1
    sc <- 1
  }
  Cu <- C * sc
  Eu <- E * se
  
  # make biplot
  plot(0, xlim=c(-mv, mv), ylim=c(-mv, mv), type="n", asp=1,
        xaxt="n", yaxt="n")
  abline(v=0, h=0, col="grey")

  arrows(0,0, -Cu[ ,d1], -Cu[ ,d2], len=.05, col=grey(.6), lty=1)              # plot left poles
  text(-Cu[ ,d1], -Cu[ ,d2], cnames[,1], pos=1, cex=.6, col=grey(.6))
  arrows(0,0, Cu[ ,d1], Cu[ ,d2], len=.05, col=grey(.6), lty=3)               # plot right poles
  text(Cu[ ,d1], Cu[ ,d2], cnames[,2], pos=1, cex=.6, col=grey(.6))
  
  points(Eu[, dim],pch=15)                                 # plot elements
  text(Eu[, dim], labels=rownames(Eu), cex=.7, pos=2)      # label elements  
  invisible(x)
}

# library(xtable)
# x <- biplotSimple(raeithel, dim=1:2, g=1, h=1, col.act=c(1,2,3,5,10,12))
# ssq.table <- ssq(x)
# #ssq.table[ssq.table < 10] <- NA
# res <- xtable(round(ssq.table, 1), digits=1, 
#               align=c("l", rep("r", ncol(ssq.table))), caption="Percentage of element's SSQ explained")
# print(res, table.placement="H", hline.after=c(-1,0,nrow(ssq.table)-1, nrow(ssq.table)))


#' Prepare dataframe passed to drawing functions for biplots
#'
#' Data frame contins the variables \code{type, show, x, y, 
#'  z, labels, color, cex}.
#'
#' @param x            \code{repgrid} object.
#' @param dim          Dimensions to be used for biplot (default is \code{c(1,2)}).
#' @param map.dim      Third dimension used to map aesthetic attributes (depth)
#'                     (default is \code{3}).
#' @param e.color      Colors to construct a color range to map the \code{map.dim} values onto.
#'                     The default is \code{c("white", "black")}. If only one color value
#'                     is supplied (e.g. \code{"black"}) no mapping occurs and all elements
#'                     will have the same color irrespective of their value on the \code{map.dim}
#'                     dimension.
#' @param c.color      Colors to construct a color range to map the \code{map.dim} values onto.
#'                     The default is \code{c("white", "darkred")}. If only one color value
#'                     is supplied (e.g. \code{"black"}) no mapping occurs and all constructs
#'                     will have the same color irrespective of their value on the \code{map.dim}
#'                     dimension.
#' @param e.color.map  Value range to determine what range of the color ramp defined in 
#'                     \code{e.color} will be used for the elements. Default is \code{c(.4, ,1)}. 
#' @param c.color.map  Value range tp determine what range of the color ramp defined in 
#'                     \code{c.color} will be used for the constructs. Default is \code{c(.4, ,1)}. 
#' @param e.cex.map    \code{cex} value range for the elements to map \code{map.dim} onto.
#'                     Default is \code{c(.6, ,8)}. If only one value is supplied (e.g.\code{.7}
#'                     all elements will have the same cex value irrespective of their value on 
#'                     \code{map.dim}.
#' @param c.cex.map    code{cex} value range for the constructs to map \code{map.dim} onto.
#'                     Default is \code{c(.6, ,8)}. If only one value is supplied (e.g.\code{.7}
#'                     all constructs will have the same \code{cex} value irrespective of their 
#'                     values on \code{map.dim}.
#' @param devangle     The deviation angle from the x-y plane in degrees. These can only be calculated
#'                     if a third dimension \code{map.dim} is specified. Only the constructs 
#'                     vectors that do not depart 
#'                     more than the specified degrees from the shown x-y plane will be printed.
#'                     This facilitates the visual interpretation, as only vectors represented in
#'                     the current plane are shown. Set the value to \code{91} (default) 
#'                     to show all vectors.
#' @param unity        Scale elements and constructs coordinates to unit scale in 2D (maximum of 1)
#'                     so they are printed more neatly (default \code{TRUE}).
#' @param unity3d      Scale elements and constructs coordinates to unit scale in 3D (maximum of 1)
#'                     so they are printed more neatly (default \code{TRUE}).
#' @param scale.e      Scaling factor for element vectors. Will cause element points to move a bit more
#'                     to the center. This argument is for visual appeal only.
#' @param ...          Not evaluated. 
#'
#' @return             \code{dataframe} containing the variables \code{type, show, x, y, 
#'                     z, labels, color, cex}. Usually not of interest to the user.
#' @note               TODO:  How to omit \code{map.dim}?
#'
#' @keywords internal
#' @export
#'
prepareBiplotData <- function(x, dim=c(1,2), map.dim=3, 
                               e.color=c("white", "black"), 
                               c.color=c("white", "darkred"),
                               e.cex.map=c(.6, .8),
                               c.cex.map=c(.6, .8),
                               e.color.map=c(.4, 1),
                               c.color.map=c(.4, 1),
                               c.points.devangle=90,
                               c.labels.devangle=90,
                               c.points.show=TRUE,
                               c.labels.show=TRUE,
                               e.points.show=TRUE,
                               e.labels.show=TRUE, 
                               unity=TRUE, 
                               unity3d=FALSE, 
                               scale.e=.9, ...)
{
  dim <- c(dim, map.dim)
  
  # make vector of length two if only one color/cex is specified
  if (length(e.color) == 1)         # element color
    e.color <- rep(e.color, 2)          
  if (length(c.color) == 1)         # construct color
    c.color <- rep(c.color, 2)
  if (length(e.cex.map) == 1)       # element cex for pseudo 3d dimension
    e.cex.map <- rep(e.cex.map, 2)
  if (length(c.cex.map) == 1)       # construct cex for pseudo 3d dimension
    c.cex.map <- rep(c.cex.map, 2)
  if (length(e.color.map) == 1)     # element color for pseudo 3d dimension
    e.color.map <- rep(e.color.map, 2)
  if (length(c.color.map) == 1)     # construct color for pseudo 3d dimension
    c.color.map <- rep(c.color.map, 2)
    
  
  # construct data frame containing all information needed for different plotting functions
  # (e.g. rgl and biplot functions)
  labels.e <- getElementNames(x)
  labels.cl <- getConstructNames(x)[,1] 
  labels.cr <- getConstructNames(x)[,2]
  labels.all <- c(labels.e, labels.cr, labels.cl)         # join all labels
  type <- factor(c(rep("e", getNoOfElements(x)),          # make factor specifying if row is element or construct
                   rep(c("cl", "cr"), each=getNoOfConstructs(x))))
  df <- data.frame(type=type, label=labels.all, stringsAsFactors=FALSE)
  df$cex <- .7            # default cex
  df$showpoint <- T       # default value for show point
  df$showlabel <- T       # default value for show label
  df$color <- grey(0)     # default color
  
  # calculate and add coordinates
  #x <- calcBiplotCoords(x, ...)
   
  E <- x@calcs$biplot$el
  C <- x@calcs$biplot$con
  X <- x@calcs$biplot$X
  
  # scale to unity to make E and C same size.
  # Two types of unity, for 2D and 3D
  
  if (unity){
    max.e <- max(apply(E[ ,dim[1:2]]^2, 1, sum)^.5)   # maximal length of element vectors
    max.c <- max(apply(C[ ,dim[1:2]]^2, 1, sum)^.5)   # maximal length of construct vectors
    se <- 1/max.e  * scale.e   # scale to unity to make E and C same size
    sc <- 1/max.c   
  } 
  if (unity3d){
    #max.e <- max(abs(E[ ,dim[1:3]]), na.rm=T)    
    #max.c <- max(abs(C[ ,dim[1:3]]), na.rm=T)    
    max.e <- max(apply(E[ ,dim[1:3]]^2, 1, sum)^.5)   # maximal length of element vectors
    max.c <- max(apply(C[ ,dim[1:3]]^2, 1, sum)^.5)   # maximal length of construct vectors
    se <- 1/max.e * scale.e     # scale to unity to make E and C same size
    sc <- 1/max.c     
  } 
  if (!unity & !unity3d){
    se <- 1
    sc <- 1
  }
  Cu <- C * sc
  Eu <- E * se
   
  coords <- rbind(Eu[, dim], Cu[ ,dim], -Cu[ ,dim])
  colnames(coords) <- c("x", "y", "z")
  rownames(coords) <- NULL            # otherwise warning in cbind occurs
  df <- cbind(df, coords) #, check.rows=F)
  if (is.na(dim[3]))   # if no 3rd dimension in specified, all values are set to zero i.e. neutral
    df$z <- 0

  # plot coords for all points
  z <- subset(df, type=="e", sel=z)
  cex.e <- mapCoordinatesToValue(z, e.cex.map)
  color.e <- mapCoordinatesToColor(z, color=e.color, val.range=e.color.map)

  z <- subset(df, type=="cl", sel=z)
  cex.cl <- mapCoordinatesToValue(z, c.cex.map)
  color.cl <- mapCoordinatesToColor(z, color=c.color, val.range=c.color.map)

  z <- subset(df, type=="cr", sel=z)
  cex.cr <- mapCoordinatesToValue(z, c.cex.map)
  color.cr <- mapCoordinatesToColor(z, color=c.color, val.range=c.color.map)

  df$cex <- unlist(rbind(cex.e, cex.cl, cex.cr))
  df$color <- c(color.e, color.cl, color.cr)
  
  df$devangle <- apply(df, 1, function(x) { 
     a <- as.numeric( c(x["x"], x["y"], x["z"]) )
      n <- c(0,0,1)                              # normal vector for x-y plane
      degreesBetweenVectorAndPlane(a=a, n=n)
  })
  
  # calculate absolute deviation angle from shown plane. If it is bigger than given values
  # the constructs will not be shown on the side and/or the construct points will
  # not be printed. If values >=90 all strokes and points are shown.  

  cs <- subset(df, type %in% c("cl", "cr"))
  draw <- abs(cs$devangle) <= c.labels.devangle   # which angles are smaller or equal than the maximal allowed ones?
  cs$showlabel <- cs$showlabel & draw             # show only labels that are requested and within allowed angle range
  draw <- abs(cs$devangle) <= c.points.devangle   # which angles are smaller or equal than the maximal allowed ones?
  cs$showpoint <- cs$showpoint & draw             # show only labels that are requested and within allowed angle range
  df[df$type %in% c("cl", "cr"), ] <- cs          # show only labels that are requested and within allowed angle range

  # elements #
  # select which element labels to show
  # numerical values for element selection are converted to logical
  seq.e <- seq_len(getNoOfElements(x))
  if (! (identical(e.labels.show, T) | identical(e.labels.show, F) | all(is.numeric(e.labels.show))) )
    stop("'e.labels.show' must either be a logical value or a numeric vector")
  if (all(is.numeric(e.labels.show)))
    e.labels.show <- seq.e %in% seq.e[e.labels.show]  
  df[df$type == "e", "showlabel"] <- e.labels.show   # replace showlabel column for elements

  # select which element points to show
  # numerical values for element selection are converted to logical
  if (! (identical(e.points.show, T) | identical(e.points.show, F) | all(is.numeric(e.points.show))) )
    stop("'e.points.show' must either be a logical value or a numeric vector")
  if (all(is.numeric(e.points.show)))
    e.points.show <- seq.e %in% seq.e[e.points.show]  
  df[df$type == "e", "showpoint"] <- e.points.show     # replace showpoint column for elements

  # constructs #  TODO: mechanism fill fail for single / double mode grids
  # select which construct labels to show (independently from devangle)
  # numerical values for construct selection are converted to logical
  seq.c <- seq_len(getNoOfConstructs(x))    # TODO for single mode grids
  if (! (identical(c.labels.show, T) | identical(c.labels.show, F) | all(is.numeric(c.labels.show))) )
   stop("'c.labels.show' must either be a logical value or a numeric vector")
  if (all(is.numeric(c.labels.show))){
    doubleadd <- c.labels.show + sign(c.labels.show[1]) * getNoOfConstructs(x)  # if double mode
    c.labels.show <- seq.c %in% seq.c[c(c.labels.show, doubleadd)]       
  }
  show.tmp <- df[df$type %in% c("cl", "cr"), "showlabel"] 
  df[df$type %in% c("cl", "cr"), "showlabel"] <- c.labels.show & show.tmp       # replace showlabel column for elements

  # select which construct points to show (independently from devangle)
  # numerical values for construct selection are converted to logical
  if (! (identical(c.points.show, T) | identical(c.points.show, F) | all(is.numeric(c.points.show))) )
    stop("'c.points.show' must either be a logical value or a numeric vector")
  if (all(is.numeric(c.points.show)))
    c.points.show <- seq.c %in% seq.c[c.points.show]  
  points.tmp <- df[df$type %in% c("cl", "cr"), "showpoint"]
  df[df$type %in% c("cl", "cr"), "showpoint"] <- c.points.show &  points.tmp    # replace showpoint column for elements
  
  #list(rg=x, df=df)
  x@calcs$biplot$e.unity <- Eu
  x@calcs$biplot$c.unity <- Cu
  x@plotdata <- df
  x
}


# OLD FUNCTION
# prepareBiplotData <- function(x, dim=c(1,2), map.dim=3, 
#                               e.color=c("white", "black"), 
#                               c.color=c("white", "darkred"),
#                               e.cex.map=c(.6, .8),
#                               c.cex.map=c(.6, .8),
#                               e.color.map=c(.4, 1),
#                               c.color.map=c(.4, 1),
#                               c.points.devangle=90,
#                               c.labels.devangle=30,
#                               c.points.show=TRUE,
#                               c.labels.show=TRUE,
#                               e.points.show=TRUE,
#                               e.labels.show=TRUE)
# {
#   dim <- c(dim, map.dim)
#   
#   if (length(e.color) == 1)
#     e.color <- rep(e.color, 2)
#   if (length(c.color) == 1)
#     c.color <- rep(c.color, 2)
#   if (length(e.cex.map) == 1)
#     e.cex.map <- rep(e.cex.map, 2)
#   if (length(c.cex.map) == 1)
#     c.cex.map <- rep(c.cex.map, 2)
#   if (length(e.color.map) == 1)
#     e.color.map <- rep(e.color.map, 2)
#   if (length(c.color.map) == 1)
#     c.color.map <- rep(c.color.map, 2)
#     
#   
#   # construct data frame containing all information
#   labels.e <- getElementNames(x)
#   labels.cl <- getConstructNames(x)[,1] 
#   labels.cr <- getConstructNames(x)[,2]
#   labels.all <- c(labels.e, labels.cr, labels.cl)
#   type <- factor(c(rep("e", getNoOfElements(x)), rep(c("cl", "cr"), each=getNoOfConstructs(x))))
#   df <- data.frame(type=type, label=labels.all, stringsAsFactors=F)
#   df$cex <- .7            # default cex
#   df$showpoint <- T       # default value for show point
#   df$showlabel <- T       # default value for show label
#   df$color <- grey(0)     # default color
#   
#   # calculate and add coordinates
#   esa <- calcEsaCoords(x)
#   coords <- rbind(esa$e[, dim], esa$c[ ,dim], esa$c[ ,dim]*-1)
#   colnames(coords) <- c("x", "y", "z")
#   df <- cbind(df, coords)
# 
#   # plot coords for all points
#   z <- subset(df, type=="e", sel=z)
#   cex.e <- mapCoordinatesToValue(z, e.cex.map)
#   color.e <- mapCoordinatesToColor(z, color=e.color, val.range=e.color.map)
# 
#   z <- subset(df, type=="cl", sel=z)
#   cex.cl <- mapCoordinatesToValue(z, c.cex.map)
#   color.cl <- mapCoordinatesToColor(z, color=c.color, val.range=c.color.map)
# 
#   z <- subset(df, type=="cr", sel=z)
#   cex.cr <- mapCoordinatesToValue(z, c.cex.map)
#   color.cr <- mapCoordinatesToColor(z, color=c.color, val.range=c.color.map)
# 
#   df$cex <- unlist(rbind(cex.e, cex.cl, cex.cr))
#   df$color <- c(color.e, color.cl, color.cr)
#   
#   
#   df$devangle <- apply(df, 1, function(x) { 
#      a <- as.numeric( c(x["x"], x["y"], x["z"]) )
#       n <- c(0,0,1)                              # normal vector for x-y plane
#       degreesBetweenVectorAndPlane(a=a, n=n)
#   })
#   
#   # calculate absolute deviation angle from shown plane. If it is bigger than given values
#   # the constructs will not be shown on the side and/or the construct points will
#   # not be printed. If values >=90 all strokes and points are shown.  
# 
#   cs <- subset(df, type %in% c("cl", "cr"))
#   draw <- abs(cs$devangle) <= c.labels.devangle   # which angles are smaller or equal than the maximal allowed ones?
#   cs$showlabel <- cs$showlabel & draw             # show only labels that are requested and within allowed angle range
#   draw <- abs(cs$devangle) <= c.points.devangle   # which angles are smaller or equal than the maximal allowed ones?
#   cs$showpoint <- cs$showpoint & draw             # show only labels that are requested and within allowed angle range
#   df[df$type %in% c("cl", "cr"), ] <- cs          # show only labels that are requested and within allowed angle range
# 
#   # elements #
#   # select which element labels to show
#   # numerical values for element selection are converted to logical
#   seq.e <- seq_len(getNoOfElements(x))
#   if (! (identical(e.labels.show, T) | identical(e.labels.show, F) | all(is.numeric(e.labels.show))) )
#     stop("'e.labels.show' must either be a logical value or a numeric vector")
#   if (all(is.numeric(e.labels.show)))
#     e.labels.show <- seq.e %in% seq.e[e.labels.show]  
#   df[df$type == "e", "showlabel"] <- e.labels.show   # replace showlabel column for elements
# 
#   # select which element points to show
#   # numerical values for element selection are converted to logical
#   if (! (identical(e.points.show, T) | identical(e.points.show, F) | all(is.numeric(e.points.show))) )
#     stop("'e.points.show' must either be a logical value or a numeric vector")
#   if (all(is.numeric(e.points.show)))
#     e.points.show <- seq.e %in% seq.e[e.points.show]  
#   df[df$type == "e", "showpoint"] <- e.points.show     # replace showpoint column for elements
# 
#   # constructs #  TODO: mechanism fill fail for single / double mode grids
#   # select which construct labels to show (independently from devangle)
#   # numerical values for construct selection are converted to logical
#   seq.c <- seq_len(getNoOfConstructs(x))    # TODO for single mode grids
#   if (! (identical(c.labels.show, T) | identical(c.labels.show, F) | all(is.numeric(c.labels.show))) )
#    stop("'c.labels.show' must either be a logical value or a numeric vector")
#   if (all(is.numeric(c.labels.show))){
#     doubleadd <- c.labels.show + sign(c.labels.show[1]) * getNoOfConstructs(x)  # if double mode
#     c.labels.show <- seq.c %in% seq.c[c(c.labels.show, doubleadd)]       
#   }
#   show.tmp <- df[df$type %in% c("cl", "cr"), "showlabel"] 
#   df[df$type %in% c("cl", "cr"), "showlabel"] <- c.labels.show & show.tmp       # replace showlabel column for elements
# 
#   # select which construct points to show (independently from devangle)
#   # numerical values for construct selection are converted to logical
#   if (! (identical(c.points.show, T) | identical(c.points.show, F) | all(is.numeric(c.points.show))) )
#     stop("'c.points.show' must either be a logical value or a numeric vector")
#   if (all(is.numeric(c.points.show)))
#     c.points.show <- seq.c %in% seq.c[c.points.show]  
#   points.tmp <- df[df$type %in% c("cl", "cr"), "showpoint"]
#   df[df$type %in% c("cl", "cr"), "showpoint"] <- c.points.show &  points.tmp    # replace showpoint column for elements
# 
#   list(rg=x, df=df)
# }





#' biplotDraw is the workhorse doing the drawing of a 2D biplot
#'
#' When the number of elements and constructs differs to a large extent, the 
#' absolute values of the coordinates for either constructs or elements 
#' will be much smaller or greater. This is an inherent property of the biplot.
#' In the case it is not necessary to be able to read off the original 
#' data entries from the plot, the axes for elements and constructs
#' can be scaled seperately. The proportional projection values will 
#' stay unaffetced. the absolue will change though. For grid interpretation 
#' the absolze values are usually oh no importance. Thus, there is an optional
#' argument \code{normalize} which is \code{FALSE} as a default which
#' rescales the axes so the longest construct and element vector will be 
#' set to the length of \code{1}.
#' 
#' @param x                    \code{repgrid} object.
#' @param inner.positioning    Logical. Whether to calculate positions to minimize overplotting of 
#'                             elements and construct labels (default is\code{TRUE}). Note that
#'                             the positioning may slow down the plotting.
#' @param outer.positioning    Logical. Whether to calculate positions to minimize overplotting of 
#'                             of construct labels on the outer borders (default is\code{TRUE}). Note that
#'                             the positioning may slow down the plotting.
#' @param c.labels.inside      Logical. Whether to print construct labels next to the points.
#'                             Can be useful during inspection of the plot (default \code{FALSE}).
#' @param flipaxes             Logical vector of length two. Whether x and y axes are reversed 
#'                             (default is \code{c(F,F)}).
#' @param strokes.x            Length of outer strokes in x direction in NDC.  
#' @param strokes.y            Length of outer strokes in y direction in NDC.
#' @param offsetting           Do offsetting? (TODO)
#' @param offset.labels        Offsetting parameter for labels (TODO).
#' @param offset.e             offsetting parameter for elements (TODO).
#' @param axis.ext             Axis extension factor (default is \code{.1}). A bigger value will 
#'                             zoom out the plot.
#' @param mai                  Margins available for plotting the labels in inch 
#'                             (default is \code{c(.2, 1.5, .2, 1.5)}).
#' @param rect.margins         Vector of length two (default is \code{c(.07, .07)}). Two values
#'                             specifying the additional horizontal and vertical margin around each 
#'                             label.      
#' @param srt                  Angle to rotate construct label text. Only used in case \code{offsetting=FALSE}.
#' @param cex.pos              Cex parameter used during positioning of labels if prompted. Does
#'                             usually not have to be changed by user.
#' @param xpd                  Logical (default is \code{TRUE}). Wether to extend text labels 
#'                             over figure region. Usually not needed by the user.      
#' @return                     Invisible return of dataframe used during construction of plot 
#'                             (useful for developers).
#'
#' @export
#' @keywords internal
#'
biplotDraw <- function(x, 
                      inner.positioning=TRUE,
                      outer.positioning=TRUE,
                      c.labels.inside=F,
                      flipaxes=c(F,F), 
                      strokes.x=.1, strokes.y=.1, 
                      offsetting=TRUE, offset.labels=.0, offset.e= 1, 
                      axis.ext=.1, mai=c(.2, 1.5, .2, 1.5),
                      rect.margins=c(.01, .01),
                      srt=45,
                      cex.pos=.7,
                      xpd=TRUE, ...)
{
  showpoint <- showlabel <- type <- NULL       # to prevent 'R CMD check' from noting a missing binding 
                                               # as the variables are provided in object x as default

  x <- x@plotdata          # df = data frame containing the information for printing
  
	max.all <- max(abs(x$x), abs(x$y))
	axis.ext <- 1 + axis.ext
  max.ext <- max.all * axis.ext
	
  # if (! draw.c) 
  #   x$labels[x$type %in% c("cl", "cr")] <- " "
	
	labels.constructs <- x$labels[x$type %in% c("cl", "cr")]
	labels.all <- x$labels
	
	if (flipaxes[1])
	  x$x <- x$x * -1
	if (flipaxes[2])
	  x$y <- x$y * -1
	
  # # calculate deviation angle from shown plane. If it is bigger than devangle
  # # the constructs will not be shown on the side. If >=90 all strokes are shown.  
  # 
  # #draw <- abs(x$devangle) <= devangle   # which angles are smaller or equal than the maximal allowed ones?
  # #x$showlabel <- x$showlabel & draw     # show only labels that are requested and within allowed angle range
  # cs <- subset(x, type %in% c("cl", "cr"))
  # draw <- abs(cs$devangle) <= c.labels.devangle   # which angles are smaller or equal than the maximal allowed ones?
  # cs$showlabel <- cs$showlabel & draw             # show only labels that are requested and within allowed angle range
  # draw <- abs(cs$devangle) <= c.points.devangle    # which angles are smaller or equal than the maximal allowed ones?
  # cs$showpoint <- cs$showpoint & draw             # show only labels that are requested and within allowed angle range
  # x[x$type %in% c("cl", "cr"), ] <- cs            # show only labels that are requested and within allowed angle range


	# build plot
	old.par <- par(no.readonly = TRUE)      # save parameters
  #on.exit(par(old.par))                  # reset old par when done
	
	par(mai=mai)
	plot.new()
	plot.window(xlim = c(-max.ext, max.ext), 
	            ylim = c(-max.ext, max.ext), 
	            xaxs="i", yaxs="i", asp=1)
	
	# add center lines and outer rectangle
	segments(-max.ext, 0, max.ext, 0, col="lightgrey")
	segments(0, -max.ext, 0, max.ext, col="lightgrey")
	rect(-max.ext, -max.ext, max.ext, max.ext)
	
	# make standard concentration ellipse # TODO, scaling of ellipse
	#sing <- diag(esa$sing)[dim] / sum(diag(esa$sing))
	#ellipse(sing[1], sing[2], col="lightgrey")

	# make element symbols
  es <- subset(x, type=="e" & showpoint==T)
	points(es[c("x", "y")], col=es$color, pch=15, cex=es$cex, xpd=xpd)
  
  # make construct symbols
  cs <- subset(x, type %in% c("cl", "cr") & showpoint==T)
	points(cs[c("x", "y")], col=cs$color, pch=4, cex=cs$cex, xpd=xpd)  
	  
	# positioning of element and constructs labels
	if (inner.positioning) {    # positioning by landmark algorithm from maptools package
		sh <- subset(x, showlabel==T & showpoint==T)
		lpos <- pointLabel(sh[c("x", "y")], labels=sh$label, doPlot=FALSE, cex=cex.pos)     # package maptools
	  x$x.pos <- NA
	  x$y.pos <- NA
	  sh$x.pos <- lpos$x
	  sh$y.pos <- lpos$y	  
	  x[x$showlabel==T & x$showpoint==T, ] <- sh
	} else {              # simple offsetting in y direction
	  x$x.pos <- x$x
	  x$y.pos <- NA
	  offset.y.pos <- strheight("aaaa", cex=.7)  # string height for dummy string
	  x[x$type=="e", ]$y.pos <- x[x$type=="e", ]$y + offset.y.pos * offset.e  # offset element labels by normal stringheight times x
	  x[x$type %in% c("cl", "cr"), ]$y.pos <- x[x$type %in% c("cl", "cr"), ]$y - .05
	}
	
	# text labels for elements inside plot
	es <- subset(x, type=="e" & showlabel==T & showpoint==T)
  if (dim(es)[1] > 0)
    text(es[, c("x.pos", "y.pos")], 
         labels=es$label, col=es$color, pch=15, cex=es$cex, xpd=xpd)
  
  # text labels for constructs inside plot
  if (c.labels.inside){
    cs <- subset(x, type %in% c("cl", "cr") & showlabel==T)
  	if (dim(cs)[1] > 0)
  	text(cs[, c("x.pos", "y.pos")], 
          labels=cs$label, col=cs$color, pch=4, cex=cs$cex, xpd=xpd)
  }
    
  # initial coords for labels for strokes
  str.3 <- calcCoordsBorders(x["x"], x["y"], 
                             xm=max.ext * (1 + strokes.x + offset.labels), # + rect.margins[1]/2), 
                             ym=max.ext * (1 + strokes.y + offset.labels))# + rect.margins[2]/2))
  colnames(str.3) <- c("str.3.x", "str.3.y")                           
  x <- cbind(x, str.3)
  
  #segments(0,0,x$str.3.x, x$str.3.y)   # debug
  # calc coordinates for sourrounding rectangles (elements and constructs)
  lb <- calcRectanglesCoordsForLabels(x[, c("str.3.x", "str.3.y")], x$label, 
                                      cex=x$cex, x.ext=rect.margins[1], y.ext=rect.margins[2])
  colnames(lb) <- c("str.3.x0", "str.3.y0", "str.3.x1", "str.3.y1")
  x <- cbind(x, lb)
  #segments(x$str.3.x0, x$str.3.y0, x$str.3.x1, x$str.3.y1)   # debug
  
    
  # offset labels in y direction if too close together
  # for labels on the left and on the right seperately
  x$angle <- atan2(x$y, x$x)      # caveat: first y, then y argument!
  x <- x[order(x$angle), ]        # sort by angles

  # assign quandrants for offsetting
  x$quadrant[x$angle >= 0    & x$angle < pi/2] <- "ur"    # 
  x$quadrant[x$angle >= pi/2 & x$angle <= pi] <- "ul"     # 
  x$quadrant[x$angle < 0     & x$angle >= -pi/2] <- "lr"  # 
  x$quadrant[x$angle < -pi/2 & x$angle >= -pi] <- "ll"    # 
  
  
  # calc necessary offset (only correct in case there is overlap!)
  necessaryOffset <- function(a, b, direction=1, margin=.05){
    if (direction >= 0){        # offset upwards
      offset <- a[4] - b[2]     # is always positive >= 0
      if (offset < 0)           # if smaller than zero there should be no overlap anyway
        offset <- 0
    } else {                    # offset downwards
      offset <- a[2] - b[4]     # should always be <= 0
      if (offset > 0)           # if bigger than zero there is no overlap
        offset <- 0
    }
    as.numeric(offset + margin * sign(direction))
  }
  
  # offset quadrants
  #lr <- subset(x.sorted, type %in% c("cr", "cl") & quadrant=="lr")
  #ol <- lr[, c("str.3.x0", "str.3.y0", "str.3.x1", "str.3.y1")]

  #order.lines <- 1:nrow(ol)
  #order.lines <- rev(order.lines)
  
  # lim <- c(min(ol), max(ol))
  # plot(0,  type="n", xlim=lim, ylim=lim)
  # rect(ol[,1], ol[,2], ol[,3], ol[,4])
  # text(ol[,1], ol[,2], order.lines)
     
  offsetQuadrant <- function(x, quadrant="ur", direction=1, 
                             reverse=T, margin=0.02){
    index <- x$type %in% c("cr", "cl") & x$quadrant==quadrant   # get constructs of quandrant
    
    ol <- x[index, ]
    vars <- c("str.3.x0", "str.3.y0", "str.3.x1", "str.3.y1")
    
    order.lines <- 1:nrow(ol)
    if (reverse)
      order.lines <- rev(order.lines)
   
    for (i in order.lines){
      for (i.n in order.lines){
        if(i != i.n){
          overlap <- doRectanglesOverlap(ol[i, vars], ol[i.n, vars])
          if (overlap){    # if overlap is present the rectangles is moved to avoid overlap
            offset <- necessaryOffset(ol[i, vars], ol[i.n, vars], dir=direction, margin=margin)
            ol[i.n, c("str.3.y", "str.3.y0","str.3.y1")] <- 
                ol[i.n, c("str.3.y", "str.3.y0", "str.3.y1")] + offset
          }
        }
      }
    }
    
    x[index, c("str.3.y", vars)] <- ol[, c("str.3.y", vars)]
    x
  }
  
  # code is slow!
  if (outer.positioning){
    x <- offsetQuadrant(x, quadrant="ur", direction=1, reverse=F)    #dir.ur <-  1; reverse <- F 
    x <- offsetQuadrant(x, quadrant="ul", direction=1, reverse=T)    #dir.ul <-  1; reverse <- T
    x <- offsetQuadrant(x, quadrant="ll", direction=-1, reverse=F)   #dir.ll <- -1; reverse <- F 
    x <- offsetQuadrant(x, quadrant="lr", direction=-1, reverse=T)   #dir.lr <- -1; reverse <- T
  }
  # 
  # for (i in order.lines){
  #   #cat("---\n")
  #   for (i.n in order.lines){
  #     if(i != i.n){
  #       overlap <- doRectanglesOverlap(ol[i, ], ol[i.n, ])
  #       #cat("(", i, i.n, ")", "\t\t"); print(overlap)
  #       if (overlap){    # if overlap is present the rectangles is moved to avoid overlap  
  #         offset <- necessaryOffset(ol[i, ], ol[i.n, ], dir=-1, margin=0.02)
  #         #print(offset)
  #         ol[i.n, c("str.3.y0","str.3.y1")] <- 
  #             ol[i.n, c("str.3.y0","str.3.y1")] + offset
  #       }
  #     }
  #   }
  # }

  # lim <- c(min(ol), max(ol))
  #   rect(ol[,1], ol[,2], ol[,3], ol[,4], border="blue", lty=2)
  #   text(ol[,3], ol[,2], order.lines, col="blue" )
  # 
  # 
  #   plot(0:5)
  #   rect(ol[4,1],ol[4,2],ol[4,3],ol[4,4])
  #   rect(ol[3,1],ol[3,2],ol[3,3],ol[3,4])
  #   doRectanglesOverlap(ol[4,], ol[3,])
  # do others overlap? If yes move them
  
  # make outer strokes for all labels (elements and constructs) 
  # select which to draw later
  # coordinates for stroke starts
  str.1 <- calcCoordsBorders(x["x"], x["y"], xm=max.ext, ym=max.ext)
  colnames(str.1) <- c("str.1.x", "str.1.y")
  
  # coordinates for stroke ends
  str.2 <- calcCoordsBorders(x["x"], x["y"], xm=max.ext * (1 + strokes.x), 
                             ym=max.ext * (1 + strokes.y))
  colnames(str.2) <- c("str.2.x", "str.2.y")
  
  x <- cbind(x, str.1, str.2)
  
  # redo coordinates for stroke ends according to edges of rectangles that have been offsetted
  a <- list()
  for (i in seq_len(nrow(x))){
    a[[i]] <- calcCoordsBorders(x[i, "x"], x[i, "y"], xm=max.ext * (1 + strokes.x), 
                               ym=abs(x[i, "str.3.y"]))
  }
  str.4 <- do.call(rbind, a)
  colnames(str.4) <- c("str.4.x", "str.4.y")
  x <- cbind(x, str.4)

  if (!c.labels.inside){   # when constructs labels are  prompted to be outside the plot(default) 
    # rotate labels srt degress on top and bottom for quick printing
    y.max.ext <- max.ext * (1 + strokes.y + offset.labels)  # position of outer strokes to determine side of labels
    x$rotate <- 0                         # default is no rotation of labels in text 
    if (!outer.positioning)               # only for positioning = FALSE to get neater label directions 
      x$rotate[x$str.3.y == y.max.ext |   # replace by standadrd rotation angle
               x$str.3.y == -y.max.ext] <- srt
    
    # only make labels, rectangles and strokes that are prompted
    cl <- subset(x, type %in% c("cl", "cr") & showlabel==T)    # select only labels that should be shown
    segments(cl$str.1.x, cl$str.1.y, cl$str.2.x, cl$str.2.y, xpd=T)
    segments(cl$str.2.x, cl$str.2.y, cl$str.4.x, cl$str.4.y, xpd=T, lty=3)
    rect(cl$str.3.x0, cl$str.3.y0, 
         cl$str.3.x1, cl$str.3.y1, col=grey(1), border=grey(1), xpd=T)

    # print constructs labels (if there are any) and not only inner labels are prompted
    if (nrow(cl) > 0){
      for (i in 1:nrow(cl)){
        if (cl$str.3.x[i] < 0) 
          adj <- c(1, .5) else
          adj <- c(0, .5)
        if (!outer.positioning){      # overwrite adj in case of no positioning
          if (cl$str.3.y[i] == y.max.ext)  
            adj <- c(0, .5)
          if (cl$str.3.y[i] == -y.max.ext)  
            adj <- c(1, .5)
        }
        text(cl$str.3.x[i], cl$str.3.y[i], labels=cl$label[i], col=cl$col[i], 
             cex=cl$cex[i], adj=adj, xpd=T, srt=cl$rotate[i])   
      }
    }
  }
  invisible(x)
}

# x <- calcBiplotCoords(raeithel, g=1, h=1)
# x <- prepareBiplotData(x)
# biplotDraw(x))    # add amount explained variance to the axes




#' adds the precentage of the sum-of-squares explained by each axis to the plot
#' 
#' @param x           \code{repgrid} object containing the biplot coords, i.e. after 
#'                    having called \code{\link{calcBiplotCoords}} and 
#'                    \code{\link{prepareBiplotData}}.
#' @param dim         The dimensions to be printed.
#' @param var.cex     The cex value for the percentages shown in the plot.
#' @param var.col     The color value of the percentages shown in the plot.
#' @param axis.ext  Axis extension factor (default is \code{.1}). A bigger value will 
#'                     zoom out the plot.
#'
#' @keywords internal
#' @export
#'
addVarianceExplainedToBiplot2d <- function(x, dim=c(1,2,3), var.cex=.7, 
                                          var.col=grey(.1), axis.ext = .1, ...){
  sv <- x@calcs$biplot$D        # get singular values from SVD
  sv.exp <- sv^2/sum(sv^2)      # proportion of ssq explained per principal component
  var <- paste("Dim ", dim[1:2], ": ", round(sv.exp[dim[1:2]] * 100, 1), "%", sep="")
  
  data <- x@plotdata          # data frame from data prepare function return
  max.all <- max(abs(data$x), abs(data$y))
  axis.ext <- 1 + axis.ext
  max.ext <- max.all * axis.ext
  
  ext <- strheight(var[1], cex=var.cex)
  
  text(max.ext - ext/2, 0, var[1] , cex=var.cex, adj=c(.5,0), col=var.col, srt=90)
  text(0, -max.ext + ext/2, var[2] , cex=var.cex, adj=c(.5,0), col=var.col)
}

# x <- randomGrid(20, 40)
# x <- boeker
# x <- raeithel
# xb <- prepareBiplotData(x, c.labels.show=F, c.points.dev=90, e.points=1:3, e.labels=T)
# biplotDraw(xb, xpd=F, inner=F, outer=F)
# addVarianceExplainedToBiplot(xb$rg, xb$df)
# 
# xb <- prepareBiplotData(x, c.points.dev=5, c.labels.dev=5)
# biplotDraw(xb, xpd=F, inner=F, outer=T, mai=rep(0,4), c.labels.inside=T)
# biplotDraw(xb, xpd=F, inner=F, outer=T)
# 
# dev.new()
# xb <- prepareBiplotData(x, dim=c(1,2), map=4)
# biplotDraw(xb, dev=15)
# addVarianceExplainedToBiplot(x, xb, dim=c(1,2,4))
# 
# 
# dev.new()
# xb <- prepareBiplotData(x, dim=c(2,3), map=1)
# biplotDraw(xb, dev=15)
# addVarianceExplainedToBiplot(x, xb, dim=c(2,3,1))
# 
# dev.new()
# xb <- prepareBiplotData(x, dim=c(3,4), map=1)
# biplotDraw(xb, dev=15)
# addVarianceExplainedToBiplot(x, xb, dim=c(3,4,1))

# x <- boeker
# x <- prepareBiplotData(x, e.col="black", c.col="black", cex.e=.7, cex.c=.7)#, color.e=.8, color.c=.8)
# biplotDraw(x)
# addVarianceExplainedToBiplot(boeker, x)
# 
# x <- boeker
# x <- prepareBiplotData(x, e.col="black", c.col="black", cex.e=.7, cex.c=.7, 
#                         color.e=.3, color.c=.5)
# biplotDraw(x)
# addVarianceExplainedToBiplot(boeker, x)
# 
# x <- boeker
# x <- prepareBiplotData(x, e.col="black", c.col="black", cex.e=c(.3,1))
# x <- prepareBiplotData(x, e.col="black", c.col="black",  cex.e=c(.3,1), cex.c=c(.3,1))
# x <- prepareBiplotData(x, cex.e=c(.5,1.3), cex.c=c(.5,1.3))
# x <- prepareBiplotData(x, cex.e=c(.5,1), cex.c=c(.5,1), color.c.map=c(0, 0))
# biplotDraw2(x)
# 
# x <- boeker
# x <- raeithel
# 
# layout(matrix(1:4, by=T, ncol=2))
# 
# xb <- prepareBiplotData(x, dim=c(1,2), map=3)
# biplotDraw(xb)
# addVarianceExplainedToBiplot(x, xb, dim=1:3)
# 
# xb <- prepareBiplotData(x, dim=c(2,3), map=1)
# biplotDraw(xb)
# addVarianceExplainedToBiplot(x, xb, dim=c(2,3,1))
# 
# xb <- prepareBiplotData(x, dim=c(3,4), map=1)
# biplotDraw(xb)
# addVarianceExplainedToBiplot(x, xb, dim=c(3,4,1))
# 
# xb <- prepareBiplotData(x, dim=c(1,4), map=2)
# biplotDraw(xb)
# addVarianceExplainedToBiplot(x, xb, dim=c(1,4,2))







#' Draw a two-dimensional biplot
#'
#' Draws a 2D biplot of the grid. Different types of biplots can be requested.
#'
#' @param x                   \code{repgrid} object.
#' @param dim                 Dimensions (i.e. principal components) to be used for biplot 
#'                            (default is \code{c(1,2)}).
#' @param map.dim             Third dimension (depth) used to map aesthetic attributes to
#'                            (default is \code{3}).
#' @param  center		        Numeric. The type of centering to be performed. 
#'                          0= no centering, 1= row mean centering (construct), 
#'                          2= column mean centering (elements), 3= double-centering (construct and element means),
#'                          4= midpoint centering of rows (constructs).
#'                          The default is \code{1} (row centering).
#' @param normalize         A numeric value indicating along what direction (rows, columns)
#'                          to normalize by standard deviations. \code{0 = none, 1= rows, 2 = columns}
#'                          (default is \code{0}).
#' @param g                 Power of the singular value matrix assigned to the left singular 
#'                          vectors, i.e. the constructs.
#' @param h                 Power of the singular value matrix assigned to the right singular 
#'                          vectors, i.e. the elements.
#' @param col.active        Columns (elements) that are no supplementary points, i.e. they are used
#'                          in the SVD to find principal components. default is to use all elements.
#' @param col.passive       Columns (elements) that are supplementary points, i.e. they are NOT used
#'                          in the SVD but projecte into the component space afterwards. They do not 
#'                          determine the solution. Default is \code{NA}, i.e. no elements are set 
#'                          supplementary.
#' @param e.color             Color of the constructs symbols (default is \code{"black"}. 
#' @param c.color             Color of the element symbols (default is \code{"black"}. 
#' @param e.cex.map           Cex values (i.e. text size) of the element labels. 
#'                            Default is \code{.7}. Not important for the user. 
#'                            See \code{\link{biplotPseudo3d}} for its use.
#' @param c.cex.map           Cex values (i.e. text size) of the construct labels. 
#'                            Default is \code{.7}. Not important for the user. 
#'                            See \code{\link{biplotPseudo3d}} for its use.
#' @param e.color.map         Value range to determine what range of the color ramp defined in 
#'                            \code{e.color} will be used for the elements. Default is \code{c(.4, ,1)}. Not important for the user. 
#'                            See \code{\link{biplotPseudo3d}} for its use. 
#' @param c.color.map         Value range tp determine what range of the color ramp defined in 
#'                            \code{c.color} will be used for the constructs. Default is \code{c(.4, ,1)}.
#'                            Not important for the user. See \code{\link{biplotPseudo3d}} for its use.
#' @param c.points.devangle   The deviation angle from the x-y plane in degrees. These can only be calculated
#'                            if a third dimension \code{map.dim} is specified. Only the constructs 
#'                            that do not depart more than the specified degrees from the 
#'                            x-y plane will be printed. This facilitates the visual 
#'                            interpretation, as only vectors represented near the current plane 
#'                            are shown. Set the value to \code{91} (default) 
#'                            to show all vectors.
#' @param c.labels.devangle   The deviation angle from the x-y plane in degrees. These can only be calculated
#'                            if a third dimension \code{map.dim} is specified. Only the labels of constructs 
#'                            that do not depart more than the specified degrees from the 
#'                            x-y plane will be printed. Set the value to \code{91} (default) 
#'                            to show all construct labels.
#' @param c.points.show       Whether the constructs are printed (default is \code{TRUE}).
#'                            \code{FALSE} will surpress the printing of the constructs.
#'                            To only print certain constructs a numeric vector can be 
#'                            provided (e.g. \code{c(1:10)}).
#' @param c.labels.show       Whether the construct labels are printed (default is \code{TRUE}).
#'                            \code{FALSE} will surpress the printing of the labels.
#'                            To only print certain construct labels a numeric vector can be 
#'                            provided (e.g. \code{c(1:10)}).
#' @param e.points.show       Whether the elements are printed (default is \code{TRUE}).
#'                            \code{FALSE} will surpress the printing of the elements.
#'                            To only print certain elements a numeric vector can be 
#'                            provided (e.g. \code{c(1:10)}).
#' @param e.labels.show       Whether the element labels are printed (default is \code{TRUE}).
#'                            \code{FALSE} will surpress the printing of the labels.
#'                            To only print certain element labels a numeric vector can be 
#'                            provided (e.g. \code{c(1:10)}).
#' @param inner.positioning    Logical. Whether to calculate positions to minimize overplotting of 
#'                             elements and construct labels (default is\code{TRUE}). Note that
#'                             the positioning may slow down the plotting.
#' @param outer.positioning    Logical. Whether to calculate positions to minimize overplotting of 
#'                             of construct labels on the outer borders (default is\code{TRUE}). Note that
#'                             the positioning may slow down the plotting.
#' @param c.labels.inside      Logical. Whether to print construct labels next to the points.
#'                             Can be useful during inspection of the plot (default \code{FALSE}).
#' @param flipaxes             Logical vector of length two. Whether x and y axes are reversed 
#'                             (default is \code{c(F,F)}).
#' @param strokes.x            Length of outer strokes in x direction in NDC.  
#' @param strokes.y            Length of outer strokes in y direction in NDC.
#' @param offsetting           Do offsetting? (TODO)
#' @param offset.labels        Offsetting parameter for labels (TODO).
#' @param offset.e             offsetting parameter for elements (TODO).
#' @param axis.ext             Axis extension factor (default is \code{.1}). A bigger value will 
#'                             zoom out the plot.
#' @param mai                  Margins available for plotting the labels in inch 
#'                             (default is \code{c(.2, 1.5, .2, 1.5)}).
#' @param rect.margins         Vector of length two (default is \code{c(.07, .07)}). Two values
#'                             specifying the additional horizontal and vertical margin around each 
#'                             label.      
#' @param srt                  Angle to rotate construct label text. Only used in case \code{offsetting=FALSE}.
#' @param cex.pos              Cex parameter used during positioning of labels if prompted. Does
#'                             usually not have to be changed by user.
#' @param xpd                  Logical (default is \code{TRUE}). Wether to extend text labels 
#'                             over figure region. Usually not needed by the user.
#' @param unity               Scale elements and constructs coordinates to unit scale in 2D (maximum of 1)
#'                            so they are printed more neatly (default \code{TRUE}).
#' @param unity3d             Scale elements and constructs coordinates to unit scale in 3D (maximum of 1)
#'                            so they are printed more neatly (default \code{TRUE}).
#' @param scale.e             Scaling factor for element vectors. Will cause element points to move a bit more
#'                            to the center. This argument is for visual appeal only.
#' @param ...                 to come
#'
#' @return  just for the sideeffects, i.e. drawing
#' @export
#'
biplot2d <- function(x, dim=c(1,2), map.dim=3, 
                    center=1,
                    normalize=0, 
                    g=0, 
                    h=1-g, 
                    col.active=NA, 
                    col.passive=NA,
                    e.color="black", 
                    c.color="black",
                    e.cex.map=.7,
                    c.cex.map=.7,
                    e.color.map=c(.4, 1),
                    c.color.map=c(.4, 1),
                    c.points.devangle=91,
                    c.labels.devangle=91,
                    c.points.show=TRUE,
                    c.labels.show=TRUE,
                    e.points.show=TRUE,
                    e.labels.show=TRUE,
                    inner.positioning=TRUE,
                    outer.positioning=TRUE,
                    c.labels.inside=F,
                    flipaxes=c(F,F), 
                    strokes.x=.1, strokes.y=.1, 
                    offsetting=TRUE, offset.labels=.0, offset.e= 1, 
                    axis.ext=.1, mai=c(.2, 1.5, .2, 1.5),
                    rect.margins=c(.01, .01),
                    srt=45,
                    cex.pos=.7,
                    xpd=TRUE, 
                    unity=FALSE, 
                    unity3d=FALSE,
                    scale.e=.9, 
                    ...)
{
  x <- calcBiplotCoords(x, center=center, normalize=normalize, 
                        g=g, h=h, col.active=col.active, col.passive=col.passive, ...)
  x <- prepareBiplotData(x, dim=dim, map.dim=map.dim, e.color=e.color, c.color=c.color,
                      e.cex.map=e.cex.map, c.cex.map=c.cex.map, e.color.map=e.color.map,
                      c.color.map=c.color.map, c.points.devangle=c.points.devangle,
                      c.labels.devangle=c.labels.devangle, c.points.show=c.points.show,
                      c.labels.show=c.labels.show, e.points.show=e.points.show,
                      e.labels.show=e.labels.show, unity=unity, unity3d=unity3d, scale.e=scale.e, ...)
  biplotDraw(x, inner.positioning=inner.positioning, outer.positioning=outer.positioning,
                      c.labels.inside=c.labels.inside, flipaxes=flipaxes, 
                      strokes.x=strokes.x, strokes.y=strokes.y, 
                      offsetting=offsetting, offset.labels=offset.labels, offset.e=offset.e, 
                      axis.ext=axis.ext, mai=mai, rect.margins=rect.margins,
                      srt=srt, cex.pos=cex.pos, xpd=xpd)
  addVarianceExplainedToBiplot2d(x, dim=dim, ...)
  invisible(x)                                           
}


#' Draws a biplot of the grid in 2D with depth impression (pseudo 3D)
#'
#' Draws a biplot for the grid. Different types of biplots can be requested.
#' This function will call the standard \code{\link{biplot2d}} function with some 
#' modified arguments. For the whole set of argumnets that can be modified
#' see \code{\link{biplot2d}}. Here only the arguments special to 
#' \code{biplotPseudo3d} are outlined.
#'
#' @param x            \code{repgrid} object.
#' @param dim          Dimensions (i.e. principal components) to be used for biplot 
#'                     (default is \code{c(1,2)}).
#' @param map.dim      Third dimension (depth) used to map aesthetic attributes to
#'                     (default is \code{3}).
#' @param e.color      Colors to construct a color range to map the \code{map.dim} values onto.
#'                     The default is \code{c("white", "black")}. If only one color value
#'                     is supplied (e.g. \code{"black"}) no mapping occurs and all elements
#'                     will have the same color irrespective of their value on the \code{map.dim}
#'                     dimension.
#' @param c.color      Colors to construct a color range to map the \code{map.dim} values onto.
#'                     The default is \code{c("white", "darkred")}. If only one color value
#'                     is supplied (e.g. \code{"black"}) no mapping occurs and all constructs
#'                     will have the same color irrespective of their value on the \code{map.dim}
#'                     dimension.
#' @param e.color.map  Value range to determine what range of the color ramp defined in 
#'                     \code{e.color} will be used for the elements. Default is \code{c(.4, ,1)}. 
#' @param c.color.map  Value range tp determine what range of the color ramp defined in 
#'                     \code{c.color} will be used for the constructs. Default is \code{c(.4, ,1)}. 
#' @param e.cex.map    \code{cex} value range for the elements to map \code{map.dim} onto.
#'                     Default is \code{c(.6, ,8)}. If only one value is supplied (e.g.\code{.7}
#'                     all elements will have the same cex value irrespective of their value on 
#'                     \code{map.dim}.
#' @param c.cex.map    code{cex} value range for the constructs to map \code{map.dim} onto.
#'                     Default is \code{c(.6, ,8)}. If only one value is supplied (e.g.\code{.7}
#'                     all constructs will have the same \code{cex} value irrespective of their 
#'                     values on \code{map.dim}.
#' @param ...         Additional parameters passed to \code{\link{biplot2d}}.
#' @export
#'
biplotPseudo3d <- function(x, dim=1:2, map.dim=3, 
                           e.color=c("white", "black"), c.color=c("white", "darkred"),
                           e.color.map = c(.4, ,1), c.color.map = c(.4, ,1),
                           e.cex.map=c(.6, .8), c.cex.map=c(.6, .8), ...){
  biplot2d(x=x, dim=dim, map.dim=map.dim, 
            e.color=e.color, c.color=c.color,
            e.color.map= e.color.map, c.color.map=c.color.map,
            e.cex.map=e.cex.map, c.cex.map=c.cex.map, ...)
}

    
#' Draws Slater's biplot in 2D
#'
#' Draws Slater's biplot for the grid in 2D. The default is to use row centering 
#' and no normalization. Note that Slater's biplot is just a special case of a biplot
#' that can be produced using the \code{\link{biplot2d}} function with the arguments
#' \code{center=1, g=1, h=1}.
#' 
#' Here, only the argumnets that are set for Slater's biplot are described.
#' To see all the parameters that can be changed see \code{\link{biplot2d}}.
#'
#' @param x           \code{repgrid} object.
#' @param  center		  Numeric. The type of centering to be performed. 
#'                    0= no centering, 1= row mean centering (construct), 
#'                    2= column mean centering (elements), 3= double-centering (construct and element means),
#'                    4= midpoint centering of rows (constructs).
#'                    Slater's biplot uses \code{1} (row centering).
#' @param g           Power of the singular value matrix assigned to the left singular 
#'                    vectors, i.e. the constructs.
#' @param h           Power of the singular value matrix assigned to the right singular 
#'                    vectors, i.e. the elements.
#' @param ...         Additional parameters for be passed to \code{\link{biplot2d}}.
#' @export
#'
biplotSlater2d <- function(x, center=1, g=1, h=1, ...){
  biplot2d(x=x, center=center, g=g, h=h, ...)
}


#' Draws Slater's biplot in 2D with depth impression (pseudo 3D)
#'
#' 
#' Draws Slater's biplot for the grid adding 3D impression. The default is to use row centering 
#' and no normalization. Note that Slater's biplot is just a special case of a biplot
#' that can be produced using the \code{\link{biplotPseudo3d}} function with the arguments
#' \code{center=1, g=1, h=1}.
#' 
#' Here, only the arguments that are set for Slater's biplot are described.
#' To see all the parameters that can be changed see \code{\link{biplotPseudo3d}}.
#'
#' @param x           \code{repgrid} object.
#' @param  center		  Numeric. The type of centering to be performed. 
#'                    0= no centering, 1= row mean centering (construct), 
#'                    2= column mean centering (elements), 3= double-centering (construct and element means),
#'                    4= midpoint centering of rows (constructs).
#'                    Slater's biplot uses \code{1} (row centering).
#' @param g           Power of the singular value matrix assigned to the left singular 
#'                    vectors, i.e. the constructs.
#' @param h           Power of the singular value matrix assigned to the right singular 
#'                    vectors, i.e. the elements.
#' @param ...         Additional parameters for be passed to \code{\link{biplotPseudo3d}}.
#' @export
#'
biplotSlaterPseudo3d <- function(x, center=1, g=1, h=1, ...){
  biplotPseudo3d(x=x, center=center, g=g, h=h, ...)
}



#' Plot an eigenstructure analysis (ESA) in 2D
#'
#' Plots an eigenstructure analysis (ESA) for the grid in 2D. The ESA is 
#' a special type of biplot suggested by Raeithel (e.g. 1998).
#' It uses midpoint centering as a default. Note that the eigenstructure analysis
#' is just a special case of a biplot that can also be produced using the 
#' biplot2d function with the arguments \code{center=4, g=1, h=1}.
#'
#' Here, only the argumnets that are set for Slater's biplot are described.
#' To see all the parameters that can be changed see \code{\link{biplot2d}}.
#'
#' @param x           \code{repgrid} object.
#' @param  center		  Numeric. The type of centering to be performed. 
#'                    0= no centering, 1= row mean centering (construct), 
#'                    2= column mean centering (elements), 3= double-centering (construct and element means),
#'                    4= midpoint centering of rows (constructs).
#'                    Eigenstructure analyis uses midpoint centering (\code{4}).
#' @param g           Power of the singular value matrix assigned to the left singular 
#'                    vectors, i.e. the constructs. Eigenstructure analyis uses  
#'                    \code{g=1}.
#' @param h           Power of the singular value matrix assigned to the right singular 
#'                    vectors, i.e. the elements. Eigenstructure analyis uses  
#'                    \code{h=1}.
#' @param ...         Additional parameters for be passed to \code{\link{biplot2d}}.
#'
#' @references   Raeithel, A. (1998). Kooperative Modellproduktion von Professionellen 
#'                und Klienten. Erlaeutert am Beispiel des Repertory Grid.
#'                In A. Raeithel (1998). Selbstorganisation, Kooperation, 
#'                Zeichenprozess. Arbeiten zu einer kulturwissenschaftlichen, 
#'                anwendungsbezogenen Psychologie (p. 209-254). Opladen: 
#'                Westdeutscher Verlag.
#' @export
#'
biplotEsa2d <- function(x, center=4, g=1, h=1, ...){
  biplot2d(x=x, center=center, g=g, h=h, ...)
}


#' Plot an eigenstructure analysis (ESA) in 2D grid adding 3D 
#' impression (pseudo 3D). The ESA is 
#' a special type of biplot suggested by Raeithel (e.g. 1998).
#' It uses midpoint centering as a default. Note that the eigenstructure analysis
#' is just a special case of a biplot that can also be produced using the 
#' biplot2d function with the arguments \code{center=4, g=1, h=1}.
#'
#' Here, only the argumnets that are set for Slater's biplot are described.
#' To see all the parameters that can be changed see \code{\link{biplotPseudo3d}}.
#'
#' @param x           \code{repgrid} object.
#' @param  center		  Numeric. The type of centering to be performed. 
#'                    0= no centering, 1= row mean centering (construct), 
#'                    2= column mean centering (elements), 3= double-centering (construct and element means),
#'                    4= midpoint centering of rows (constructs).
#'                    Eigenstructure analyis uses midpoint centering (\code{4}).
#' @param g           Power of the singular value matrix assigned to the left singular 
#'                    vectors, i.e. the constructs. Eigenstructure analyis uses  
#'                    \code{g=1}.
#' @param h           Power of the singular value matrix assigned to the right singular 
#'                    vectors, i.e. the elements. Eigenstructure analyis uses  
#'                    \code{h=1}.
#' @param ...         Additional parameters for be passed to \code{\link{biplotPseudo3d}}.
#' @export
#'
biplotEsaPseudo3d <- function(x, center=4, g=1, h=1, ...){
  biplotPseudo3d(x=x, center=center, g=g, h=h, ...)
}


# x <- raeithel
# biplot2d(x)
# dev.new()
# biplotPseudo3d(x)
# 
# biplot2d(x)
# dev.new()
# biplot2d(x, flipaxes=c(T, F))
# biplot2d(x, c.color="brown")
# 
# biplot2d(x, c.labels.inside=T)
# biplot2d(x, c.labels.inside=T, mai=c(0,0,0,0), xpd=F)
# 
# biplot2d(x, e.color="black", c.color="black", e.cex=.7, c.cex=.7)
# 
# biplot2d(bell2010)
# biplot2d(bell2010, e.points=c(1,3,6))
# biplot2d(bell2010, e.points=c(1,3,6), c.labels.dev=15)
# 
# biplotPseudo3d(bell2010, e.points=c(1,3,6))
# biplotPseudo3d(bell2010, e.points=c(1,3,6), c.labels.dev=15)
# 
# bell2010



###############################################################################

# x <- boeker
# x <- calcBiplotCoords(x, g=1, h=1)
# x <- prepareBiplotData(x, unity=F)
# biplot2d(x)
# 
# biplot2d(x)


###############################################################################



