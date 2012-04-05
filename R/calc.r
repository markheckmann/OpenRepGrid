#' Descriptive statistics for constructs and elements of a grid.
#'
#' @param x       \code{repgrid} object.
#' @param index   Whether to print the number of the element. 
#' @param trim    The number of characters an element or a construct
#'                is trimmed to (default is
#'                \code{20}). If \code{NA} no trimming occurs. Trimming
#'                simply saves space when displaying correlation of constructs
#'                or elements with long names.
#' @param output  The type of output printed to the console. \code{output=0}
#'                will supress printing of the output.
#' @return        A dataframe containing the following measures is returned invisibly 
#'                (see \code{\link{describe}}): \cr
#'                item name \cr
#'                item number \cr 
#'                number of valid cases \cr
#'                mean standard deviation \cr
#'                trimmed mean (with trim defaulting to .1) \cr
#'                median (standard or interpolated) \cr
#'                mad: median absolute deviation (from the median) \cr
#'                minimum \cr
#'                maximum \cr
#'                skew \cr
#'                kurtosis \cr
#'                standard error \cr
#'
#' @note          Note that standard deviation and variance are estimated ones, 
#'                i.e. including Bessel's correction. For more info type \code{?describe}.
#'
#' @author        Mark Heckmann
#' @export
#' @aliases statsElements statsConstructs 
#' @rdname stats
#' @examples \dontrun{
#'
#'    statsConstructs(fbb2003)
#'    statsConstructs(fbb2003, trim=10)
#'    statsConstructs(fbb2003, trim=10, index=F)
#'
#'    statsElements(fbb2003)
#'    statsElements(fbb2003, trim=10)
#'    statsElements(fbb2003, trim=10, index=F)
#'
#'    d <- statsElements(fbb2003, out=0)
#'    d
#'  
#'    d <- statsConstructs(fbb2003, out=0)
#'    d
#' }
#'
statsElements <- function(x, index=T, trim=20, output=1){
  if (!inherits(x, "repgrid")) 							      # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  s <- getRatingLayer(x)
  res <- describe(s)                              # psych function
  enames <- getElementNames2(x, index=index, trim=trim)
  
  ne <- getNoOfElements(x)
  if(length(unique(enames)) != ne){
    stop("please chose a longer value for 'trim' or set 'index' to TRUE", 
         "as the current value produces indentical rownames")
  }
  rownames(res) <- enames
  
  # output to console
  if (output){
    cat("\n##################################")
    cat("\nDesriptive statistics for elements")
    cat("\n##################################\n\n")
    print(res)
  }
  invisible(res)
}
# compare Bell, 1997, p. 7


#' Descriptive statistics for constructs and elements of a grid.
#'
#' @note          Note that standard deviation and variance are estimated ones, 
#'                i.e. including Bessel's correction. For more info type \code{?describe}.
#'
#' @author        Mark Heckmann
#' @export
#' @rdname stats
#'
statsConstructs <- function(x, index=T, trim=20, output=1){
  if (!inherits(x, "repgrid")) 							      # check if x is repgrid object
   	stop("Object x must be of class 'repgrid'")
  s <- getRatingLayer(x)
  res <- describe(t(s))
  cnames <- getConstructNames2(x, index=index, trim=trim)
  rownames(res) <- cnames
  
  # output to console
  if (output){
    cat("\n####################################")
    cat("\nDesriptive statistics for constructs")
    cat("\n####################################\n\n")
    print(res)
  }
  invisible(res)
}
# compare Bell, 1997, p. 9


# TODO:
# Golden Section Statistics, p. 10
# Adams-Webber (1990) for dichotmous data
# Bell & McGorry for rating grids
#
# Adams-Webber, J.R. (1990) Some fundamental asymmetries in the structure of personal
# constructs. Advances in Personal Constructs, 1, 49-55.
#
# Bell, R. C., & McGorry P. (1992) The analysis of repertory 
# grids used to monitor the perceptions of recovering psychotic 
# patients. In A. Thomson & P. Cummins (Eds) European 
# Perspectives in Personal Construct Psychology. Lincoln UK: 
# European Personal Construct Association.
# --> ask Richard for paper
# statsGoldenSection


getScoreDataFrame <- function(x){
  sc <- x@ratings[,,1]
  rownames(sc) <- getConstructNames(x)$l
  colnames(sc) <- getElementNames(x)
  sc
}

# disc    element to be used as discrepancy
# remove  logical. remove element that was used as discrepancy
#
makeDiscrepancy <- function(x, disc, remove=TRUE){
  sc <- x@ratings[,,1]
  colnames(sc) <- getElementNames(x)
  scDiscElement <- sc[, disc]
  if (remove)
    sc[, -disc] - scDiscElement else 
    sc[,] - scDiscElement
}

statsDiscrepancy <- function(x, disc, sort=TRUE){
  a <- describe(makeDiscrepancy(x, disc))
  if (sort)
    a[order(a$mean),] else
    a 
}




#' Distance measures (between constructs or elements).
#'
#' Various distance measures between elements or constructs are calculated.
#'
#' @param x           \code{repgrid} object.
#' @param along       Whether to calculate distance for 1 = constructs (default) 
#'                    or for 2= elements.
#' @param dmethod     The distance measure to be used. This must be one of 
#'                    "euclidean", "maximum", "manhattan", "canberra", "binary" 
#'                    or "minkowski". Any unambiguous substring can be given. 
#'                    For additional information on the different types type
#'                    \code{?dist}. 
#' @param  p          The power of the Minkowski distance, in case \code{"minkowski"}
#'                    is used as argument for \code{dmethod}.
#' @param trim        The number of characters a construct is trimmed to (default is
#'                    \code{20}). If \code{NA} no trimming occurs. Trimming
#'                    simply saves space when displaying correlation of constructs
#'                    with long names.
#' @param index       Whether to print the number of the construct 
#'                    (default is \code{TRUE}).
#' @param col.index   Logical. Whether to add an extra index column so the 
#'                    column names are indexes instead of construct names. This option 
#'                    renders a neater output as long construct names will stretch 
#'                    the output (default is \code{FALSE}).
#' @param digits      Numeric. Number of digits to round to (default is 
#'                    \code{2}).
#' @param output      The type of output printed to the console. \code{output=0}
#'                    will supress printing of the output.
#' @param upper       Whether to display upper triangle of correlation matrix only 
#'                    (default is \code{TRUE}).
#' @param ...         Additional parameters to be passed to function \code{dist}.
#'                    Type \code{dist} for further information. 
#' @return            \code{matrix} object.
#'
#' @author            Mark Heckmann
#' @export
#' @examples \dontrun{
#'
#'    # between constructs
#'    distance(bell2010, along=1)
#'    # between elements
#'    distance(bell2010, along=2)
#'  
#'    # several distance methods
#'    distance(bell2010, dm="man")         # manhattan distance
#'    distance(bell2010, dm="mink", p=3)   # minkowski metric to the power of 3
#'
#'    # to save the results without printing to the console
#'    d <- distance(bell2010, out=0, trim=7)
#'    d
#'
#' }
#'
distance <- function(x, along=1, dmethod="euclidean", 
                     p=2, trim=20, index=TRUE, col.index=FALSE, digits=2, 
                     output=1, upper=T, ...)
{
  dmethods <- c("euclidean", "maximum", "manhattan",    # possible distance methods
                "canberra", "binary", "minkowski")
  dmethod <- match.arg(dmethod, dmethods)                        # match method
  if (!inherits(x, "repgrid")) 							            # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  
  r <- getRatingLayer(x, trim = trim)
  if (along == 2)
    r <- t(r)
  d <- dist(r, method = dmethod, p = p, ...)
  d <- as.matrix(round(d, digits))
  d <- addNamesToMatrix2(x, d, index=index, trim=trim, along=along)
  out <- d
  d <- format(d, nsmall=digits)
  
  # console output
  if (output == 1){
    if (upper)
      d[lower.tri(d, diag=T)] <- paste(rep(" ", digits + 1), collapse="", sep="")
    if (col.index)                                   # make index column for neater colnames
      d <- addIndexColumnToMatrix(d) else
      colnames(d) <- seq_len(ncol(d))
    d <- as.data.frame(d)
    
    if (along == 1){ 
      cat("\n############################")
      cat("\nDistances between constructs") 
      cat("\n############################")
    } else if (along == 2){
      cat("\n##########################")
      cat("\nDistances between elements") 
      cat("\n##########################")
    }
    cat("\n\nDistance method: ", dmethod, "\n\n")
    print(d)
  }
  invisible(out)
}



# Measures comparing Constructs: Intensity, Cognitive Complexity 
# and other measures of Grid variation.

# Ward Hierarchical Clustering
# cluster <- function(){
#   
#   
# }
# a <- doubleEntry(x)
# sc <- getScoreDataFrame(x)
# d <- dist(sc, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward")
# plot(as.dendrogram(fit), horiz = F)
# plot(as.dendrogram(fit), horiz = F)


###############################################################################
# order elements and constructs by angles in first two dimensions from
# singular value decomposition approach (cf. Raeithel ???)
###############################################################################


#' Calculate angles for points in first two columns.
#'
#' The angles of the points given by the values in the first and second
#' column of a matrix seen from the origin are calulated in degrees.
#'
#' @param x           A matrix.
#' @param dim         Dimensions used for calculating angles.
#' @param clockwise   Logical. Positive angles are clockwise with x axis as 
#'                    basis.
#' @return  vector.   The angles of each row point with the origin as reference.
#'
#' @author       Mark Heckmann
#' @keywords internal
#'
#' @examples \dontrun{
#'
#'    calcAngles(matrix(rnorm(9), 3))
#' }
#'
calcAngles <- function(x, dim=c(1,2), clockwise=TRUE){
  angles <- atan2(x[ ,dim[2]], x[ ,dim[1]]) / (2 * pi / 360)
  angles <- angles * -1                    # positive angles are counted clockwise atan2 does anticlockwise by default
  angles[angles < 0] <- 360 + angles[angles < 0]  # map to 0 to 360 degrees i.e. only positive  values for ordering
  if (!clockwise)
    angles <- 360 - angles
  angles
}


#' Make indexes to order grid by angles in given dimensions.
#'
#' Reorder indexes for constructs and elements are calculated 
#' using the coordinates of the given dimensions.
#'
#' @param x           \code{repgrid} object that has been submitted to
#'                    \code{\link{calcBiplotCoords}}.
#' @param dim         Dimensions used to calculate angles for reordering grid.
#' @param clockwise   Logical. Positive angles are clockwise with x axis as 
#'                    basis.
#' @return  A list containing the indexes to reorder the grid. The 
#'          first list element for the constructs, the second for the elements indexes.
#'
#' @author        Mark Heckmann
#' @keywords internal
#' @examples \dontrun{
#'
#'    x <- randomGrid(15,30)      # make random grid
#'    i <- angleOrderIndexes2d(x) # make indexes for ordering
#'    x <- x[i[[1]], i[[2]]]      # reorder constructs and elements
#'    x                           # print grid
#' }
#'                  
angleOrderIndexes2d <- function(x, dim=c(1,2), clockwise=TRUE){
  if (!inherits(x, "repgrid")) 							      # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'") 
  E.mat <- x@calcs$biplot$elem
  C.mat <- x@calcs$biplot$con
  C.angles <- calcAngles(C.mat, dim=dim, clockwise=clockwise)
  E.angles <- calcAngles(E.mat, dim=dim, clockwise=clockwise)
  list(c.order=order(C.angles), 
       e.order=order(E.angles))
}


#' Order grid by angles between construct and/or elements in 2D.
#'
#' The approach is to reorder 
#' the grid matrix by their polar angles on the first two principal 
#' components from a data reduction technique 
#' (here the biplot, i.e. SVD). The function 
#' \code{reorder2d} reorders the grid according to the angles 
#' between the x-axis and the element (construct) 
#' vectors derived from a 2D biplot solution.
#' This approach is apt to identify circumplex 
#' structures in data indicated by the diagonal stripe 
#' in the display (see examples).
#'
#' @param x           \code{repgrid} object.
#' @param dim         Dimension of 2D solution used to calculate angles
#'                    (default \code{c(1,2)}).
#' @param center		  Numeric. The type of centering to be performed. 
#'                    0= no centering, 1= row mean centering (construct), 
#'                    2= column mean centering (elements), 3= double-centering (construct and element means),
#'                    4= midpoint centering of rows (constructs).
#'                    The default is \code{1} (row centering).
#' @param normalize   A numeric value indicating along what direction (rows, columns)
#'                    to normalize by standard deviations. \code{0 = none, 1= rows, 2 = columns}
#'                    (default is \code{0}).
#' @param g           Power of the singular value matrix assigned to the left singular 
#'                    vectors, i.e. the constructs.
#' @param h           Power of the singular value matrix assigned to the right singular 
#'                    vectors, i.e. the elements.
#' @param rc          Logical. Reorder constructs by similarity (default \code{TRUE}).
#' @param re          Logical. Reorder elements by similarity (default \code{TRUE}).
#' @param ...         Not evaluated.
#'
#' @return            Reordered \code{repgrid} object. 
#'
#' @author            Mark Heckmann
#' @export
#'
#' @examples \dontrun{
#'
#'    x <- feixas2004  
#'    reorder2d(x)            # reorder grid by angles in first two dimensions
#'    reorder2d(x, rc=F)      # reorder elements only
#'    reorder2d(x, re=F)      # reorder constructs only
#' }
#'
reorder2d <- function(x, dim=c(1,2), center=1, normalize=0, g=0, h=1-g, 
                    rc=TRUE, re=TRUE, ... ){
  if (!inherits(x, "repgrid")) 							  # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  x <- calcBiplotCoords(x, center=center, normalize=normalize, g=g, h=h, ...)
  i <- angleOrderIndexes2d(x, dim=dim)        # make indexes for ordering
  if(rc)
    x <- x[i[[1]], ,drop=FALSE]
  if(re)
    x <- x[ ,i[[2]], drop=FALSE]
  x
}



  
  
  
###############################################################################


#' Calculate the correlations between elements.
#' 
#' Note that simple element correlations as a measure of similarity
#' are flawed as they are not invariant to construct reflection (Mackay, 1992; 
#' Bell, 2010). A correlation index invariant to construct reflection is 
#' Cohen's rc measure (1969), which can be calculated using the argument 
#' \code{rc=TRUE} which is the default option.
#'
#' @param x         \code{repgrid} object.
#' @param rc        Use Cohen's rc which is invariant to construct 
#'                  reflection (see notes). It is used as default.
#' @param method    A character string indicating which correlation coefficient 
#'                  is to be computed. One of \code{"pearson"} (default), 
#'                  \code{"kendall"} or \code{"spearman"}, can be abbreviated.
#'                  The default is \code{"pearson"}.
#' @param trim      The number of characters a construct is trimmed to (default is
#'                  \code{20}). If \code{NA} no trimming occurs. Trimming
#'                  simply saves space when displaying correlation of constructs
#'                  with long names.
#' @param index     Whether to print the number of the construct. 
#' @param col.index Logical. Whether to add an extra index column so the 
#'                  column names are indexes instead of construct names. This option 
#'                  renders a neater output as long construct names will stretch 
#'                  the output (default is \code{FALSE}).
#' @param digits    Numeric. Number of digits to round to (default is 
#'                  \code{2}).
#' @param output    The type of output printed to the console. \code{output=0}
#'                  will supress printing of the output.
#' @param upper     Whether to display upper triangle of correlation matrix only 
#'                 (default is \code{TRUE}).
#'
#' @references      Bell, R. C. (2010). A note on aligning constructs. 
#'                  \emph{Personal Construct Theory & Practice}, (7), 42-48.
#'
#'                  Cohen, J. (1969). rc: A profile similarity coefficient 
#'                  invariant over variable reflection. \emph{Psychological 
#'                  Bulletin, 71}(4), 281-284.
#'
#'                  Mackay, N. (1992). Identification, Reflection, 
#'                  and Correlation: Problems In The Bases Of Repertory 
#'                  Grid Measures. \emph{International Journal of Personal
#'                  Construct Psychology, 5}(1), 57-75. 
#'
#' @return  \code{matrix} of element correlations
#'
#' @author        Mark Heckmann
#' @export
#' @seealso \code{\link{constructCor}}
#'
#' @examples \dontrun{
#'
#'    elementCor(mackay1992)                      # Cohen's rc
#'    elementCor(mackay1992, rc=F)                # PM correlation
#'    elementCor(mackay1992, rc=F, meth="spear")  # Spearman correlation
#'
#'    # format output
#'    elementCor(mackay1992, upper=F)   
#'    elementCor(mackay1992, col.index=F, trim=6)
#'    elementCor(mackay1992, index=T, col.index=F, trim=6)
#'
#'    # save as object for further processing.
#'    # no visible output.
#'    r <- elementCor(mackay1992, trim=6, out=0)
#'    r
#'
#' }
#'
elementCor <- function(x, rc=TRUE, method = c("pearson", "kendall", "spearman"), 
                          trim=20, index=FALSE, col.index=TRUE, digits=2, 
                          output=1, upper=T){
  method <- match.arg(method)
  if (!inherits(x, "repgrid")) 							# check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  if (rc){
    x <- doubleEntry(x)                     # double entry to get rc correlation
    method <- "pearson"                     # Cohen's rc is only defined for pearson correlation
  }
  scores <- getRatingLayer(x)
  res <- cor(scores, method=method)
  res <- round(res, digits)
  
  res <- addNamesToMatrix2(x, res, index=index, trim=trim, along=2)
  out <- res
  res <- format(res, nsmall=digits)
  
  # console output
  if (output == 1){
    if (upper)
      res[lower.tri(res, diag=T)] <- paste(rep(" ", digits + 1), collapse="", sep="")
    if (col.index)                                   # make index column for neater colnames
      res <- addIndexColumnToMatrix(res) 
    res <- as.data.frame(res)
    cat("\n############################")
    cat("\nCorrelation between elements")
    cat("\n############################")
    if (rc)
      method <- "Cohens's rc"
    cat("\n\nType of correlation: ", method, "\n\n")
    print(res)
  }
  invisible(out) 
}



###############################################################################

#' Calculate the correlations between constructs. 
#'
#' Different types of correlations can be requested: 
#' PMC, Kendall tau rank correlation, Spearman rank correlation.
#'
#' @param x         \code{repgrid} object.
#' @param method    A character string indicating which correlation coefficient 
#'                  is to be computed. One of \code{"pearson"} (default), 
#'                  \code{"kendall"} or \code{"spearman"}, can be abbreviated.
#'                  The default is \code{"pearson"}.
#' @param trim      The number of characters a construct is trimmed to (default is
#'                  \code{20}). If \code{NA} no trimming occurs. Trimming
#'                  simply saves space when displaying correlation of constructs
#'                  with long names.
#' @param index     Whether to print the number of the construct. 
#' @param col.index Logical. Whether to add an extra index column so the 
#'                  column names are indexes instead of construct names. This option 
#'                  renders a neater output as long construct names will stretch 
#'                  the output (default is \code{FALSE}).
#' @param digits    Numeric. Number of digits to round to (default is 
#'                  \code{2}).
#' @param output    The type of output printed to the console. \code{output=0}
#'                  will supress printing of the output. \code{output=1} will print
#'                  results to the screen. \code{output=2} will surpress printing
#'                  but return a matrix ready for printing.
#' @param upper     Whether to display upper triangle of correlation matrix only 
#'                 (default is \code{TRUE}).
#' @return          Prints results to the console and invisibly
#'                  returns a matrix of construct correlations.
#'
#' @author          Mark Heckmann
#' @export
#' @seealso \code{\link{elementCor}}
#'
#' @examples \dontrun{
#'
#'    # three different types of correlations
#'    constructCor(mackay1992)                
#'    constructCor(mackay1992, meth="kend")
#'    constructCor(mackay1992, meth="spea")
#'
#'    # format output
#'    constructCor(mackay1992, upper=F)   
#'    constructCor(mackay1992, col.index=F, trim=6)
#'    constructCor(mackay1992, index=T, col.index=F, trim=6)
#'    
#'    # save as object for further processing.
#'    # no visible output.
#'    r <- constructCor(mackay1992, trim=6, out=0)
#'    r
#'
#' }
#'
constructCor <- function(x, method = c("pearson", "kendall", "spearman"), 
                         trim=20, index=FALSE, col.index=TRUE, digits=2, 
                         output=1, upper=T){
  if (!inherits(x, "repgrid")) 							      # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  method <- match.arg(method)
  scores <- getRatingLayer(x)
  res <- cor(t(scores), method=method)
  res <- round(res, digits)
  res <- addNamesToMatrix2(x, res, index=index, trim=trim)
  out <- res
  res <- format(res, nsmall=digits)
  
  # console output
  if (output %in% 1:2){
    if (upper)
      res[lower.tri(res, diag=T)] <- paste(rep(" ", digits + 1), collapse="", sep="")
    if (col.index)                                   # make index column for neater colnames
      res <- addIndexColumnToMatrix(res) else
      colnames(res) <- seq_len(ncol(res))
    res <- as.data.frame(res)
    if (output == 1){
      cat("\n##############################")
      cat("\nCorrelation between constructs")
      cat("\n##############################")
      cat("\n\nType of correlation: ", method, "\n\n")
      print(res)
    } else {
      out <- res 
    }
  }   
  invisible(out)
}


#' Root mean square (RMS) of inter-construct correlations.
#'
#' The RMS is also known as 'quadratic mean' of 
#' the inter-construct correlations. The RMS serves as a simplification of the 
#' correlation table. It reflects the average relation of one construct to all 
#' other constructs. Note that as the correlations are squared during its calculation, 
#' the RMS is not affected by the sign of the correlation (cf. Fransella, 
#' Bell & Bannister, 2003, p. 86).
#'
#' @param x       \code{repgrid} object
#' @param method  a character string indicating which correlation coefficient 
#'                is to be computed. One of \code{"pearson"} (default), 
#'                \code{"kendall"} or \code{"spearman"}, can be abbreviated.
#'                The default is \code{"pearson"}.
#' @param trim    the number of characters a construct is trimmed to (default is
#'                \code{7}). If \code{NA} no trimming occurs. Trimming
#'                simply saves space when displaying correlation of constructs
#'                with long names.
#' @param digits  \code{numeric}. Number of digits to round to (default is 
#'                \code{2}).
#' @param output  The type of output printed to the console. \code{output=0}
#'                will supress printing of the output.
#' @return        \code{dataframe} of the RMS of inter-construct correlations
#'
#' @author        Mark Heckmann
#' @export
#' @seealso \code{\link{constructCor}}
#'
#' @references    Fransella, F., Bell, R. C., & Bannister, D. (2003). 
#'                A Manual for Repertory 
#'                Grid Technique (2. Ed.). Chichester: John Wiley & Sons.
#'
#' @examples \dontrun{
#'
#'    # data from grid manual by Fransella, Bell and Bannister
#'    constructRmsCor(fbb2003)    
#'    constructRmsCor(fbb2003, trim=20, digits=3)
#'    
#'    # calculate invisibly
#'    r <- constructRmsCor(fbb2003, out=0) 
#'    r
#'
#' }
#'
constructRmsCor <- function(x, method = c("pearson", "kendall", "spearman"), 
                            trim=NA, digits=2, output=1){
  method <- match.arg(method)
  res <- constructCor(x, method = method, trim=trim,    # calc correlations
                      col.index=FALSE, digits=10, output=0)
  diag(res) <- NA                                       # remove diagonal 
  res <- apply(res^2, 1, mean, na.rm=TRUE)              # mean of squared values
  res <- data.frame(RMS=res^.5)                         # root of mean squares
  res <- round(res, digits)
  
  # console output
  if (output){
    cat("\n##########################################")
    cat("\nRoot-mean-square correlation of constructs")
    cat("\n##########################################\n\n")
    print(res)                             
    cat("\naverage of statistic", round(mean(res, na.rm=TRUE), digits), "\n")
    cat("standard deviation of statistic", round(sdpop(res, na.rm=TRUE), digits), "\n")
  }
  invisible(res)
}



#' Calculate Somers' d for the constructs. 
#'
#' Somer'ss d is an 
#' assymetric association measure as it depends on which 
#' variable is set as dependent and independent.
#' The direction of dependency needs to be specified.
#'
#' @param x           \code{repgrid} object
#' @param dependent   A string denoting the direction of dependency in the output 
#'                    table (as d is assymetrical). Possible values are \code{"c"}
#'                    (the default) for setting the columns as dependent, \code{"r"} 
#'                    for setting the rows as the dependent variable and \code{"s"} for the 
#'                    symmetrical Somers' d measure (the mean of the two directional 
#'                    values for code{"c"} and \code{"r"}).
#' @param trim        The number of characters a construct is trimmed to (default is
#'                    \code{30}). If \code{NA} no trimming occurs. Trimming
#'                    simply saves space when displaying correlation of constructs
#'                    with long names.
#' @param index       Whether to print the number of the construct 
#'                    (default is \code{TRUE}). 
#' @param col.index   Logical. Wether to add an extra index column so the 
#'                    column names are indexes instead of construct names. This option 
#'                    renders a neater output as long construct names will stretch 
#'                    the output (default is \code{FALSE}).
#' @param digits      Numeric. Number of digits to round to (default is 
#'                    \code{2}).
#' @param output      The type of output printed to the console. \code{output=0}
#'                    will supress printing of the output. \code{output=1} (default) will print
#'                    results to the screen. 
#' @return            \code{matrix} of construct correlations.
#' @note              Thanks to Marc Schwartz for supplying the code to calculate
#'                    Somers' d.
#' @references        Somers, R. H. (1962). A New Asymmetric Measure of Association
#'                    for Ordinal Variables. \emph{American Sociological Review, 27}(6),
#'                    799-811.
#'
#' @author        Mark Heckmann
#' @export
#'
#' @examples \dontrun{
#'
#'    constructD(fbb2003)       # columns as dependent (default)
#'    constructD(fbb2003, "c")  # row as dependent
#'    constructD(fbb2003, "s")  # symmetrical index
#'  
#'    # surpress printing
#'    d <- constructD(fbb2003, out=0, trim=5)
#'    d
#'    
#'    # more digits
#'    constructD(fbb2003, dig=3)
#'
#'    # add index column, no trimming
#'    constructD(fbb2003, col.index=TRUE, index=F, trim=NA)  
#'
#' }
#'
constructD <- function(x, dependent = "c", 
                       trim=30, index=T, col.index=F, digits=1, output=1){
  if (!inherits(x, "repgrid")) 							    # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  scores <- getRatingLayer(x)
  l <- lapply(as.data.frame(t(scores)),  I)     # put each row into a list
    
  somersd <- function(x, y, dependent, smin, smax){
    na.index <- is.na(x) | is.na(y)
    x <- x[!na.index]
    y <- y[!na.index]
    x <- factor(unlist(x), levels=seq(smin, smax))
    y <- factor(unlist(y), levels=seq(smin, smax))
    m <-  as.matrix(table(x,y))
    
    if (dependent == "r")
      i <- 1 else 
    if (dependent == "c")
      i <- 2 else i <- 3
    calc.Sd(m)[[i]]
  }  
  
  nc <- length(l)
  smin <- x@scale$min
  smax <- x@scale$max
  sds <- mapply(somersd, rep(l,each=nc), rep(l, nc), 
                MoreArgs=list(dependent=dependent, 
                              smin=smin, smax=smax))
  res <- matrix(sds, nc)
  res <- addNamesToMatrix2(x, res, index=index, trim=trim, along=1)
  res <- round(res, digits)
  out <- res
  
  # console output
  if (output == 1){
    cat("\n#########")
    cat("\nSomers' D")
    cat("\n#########\n\n")
    dep <- match.arg(dependent, c("columns", "rows"))
    cat("Direction:", dep, "are set as dependent\n")
    if (col.index)                                  # make index column for neater colnames
      res <- addIndexColumnToMatrix(res) else
      colnames(res) <- seq_len(ncol(res))
    print(res)
  }
  invisible(out)
}



#' Principal component analysis (PCA) of inter-construct correlations.
#'
#' Various methods for rotation and methods for the calculation of the 
#' correlations are available. Note that the number of factors
#' has to be specified. For more information on the PCA function itself type 
#' \code{?principal}. 
#'
#' @param x           \code{repgrid} object.
#' @param nfactors    Number of components to extract (default is \code{3}).
#' @param rotate      \code{"none"}, \code{"varimax"}, \code{"promax"} and \code{"cluster"} 
#'                    are possible rotations (default is \code{none}).
#' @param method      A character string indicating which correlation coefficient 
#'                    is to be computed. One of \code{"pearson"} (default), 
#'                    \code{"kendall"} or \code{"spearman"}, can be abbreviated.
#'                    The default is \code{"pearson"}.
#' @param trim        The number of characters a construct is trimmed to (default is
#'                    \code{7}). If \code{NA} no trimming occurs. Trimming
#'                    simply saves space when displaying correlation of constructs
#'                    with long names.
#' @param digits      Numeric. Number of digits to round to (default is 
#'                    \code{2}).
#' @param cutoff      Loadings smaller than cutoff are not printed.
#' @param output      The type of output printed to the console. \code{output=0}
#'                    will supress printing of the output. \code{output=1} (default) will print
#'                    results to the screen. \code{output==2} will invisibly retrn the whole 
#'                    object from \code{principal} not only the loadings.
#' @return            Invisibly returns a matrix of loadings on the principal components.
#'
#' @author            Mark Heckmann
#' @export
#'
#' @references        Fransella, F., Bell, R. & Bannister, D. (2003). \emph{A Manual for Repertory 
#'                    Grid Technique} (2. Ed.). Chichester: John Wiley & Sons.
#'
#' @examples \dontrun{
#'
#'    # data from grid manual by Fransella et al. (2003, p. 87)
#'    # note that the construct order is different
#'    constructPca(fbb2003, nf=2)
#'
#'    # surpress printing to console            
#'    m <- constructPca(fbb2003, nf=2)
#'    m
#'
#'    # no rotation
#'    constructPca(fbb2003, rotate="none")
#'    
#'    # using a different correlation matrix (Spearman)
#'    constructPca(fbb2003, method="spearman")  
#'
#' }
#'
constructPca <- function(x, nfactors=3, rotate="varimax", 
                         method = c("pearson", "kendall", "spearman"), 
                         trim=NA, digits=2, cutoff=0, output=1) {
  rotate <- match.arg(rotate, c("none", "varimax", "promax", "cluster"))
  if (!rotate %in% c("none", "varimax", "promax", "cluster"))
    stop('only "none", "varimax", "promax" and "cluster" are possible rotations')
  
  res <- constructCor(x, method=method, trim=trim,          # calc inter constructs correations
                      digits=10, output=0)
  pc <- principal(res, nfactors = nfactors, rotate=rotate)  # make PCA

  # console output
  if (output == 1){
    cat("\n##############")
    cat("\nConstruct PCA")
    cat("\n##############\n")
    
    cat("\nNumber of components extracted:", nfactors)
    cat("\nType of rotation:", rotate, "\n")
    print(loadings(pc), cutoff=cutoff, digits=digits)  
  }
  
  # return loadings or whole principal object
  if (output == 2)
    invisible(pc) else
    invisible(round(loadings(pc)[,], digits))       
}



#' Align constructs by loadings on first pricipal component. 
#'
#' In case a construct loads negatively on the first principal
#' component, the function \code{alignByLoadings} will reverse it 
#' so that all constructs have positive loadings on the first 
#' principal component (see deatil section for more).
#'
#' The direction of the constructs in a grid is arbitrary and 
#' a reflection of a scale does not affect the information 
#' contained in the grid. Nonetheless, the direction of a scale 
#' has an effect on inter-element correlations (Mackay, 1992) 
#' and on the spatial representation and clustering of the grid 
#' (Bell, 2010). Hence, it is desirable to follow a protocol to align 
#' constructs that will render unique results. A common approach 
#' is to align constructs by pole preference, but this information 
#' is not always accessible. Bell (2010) proposed another solution for
#'  the problem of construct 
#' alignment. As a unique protocol he suggests to align constructs 
#' in a way so they all have positive loadings on the first 
#' component of a grid PCA.
#'
#' @param x         \code{repgrid} object.
#' @param trim      The number of characters a construct is trimmed to (default is
#'                  \code{10}). If \code{NA} no trimming is done. Trimming
#'                  simply saves space when displaying the output.
#' @param output    Numeric. The function returns either the \code{repgrid}
#'                  object with aligned constructs (\code{output=0 or 1}), additionaly prints 
#'                  the calculation results to the console (\code{output=1}) or returns 
#'                  a \code{list} containing the calculation results, i.e. 
#'                  correlation matrices before and after reversal, loadings 
#'                  on first PC before and after reversal and a list (printout)
#'                  of the constructs that have been reversed.
#' @param digits    Numeric. Number of digits to round to (default is 
#'                  \code{2}).
#' @return          Aligned \code{repgrid} object for \code{output=0 or 1} or a 
#'                  list for \code{output=2}.
#'
#' @note            Bell (2010) proposed a solution for the problem of construct 
#'                  alignment. As construct reversal has an effect on element 
#'                  correlation and thus on any measure that based on element 
#'                  correlation (Mackay, 1992), it is desireable to have a 
#'                  standard method for 
#'                  construct alignment independently from its semantics (preferred 
#'                  pole etc.). Bell (2010) proposes to align constructs in a way
#'                  so they all have positive loadings on the first component of a 
#'                  grid PCA.
#' 
#' @references      Bell, R. C. (2010). A note on aligning constructs.
#'                  \emph{Personal Construct Theory & Practice, 7}, 42-48.
#' 
#'                  Mackay, N. (1992). Identification, Reflection, 
#'                  and Correlation: Problems in ihe bases of repertory 
#'                  grid measures. \emph{International Journal of Personal
#'                  Construct Psychology, 5}(1), 57-75. 
#'
#' @author          Mark Heckmann
#' @export
#'
#' @seealso \code{\link{alignByIdeal}}
#'
#' @examples \dontrun{
#'
#'   # reproduction of the example in the Bell (2010)
#'   bell2010                    # show grid
#'   alignByLoadings(bell2010)   # constructs aligned by loadings on PC 1
#'
#'   alignByLoadings(bell2010, output=2)  # show results
#'
#' }
#'
alignByLoadings <- function(x, trim=20, output=0, digits=1){
  options(warn=1)                                    # surpress warnings (TODO sometimes error in SVD due to singularities in grid)
  ccor.old <- constructCor(x, trim=trim, 
                          output=0)                  # construct correlation unreversed
  pc.old <- principal(ccor.old)                      # calc principal component (psych pkg)
  reverseIndex <- 
    which(pc.old$loadings[ ,1] < 0)                  # which constructs to reverse
  x2 <- swapPoles(x, reverseIndex)                   # reverse constructs
  ccor.new <- constructCor(x2, trim=trim, output=0)  # correlation with reversed constructs
  pc.new <- principal(constructCor(x2, output=0))    # 2nd principal comps
  options(warn=0)                                    # reset to do warnings
  
  # output to concole
  if (output == 1){
    cat("\n###################################")
    cat("\nAlignment of constructs by loadings")
    cat("\n###################################\n")
    
    cat("\nConstruct correlations - before alignment\n\n")
    print(constructCor(x, trim=trim, col.index=F, index=T, 
                       output=2, digits=digits))
    
    cat("\nConstruct factor loadiongs on PC1 - before alignment\n\n")
    print(round(pc.old$loadings[,1, drop=F], digits))
    
    cat("\nThe following constructs are reversed:\n\n")
    reversed <- data.frame(index=reverseIndex)
    if (dim(reversed)[1] == 0){
      cat("None. All constructs are already aligned accordingly.\n")
    } else {
      print(reversed)
    }
    
    cat("\nConstruct correlations - after alignment\n\n")
    print(constructCor(x2, trim=trim, col.index=F, index=T, 
                       output=2, digits=digits))    
    
    cat("\nConstruct factor loadings on PC1 - after alignment\n\n")
    print(round(pc.new$loadings[,1, drop=F], digits))
    cat("\n\n")
    invisible(x2)
  } else if (output == 2) {
    return(list(cor.before=round(ccor.old, digits), 
                loadings.before=pc.old$loadings[,1, drop=F],
                reversed=data.frame(index=reverseIndex),
                cor.after=round(ccor.new, digits), 
                loadings.after=pc.new$loadings[,1, drop=F]))
  } else 
    return(x2)
}



###############################################################################


#' Align constructs using the ideal element to gain pole preferences.
#'
#' The direction of the constructs in a grid is arbitrary and a reflection of 
#' a scale does not affect the information contained in the grid. 
#' Nonetheless, the direction of a scale has an effect on inter-element 
#' correlations (Mackay, 1992) and on the spatial representation and clustering
#' of the grid (Bell, 2010). Hence, it is desirable to follow a protocol to 
#' align constructs that will render unique results. A common approach is
#' to align constucts by pole preference, i. e. aligninig all positive and  
#' negative poles. This can e. g. be achieved using \code{\link{swapPoles}}.
#' If an ideal element is present, this element can be used to identify
#' the positive and negative pole. The function \code{alignByIdeal} will
#' align the constructs accordingly. Note that this approach does not always 
#' yield definite results as sometimes ratings do not show a clear 
#' preference for one pole (Winter, Bell & Watson, 2010).
#' If a preference cannot be determined definitely,
#' the construct direction remains unchanged (a warning is issued in that case).
#' 
#' @param x       \code{repgrid} object
#' @param ideal   Number of the element that is used for alignment 
#'                (the ideal).
#' @param high    Logical. Whether to align the constructs so the ideal 
#'                will have high
#'                ratings on the constructs (i.e. \code{TRUE}, default) or low
#'                ratings (\code{FALSE}). High scores will lead to the preference pole
#'                on the right side, low scores will align the preference pole
#'                on the left side.
#'
#' @return        \code{repgrid} object with aligned constructs.
#'
#' @references    Bell, R. C. (2010). A note on aligning constructs.
#'                \emph{Personal Construct Theory & Practice, 7}, 42-48.
#' 
#'                Mackay, N. (1992). Identification, Reflection, 
#'                and Correlation: Problems in ihe bases of repertory 
#'                grid measures. \emph{International Journal of Personal
#'                Construct Psychology, 5}(1), 57-75. 
#'          
#'                Winter, D. A., Bell, R. C., & Watson, S. (2010). Midpoint 
#'                ratings on personal constructs: Constriction or the middle 
#'                way? \emph{Journal of Constructivist Psychology, 23}(4), 337-356.
#'
#' @export
#' @author    Mark Heckmann
#' @seealso \code{\link{alignByLoadings}}
#'
#' @examples \dontrun{
#'
#'   feixas2004                         # original grid
#'   alignByIdeal(feixas2004, 13)       # aligned with preference pole on the right
#'
#'   raeithel                           # original grid
#'   alignByIdeal(raeithel, 3, high=F)  # aligned with preference pole on the left
#'
#' }
#'
alignByIdeal <- function(x, ideal, high=TRUE){
  if (!inherits(x, "repgrid")) 							  # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  	
  idealRatings <- getRatingLayer(x)[, ideal]
  unclear <- which(idealRatings == getScaleMidpoint(x))
  positive <- which(idealRatings > getScaleMidpoint(x))
  negative <- which(idealRatings < getScaleMidpoint(x))
  
  if (high)     
    x <- swapPoles(x, negative) else   # align such that ratings on ideal are high
    x <- swapPoles(x, positive)        # align such that ratings on ideal are low

  if (length(unclear) != 0)
    warning("The following constructs do not show a preference for either pole",
        "and have thus not been aligned: ", paste(unclear, collapse=","))
  x
}



#' Cluster analysis (of constructs or elements).
#'
#' \code{cluster} is a preliminary implementation of a cluster function. 
#' It supports
#' various distance measures as well as cluster methods. More is to come. 
#'
#' @param x          \code{repgrid} object.
#' @param along      Along which dimension to cluster. 1 = constructs only, 
#'                   2= elements only, 0=both (default).
#' @param dmethod    The distance measure to be used. This must be one of 
#'                   "euclidean", "maximum", "manhattan", "canberra", "binary" 
#'                   or "minkowski". Any unambiguous substring can be given. 
#'                   For additional information on the different types type
#'                   \code{?dist}. 
#' @param  cmethod   The agglomeration method to be used. This should be (an
#'                   unambiguous abbreviation of) one of \code{"ward"}, 
#'                   \code{"single"}, \code{"complete"}, \code{"average"}, 
#'                   \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.
#' @param  p         The power of the Minkowski distance, in case \code{"minkowski"}
#'                   is used as argument for \code{dmethod}.
#' @param trim       the number of characters a construct is trimmed to (default is
#'                   \code{10}). If \code{NA} no trimming is done. Trimming
#'                   simply saves space when displaying the output.
#' @param main       Title of plot. The default is a name indicating the distance 
#'                   function and cluster method.
#' @param mar        Define the plot region (bottom, left, upper, right).
#' @param cex        Size parameter for the nodes. Usually not needed.              
#' @param lab.cex    Size parameter for the constructs on the right side.
#' @param cex.main   Size parameter for the plot title (default is \code{.9}).
#' @param print      Logical. Wether to print the dendrogram (default is \code{TRUE}).
#' @param ...        Additional parameters to be passed to plotting function from
#'                   \code{as.dendrogram}. Type \code{?as.dendrogram} for further 
#'                   information. This option is usually not needed, except if special
#'                   designs are needed.
#' @return           Reordered \code{repgrid} object.
#'
#' @author            Mark Heckmann
#' @export
#' @seealso           \code{\link{bertinCluster}}
#'
#' @examples \dontrun{
#'
#'   cluster(bell2010)
#'   cluster(bell2010, main="My cluster analysis")   # new title
#'   cluster(bell2010, type="t")                     # different drawing style
#'   cluster(bell2010, dmethod="manhattan")          # using manhattan metric
#'   cluster(bell2010, cmethod="single")             # do single linkage clustering
#'   cluster(bell2010, cex=1, lab.cex=1)             # change appearance
#'   cluster(bell2010, lab.cex=.7,                   # advanced appearance changes
#'           edgePar = list(lty=1:2, col=2:1))
#' }
#'
cluster <- function(x, along=0, dmethod="euclidean", cmethod="ward", p=2, trim=NA,
                    main=NULL,
                    mar=c(4, 2, 3, 15), cex=0, lab.cex=.8, cex.main=.9, 
                    print=TRUE, ...){
  
  dmethods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  dmethod <- match.arg(dmethod, dmethods)
  
  cmethods <- c("ward", "single", "complete", "average", "mcquitty", "median", "centroid")
  cmethod <- match.arg(cmethod, cmethods)
  
  if (is.null(main))
    main <- paste(dmethod, "distance and", cmethod, "clustering")
  if (! along %in% 0:2)
    stop("'along must take the value 0, 1 or 2.")
    
  r <- getRatingLayer(x, trim=trim)             # get ratings

  # dendrogram for constructs
  if (along %in% 0:1){
    d <- dist(r, method = dmethod, p=p)           # make distance matrix for constructs
    fit.constructs <- hclust(d, method=cmethod)   # hclust object for constructs
    dend.con <- as.dendrogram(fit.constructs)
    con.ord <- order.dendrogram(rev(dend.con))
    x <- x[con.ord, ]                             # reorder repgrid object
    
  }
  
  if (along %in% c(0,2)){
    # dendrogram for elements
    d <- dist(t(r), method = dmethod, p=p)        # make distance matrix for elements
    fit.elements <- hclust(d, method=cmethod)     # hclust object for elements
    dend.el <- as.dendrogram(fit.elements)
    el.ord <- order.dendrogram(dend.el)
    x <- x[ , el.ord]                               # reorder repgrid object
  }
   
  if (print){                                       # print dendrogram?
    op <- par(mar=mar)                              # change mar settings and save old mar settings
    if (along == 0)
      layout(matrix(1:2, ncol=1))  
    if (along %in% c(0,1)){
      plot(dend.con, horiz=TRUE, main=main, xlab="",  # print cluster solution
           nodePar=list(cex=cex, lab.cex=lab.cex), 
           cex.main=cex.main, ...)
    }
    if (along %in% c(0,2)){
      plot(dend.el, horiz=TRUE, main=main, xlab="",  # print cluster solution
           nodePar=list(cex=cex, lab.cex=lab.cex), 
           cex.main=cex.main, ...)
    } 
    par(op)                                         # reset to old mar settings  
  }
  invisible(x)                                      # return reordered repgrid object
}











### using gridn graphics
# data(mtcars)
# x  <- t(as.matrix(scale(mtcars)))
# dd.row <- as.dendrogram(hclust(dist(x)))
# row.ord <- order.dendrogram(dd.row)
# 
# dd.col <- as.dendrogram(hclust(dist(t(x))))
# col.ord <- order.dendrogram(dd.col)
# library(latticeExtra)
# dendrogramGrob(dd.col)


# data(mtcars)
# x  <- t(as.matrix(scale(mtcars)))
# dd.row <- as.dendrogram(hclust(dist(x)))
# row.ord <- order.dendrogram(dd.row)
# 
# dd.col <- as.dendrogram(hclust(dist(t(x))))
# col.ord <- order.dendrogram(dd.col)
# 
# library(lattice)
# 
# levelplot(x[row.ord, col.ord],
#           aspect = "fill",
#           scales = list(x = list(rot = 90)),
#           colorkey = list(space = "left"),
#           legend =
#           list(right =
#                list(fun = dendrogramGrob,
#                     args =
#                     list(x = dd.col, ord = col.ord,
#                          side = "right",
#                          size = 10)),
#                top =
#                list(fun = dendrogramGrob,
#                     args =
#                     list(x = dd.row, 
#                          side = "top",
#                          type = "triangle"))))





#' Normalize rows or columns by its standard deviation.  
#'
#' @param  x          \code{matrix}
#' @param  normalize  A numeric value indicating along what direction (rows, columns)
#'                    to normalize by standard deviations. \code{0 = none, 1= rows, 2 = columns}
#'                    (default is \code{0}).
#' @param ...         Not evaluated.
#' @return            Not yet definde TODO!
#'
#' @author        Mark Heckmann
#' @export
#'
#' @examples \dontrun{
#'
#'  x <- matrix(sample(1:5, 20, rep=T), 4)
#'  normalize(x, 1)						      # normalizing rows
#'  normalize(x, 2)						      # normalizing columns
#' }
#'
normalize <- function(x, normalize=0, ...){
  if (!normalize %in% 0:2 )
    stop("along must take a numeric value:\n",
         "normalize, 0 = none, 1 = rows, 2=columns")
  if (normalize == 1){
    x <- t(x) 
    x <- scale(x, center=FALSE, scale=apply(x, 2, sd, na.rm=TRUE))  # see ? scale   
    x <- t(x)
  } else  if (normalize == 2){
    x <- scale(x, center=FALSE, scale=apply(x, 2, sd, na.rm=TRUE))  # see ? scale   
  }
  x        
}


#' Centering of rows (constructs) and/or columns (elements).
#'
#' @param  x          \code{repgrid} object.
#' @param  center		  Numeric. The type of centering to be performed. 
#'                    0= no centering, 1= row mean centering (construct), 
#'                    2= column mean centering (elements), 3= double-centering (construct and element means),
#'                    4= midpoint centering of rows (constructs).
#'                    of the scale(default \code{FALSE}). Default is \code{1} (row centering).
#' @param  ...		    Not evaluated.
#' @return  \code{matrix} containing the transformed values.
#'
#' @note  If scale midpoint centering is applied no row or column centering can be 
#'        applied simultaneously.
#'        TODO: After centering the standard representation mode does not work any more as 
#'         it remains unclear what color values to attach to the centered values.
#'
#' @author        Mark Heckmann
#' @export
#'
#' @examples \dontrun{
#'
#'  center(bell2010)						      # no centering
#'  center(bell2010, rows=T)				  # row centering of grid 
#'  center(bell2010, cols=T)				  # column centering of grid
#'  center(bell2010, rows=T, cols=T)	# row and column centering
#' }
#'
center <- function(x, center=1, ...){
	dat <- x@ratings[ , ,1]
	if (center == 0)        # no centering
	  res <- dat
	if (center == 1)        # center at row (construct) mean
	  res <- sweep(dat, 1, apply(dat, 1, mean, na.rm=TRUE))
	if (center == 2)        # center at column (element) mean
    res <- sweep(dat, 2, apply(dat, 2, mean, na.rm=TRUE))
	if (center == 3){       # center at row and column mean
    res <- sweep(dat, 1, apply(dat, 1, mean, na.rm=TRUE))
    res <- sweep(res, 2, apply(res, 2, mean, na.rm=TRUE))
  }
  if (center == 4)        # center at midpoint i.e. middle of scale
	  res <- dat - getScaleMidpoint(x)  
	res
}


#' Calculate SSQ (accuracy) of biplot representation for elements 
#' and constructs. 
#'
#' Each construct and element are vectors in a 
#' multidimensional space. When reducing the representation 
#' to a lower dimensional space, a loss
#' of information (sum-of-squares) will usually occur. The output of the function
#' shows the proportion of sum-of-squares (SSQ) explained for the elements 
#' (constructs) and the amount explained by each principal component. This 
#' allows to assess which elements (construct) are represented how well in the 
#' current representation. Also it shows how much of the total variation is
#' explained.
#'
#' @param x                 \code{repgrid} object.
#' @param along             Numeric. Table of sum-of-squares (SSQ) for 1=constructs, 2=elements. 
#'                          Note that currently these calculations only make sense
#'                          for biplot reperesentations with \code{g=1} and \code{h=1}
#'                          respectively.
#' @param cum               Logical. Return a cumulated table of sum-of-squares? 
#'                          (default is \code{TRUE}).
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
#' @param digits            Number of digits to round the output to.
#' @param print             Whether to print the oputut to the console (default \code{TRUE}).
#'
#' @return                  \code{dataframe} containing the explained (cumulated)  
#'                          sum-of-squares for each construct or element on each
#'                          principal component.
#' @note                    TODO: if g or h is not equal to 1 the SSQ does not measure
#'                          accuracy of representation as currently the ssq of each point
#'                          are set in constrast with the pre-transformed matrix.
#'
#' @author        Mark Heckmann
#' @keywords      internal
#' @export
#'
ssq <- function(x, along=2, cum=T, center=1, normalize=0, 
                g=0, h=1-g, col.active=NA, col.passive=NA, 
                digits=1, print=TRUE, ...){ 
  x <- calcBiplotCoords(x, center=center, normalize=normalize, 
                        g=g, h=h,  col.active= col.active, 
                        col.passive=col.passive)
  if (is.null(x@calcs$biplot))
    stop("biplot coordinates have not yet been calculated")
  
  E <- x@calcs$biplot$el
  C <- x@calcs$biplot$con
  X <- x@calcs$biplot$X
  
  enames <- getElementNames(x)
  cnames <- getConstructNames(x)$rightpole
  
  ssq.c <- diag(X %*% t(X))         # SSQ for each row (constructs)
  ssq.e <- diag(t(X) %*% X)         # SSQ for each column(element)
  ssq.c.prop <- C^2 / ssq.c         # ssq of row and dimension / total ssq element  
  ssq.e.prop <- E^2 / ssq.e         # ssq of element and dimension / total ssq element  
 
  rownames(ssq.c.prop) <- cnames    # add construct names to rows
  colnames(ssq.c.prop) <-           # add dimension labels to columns
    paste(1L:ncol(ssq.c.prop), "D", sep="") 
  rownames(ssq.e.prop) <- enames    # add element names to rows
  colnames(ssq.e.prop) <-           # add dimension labels to columns
    paste(1L:ncol(ssq.e.prop), "D", sep="")

  ssq.c.cumsum <- t(apply(ssq.c.prop, 1, cumsum))         # cumulated ssq of construct per dimension / total ssq element 
  ssq.e.cumsum <- t(apply(ssq.e.prop, 1, cumsum))         # cumulated ssq of elements per dimension / total ssq element 
  ssq.c.avg <- apply(C^2, 2, sum, na.rm=T) / sum(ssq.c)   # ssq per dimension / ssq total 
  ssq.e.avg <- apply(E^2, 2, sum, na.rm=T) / sum(ssq.e)   # ssq per dimension / ssq total
  ssq.c.avg.cumsum <- cumsum(ssq.c.avg)                     # cumulated ssq per dimension / ssq total
  ssq.e.avg.cumsum <- cumsum(ssq.e.avg)                     # cumulated ssq per dimension / ssq total
  ssq.c.table <- rbind(ssq.c.prop, TOTAL=ssq.c.avg)         # 
  ssq.e.table <- rbind(ssq.e.prop, TOTAL=ssq.e.avg)         # 
  ssq.c.table.cumsum <- rbind(ssq.c.cumsum, TOTAL=ssq.c.avg.cumsum)
  ssq.e.table.cumsum <- rbind(ssq.e.cumsum, TOTAL=ssq.e.avg.cumsum)
  ssq.c.table <- ssq.c.table * 100
  ssq.e.table <- ssq.e.table * 100
  ssq.c.table.cumsum <- ssq.c.table.cumsum * 100
  ssq.e.table.cumsum <- ssq.e.table.cumsum * 100
  
  if (along == 1){          # elements or constructs?
    ssq.table <- ssq.c.table
    ssq.table.cumsum <- ssq.c.table.cumsum
    along.text <- "constructs"
  } else {
    ssq.table <- ssq.e.table
    ssq.table.cumsum <- ssq.e.table.cumsum
    along.text <- "elements"
  }
  
  if (cum){                 # ouput cumulated table?
    ssq.out <- ssq.table.cumsum 
    cum.text <- "cumulated "
  } else {
    ssq.out <- ssq.table    
    cum.text <- ""
  }
  
  # printed output 
  if (print){
    cat("\n", cum.text, "proportion of explained sum-of-squares for the ", 
         along.text, "\n\n", sep="")
    print(round(ssq.out, digits))
    cat("\nTotal sum-of-squares of pre-transformed (i.e. centered, scaled) matrix:", sum(ssq.e))
  }
  
  invisible(ssq.out) 
}



