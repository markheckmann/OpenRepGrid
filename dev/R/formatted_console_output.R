# In R there are a lot conlose output functions available, like sprintf, cat, 
# default.print etc. Still I do not find it that simple to design the output 
# below unfortunalety I need it. So here it comes

### VERSION 1 ###
#                 c    c    c    c
#				          o    o    o    o
#				          l    l    l    l
#				          u    u    u    u
#				          m    m    m    m
#				          n    n    n    n
#				 
#				          1    2    3    4
#               ------------------
#	rowname 1	    1.2  3.2  2.1  2.0 
#	rowname 2	    1.0  2.1  2.0  1.8
#	rowname 3	    2.1  1.0  2.2  1.1
#	rowname 4     1.0  1.3  1.0  1.0


### VERSION 2 ###
#				        first column
#				        |  second column
#				        |  |  third column
#				        |  |  |  fourth column
# 			        |  |  |  |
# 		        	+--+--+--+
# 	rowname 1  	1  2  3  4
# 	rowname 2	  2  1  1  0
# 	rowname 3	  1  2  1  1
# 	rowname 4	  1  1  0  1

# Lets divide the output viewport into regions, like:
# lets see what functions we do have to output:
# -----------------------
# R functions to use:
# -----------------------
# encodeString
# cat
# print
# format
# sprintf
# format.default
#x <- c("a", "ab", "abcde")
#encodeString(x, width = NA) # left justification
#y <- format(x)

# Lets look at print.data.frame()
# This calls format which formats the data frame column-by-column, then 
# converts to a character matrix and dispatches to the print method for 
# matrices.

#simple appraoch
#df <- data.frame(A=1:10, B=1:10)
#df1 <- data.frame(A=c("a", ""), B=c("n", "m"), stringsAsFactors=F)

#rbind(df1, df)
#    A  B
#1   a  n
#2   b  m
#3   1  1
#4   2  2
#...

###############################################################################
dfOut1 <- function(df){
  colnames <- format(names(df), justify="r")
  colnames <- paste("  ", colnames, "_", sep="")
  colnamesAsLettersList <- strsplit(colnames, split="")
  dfColnames <- data.frame(colnamesAsLettersList, stringsAsFactors=F)
  names(dfColnames) <- LETTERS[seq_along(dfColnames)]
  names(df) <- names(dfColnames)
  df <- rbind(dfColnames, df)
  df  
}
# df <- data.frame(1:10, 1:10)
# names(df) <- c("Mein Leben aus Sicht meiner Eltern", "Mein Leben heute")
# rownames <- sapply(1:10, function(x) paste(sample(letters, sample(5:20, 1), rep=TRUE), collapse=""))
# rownames(df) <- rownames
# dfOut1(df)

###############################################################################
dfOut2 <- function(df){
  colnames <- format(names(df), justify="r")
  colnames <- paste("  ", colnames, "_", sep="")
  colnamesAsLettersList <- strsplit(colnames, split="")
  dfColnames <- data.frame(colnamesAsLettersList, stringsAsFactors=F)
  names(dfColnames) <- LETTERS[seq_along(dfColnames)]
  names(df) <- names(dfColnames)
  tmp <- rbind(dfColnames, df)
  tmp
}
# randomStrings <- function(n){
#   sapply(1:n, function(x) paste(sample(letters, sample(5:20, 1), rep=TRUE), 
#                           collapse=""))
# }
# df <- data.frame(Left.pole=randomStrings(10), B=1:10, C=1:10, D=1:10,
#                   Right.pole=randomStrings(10))
# names(df) <- c( "Left.pole", "Deutschland in den 60ern", 
#                 "Mein Leben aus Sicht meiner Eltern", 
#                 "Mein Leben heute",  "Right.pole")
# dfOut2(df)


###############################################################################
# version: first and last row right/left aligned heading not split into pieces 
#		        
dfOut3 <- function(df, sep="_", init.space=1, just=c("r", "l")){
  if (!is.data.frame(df))
    stop('df must be a data frame')
  if (nchar(sep) != 1L)
    stop('seperator must be ONE character, e.g. " "')
  if (length(just) != 2L)
    stop('just must be a vector of length two e.g. c("r", "l")')
  
  names.to.split <- names(df)[2:(ncol(df) - 1L)]
  name.left <- names(df)[1]
  name.right <- names(df)[ncol(df)]
  names.to.split <- format(names.to.split, justify = "r")
  
  # add seperator at end and space at the beginning
  colnames <- paste(rep(" ", init.space), names.to.split, sep, sep="")      
  names.length <- max(nchar(colnames))  # length of names
  colnames.as.letters.list <- strsplit(colnames, split = "")
  name.left <- list(c(rep(" ", names.length - nchar(sep) - 1L), name.left, sep))
  name.right <- list(c(rep(" ", names.length - nchar(sep) - 1L), name.right, sep))
  colnames.as.letters.list <- c(name.left, colnames.as.letters.list, name.right)
  
  df.colnames <- data.frame(colnames.as.letters.list, stringsAsFactors = FALSE)
  names(df.colnames) <- LETTERS[seq_along(df.colnames)]
  names(df) <- names(df.colnames)
  df <- rbind(df.colnames, df)
  df[ ,1] <- format(df[ ,1], justify = just[1])   # justify first and last column
  df[ ,ncol(df)]  <- format(df[, ncol(df)], justify = just[2])
  df
}
# randomStrings <- function(n){
#   sapply(1:n, function(x) paste(sample(letters, sample(5:20, 1), rep=TRUE), 
#                           collapse=""))
# }
# df <- data.frame(A=randomStrings(10), B=1:10, C=1:10, D=1:10,
#                  E=randomStrings(10))       
# names(df) <- c("Left pole", 
#                "Deutschland in den 60ern", 
#                "Mein Leben aus Sicht meiner Eltern", 
#                "Mein Leben heute",  
#                "Right pole")
# dfOut3(df)
# dfOut3(df, sep="-")
# dfOut3(df,s=" ", just=c("l", "r"))


###############################################################################
# A more general form that can print to console or to a txt file
#
dfOut4 <- function(df, file=NULL, supress=TRUE, sep="   "){
  # replace with and last name first and last line do have different number of blankets
  if (supress)
    names(df)[c(1, ncol(df))] <- c(" ", "  ")
  colnames <- format(names(df), justify="r")				# add leading blanks to each name
  colnames <- paste(" ", colnames, " ", sep="")			# add blanks at bottom and top	
  colnamesAsLettersList <- strsplit(colnames, split="")	# seperate each string into single characters
  dfColnames <- data.frame(colnamesAsLettersList, 
                           stringsAsFactors=F)      # make df out of strings
  names(dfColnames) <- seq_along(dfColnames)				# rename df columns
  mat <- as.matrix(dfColnames)							        # convert to matrix 
  names(df) <- names(dfColnames)						        # name df coluns like the matrix's
  mat_2 <- as.matrix(df)									          # convert to matrix as well	
  mat_all <- rbind(mat, mat_2)							        # bind both matrices together

  for(i in 1:ncol(mat_all)){								        # format each column of matrix
  	if(i==1) mat_all[,1] <- format(mat_all[,i], just="r")
  	if(i==ncol(mat_all)) mat_all[,i] <- format(mat_all[,i], trim=FALSE, width=10, just="l") 
  	else mat_all[,i] <- format(mat_all[,i], just="r")
  }
  # console output
  if (is.null(file)){
    dummy <- apply(mat_all, 1, function(x) cat(c(x, "\n"), collapse = "", sep = sep))
  } else {
    write.table(mat_all, file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep=sep)     
  }
}
# 
# randomStrings <- function(n){
#   sapply(1:n, function(x) paste(sample(letters, sample(5:20, 1), rep=TRUE), 
#                           collapse=""))
# }
# df <- data.frame(Left=randomStrings(10), B=1:10, C=1:10, D=1:10,
#                  Right=randomStrings(10))
# names(df) <- c( "X",                  
#                 "Deutschland in den 60ern", 
#                 "Mein Leben aus Sicht meiner Eltern", 
#                 "Mein Leben heute",  
#                 "Y ")
# dfOut4(df)
# dfOut4(df, sep=" ")
# dfOut4(df, file="test.txt")



###############################################################################

### VERSION 2 ###
# This version prints the column names indented vertically.
#
#				        first column
#				        |  second column
#				        |  |  third column
#				        |  |  |  fourth column
# 			        |  |  |  |
# 		        	+--+--+--+
# 	rowname 1  	1  2  3  4
# 	rowname 2	  2  1  1  0
# 	rowname 3	  1  2  1  1
# 	rowname 4	  1  1  0  1
#
# Implementation: the data is converted into a matrix structure and each
# matrix row is printed by cat() individually.




# trim column of a matrix to equal width
# x: matrix
trim_column_width <- function(x, just="l"){
  lengths <- apply(nchar(x), 2, max)
  for (j in seq_len(ncol(x))) {
    x[ ,j] <- format(x[ ,j], just = just, width = lengths[j])
  }
  x
}
#trim_column_width(x, just="r")


collapse_matrix <- function(x, collapse="", sep=" "){
  x <- apply(x, 1, paste, 
             collapse = collapse, sep = sep)  # collapse whole matrix
  do.call(rbind, as.list(x))                  # bind whole matrix together
}


# x:  matrix
matrix_to_single_char_matrix <- function(x, collapse="", sep=" "){
  x <- collapse_matrix(x, collapse = collapse, sep = sep)
  x <- sapply(x, strsplit, split = "")            # split to single chars
  names(x) <- NULL                                # for cleaner output
  do.call(rbind, x)                               # single chars matrix
}


# TODO: first line indent wrong
matrix_to_console <- function(x, sep=""){
  #cat(" ")  ???
  dummy <- apply(x, 1, function(x) 
                  cat(c(x, "\n"), collapse = "", sep = sep))
}

widths_matrix_columns <- function(x){
  apply(x, 2, function(y) max(nchar(y)))
}


# just.rows <- "r"      # justification of row names
# just.main <- "c"      # justification of body
# just.top <- "l"
# max.char.rows <- 200  # maximum number of characters of row names to be printed
# sep <- " "            # seperator symbol between columns
# sep2 <- "   "         # seperator between row names and first column
# equal <- FALSE        # equal width for columns (max column width)
# prefix <- ""          # optional prefix before printed column name 
#                       # (e.g. "+---"). characters
# colstart <- "cl"
#
# TODO: (mh) 
# colstart argument "cl" and "cr" do not work properly
# when linebreaking, the long arrows for elements are unnecessary and can be shortened?
# make roof like output like Bremer Bertin matrix
# cut lines ends if no element columns remain to be printed on subsequent page
df_out <- function(df, 
                   just.rows="r",     # justification of row names
                   just.main="l",     # justification of body
                   max.char.rows=200, # max no of chars of row names to be printed
                   sep=" ",           # seperator symbol between columns
                   sep2="  ",         # seperator between row names and first column
                   equal=FALSE,       # equal width for columns (max column width)
                   prefix="",         # optional prefix before printed column name 
                                      # (e.g. "+---"). characters
                   keeprows=T,        # whether to show rows after each pagebreak
                   colstart="l",
                   margin=1,          # right mragin for linebreak
                   trim=c(NA,NA),     # maximum number of character for r/c entry.
                   id=c(F,F) )        # id numbers at beginning/end of each row/column
{       
  # row names matrix mat.r
  names.rows <- rownames(df)                      # extract rownames
  if (!is.na(trim[1]))                            # trim rownames
    names.rows <- substr(names.rows, 1, trim[1])
  if (id[1]) {                                    # add id number to each row
    ids <- paste("(", seq_along(names.rows), ")", sep="")
    names.rows <- paste(names.rows, ids)
  }
  names.rows <- substr(names.rows, 1 , max.char.rows)   # shorten rownames
  names.rows <- format(names.rows, justify=just.rows)   # justify rownames
  mat.r <- as.matrix(names.rows)

  # main matrix mat.m 
  mat.m <- sapply(df, as.character)   # convert to character for type security
  rownames(mat.m) <- NULL             # unnecessary 
  colnames(mat.m) <- NULL             # unnecessary 
  mat.m <- as.matrix(mat.m)           # convert to matrix
  nchar.column <- widths_matrix_columns(mat.m)   # no of chars per column
  if (equal) {                        # equal or dynamic column width
    mat.m <- format(mat.m, justify = just.main, width = max(nchar.column)) 
  } else {  
    mat.m <- trim_column_width(mat.m, just = just.main)
  }

  # upper matrix mat.u
  widths.rownames <- widths_matrix_columns(mat.r)  # width rownames column
  widths.columns <- widths_matrix_columns(mat.m)   # vector column widths
  widths.sep1 <- nchar(sep)
  widths.sep2 <- nchar(sep2)

  # where to place colnames in matrix upper
  columns.start.r <- cumsum(widths.columns + widths.sep1) - widths.sep1
  columns.start.l <- columns.start.r - widths.columns + 1
  columns.start.cl <- columns.start.l + floor((widths.columns + 1) / 2)
  columns.start.cr <- columns.start.l + ceiling((widths.columns + 1) / 2)

  # select column start vector
  if (colstart == "r")
    columns.start <- columns.start.r else
  if (colstart == "cl")
    columns.start <- columns.start.cl else
  if (colstart == "cr")
    columns.start <- columns.start.cr else 
    columns.start <- columns.start.l

  # maximal rows of mat.u is length of column name plus starting position
  # plus prefix
  names.columns <- colnames(df)                         # extract colnames
  if (!is.na(trim[2]))                                  # trim colnames
    names.columns <- substr(names.columns, 1, trim[2])
  if (id[2]) {                                    # add id number to each col
     ids <- paste(seq_along(names.columns), "-", sep=" ")
     names.columns <- paste(ids, names.columns)
  }
  names.columns <- paste(prefix, names.columns, sep="") # add prefix (default "")
  ncol.mat.columns <- max(columns.start + 
                          nchar(names.columns) - 1)     # min no of columns
  nrow.mat.columns <- length(names.columns) + 1
  mat.u.atomic <- matrix(" ", nrow=nrow.mat.columns,    # empty matrix
                         ncol=ncol.mat.columns)              

  # fill matrix
  names.atomic.list <- strsplit(names.columns, "")
  lengths.colnames <- nchar(names.columns)
  for(j in seq_along(columns.start)){  # vertical lines ("|") at column starts
    mat.u.atomic[(j + 1):nrow(mat.u.atomic), columns.start[j]] <- "|"
    mat.u.atomic[j, columns.start[j]:(columns.start[j] + 
                 lengths.colnames[j] -1)] <- names.atomic.list[[j]]
  }
  
  mat.r.atomic <- matrix_to_single_char_matrix(mat.r)
  mat.m.atomic <- matrix_to_single_char_matrix(mat.m, collapse=sep)

  # adjust matrix main or upper size by attaching empty columns if necessary
  ncol.m <- ncol(mat.m.atomic)
  ncol.u <- ncol(mat.u.atomic)
  if (ncol.m > ncol.u)                             # if mat.main is bigger 
    mat.u.atomic <- cbind(mat.u.atomic, matrix(" ", ncol = ncol.m - ncol.u, 
                                               nrow = nrow(mat.u.atomic)))
  if (ncol.m < ncol.u)                             # if mat.upper is bigger 
    mat.m.atomic <- cbind(mat.m.atomic, matrix(" ", ncol = ncol.u - ncol.m, 
                                               nrow = nrow(mat.m.atomic)))

  # void matrix to fill upper left corner when combining matrices
  mat.v.atomic <- matrix(" ", nrow = nrow(mat.u.atomic),
                         ncol=ncol(mat.r.atomic))
                                                                    
  mat.um <- rbind(mat.u.atomic, mat.m.atomic)
  mat.vr <- rbind(mat.v.atomic, mat.r.atomic) 
  sep2.normal <- strsplit(sep2, "")[[1]]
  sep2.void <- rep(" ", widths.sep2)
  mat.sep2.void <- matrix(sep2.void, nrow = nrow(mat.u.atomic), 
                          ncol=widths.sep2, byrow=TRUE)
  mat.sep2.normal <- matrix(sep2.normal, nrow = nrow(mat.r.atomic), 
                            ncol=widths.sep2, byrow=TRUE)
  mat.sep2 <- rbind(mat.sep2.void, mat.sep2.normal) 
  mat.out <- cbind(mat.vr, mat.sep2, mat.um)
  
  # function without breaking would use next two lines and end there
  #out <- collapse_matrix(mat.out)   # collapse rows
  #matrix_to_console(out)            # print to console

  # split output into parts if it does not fit console width
  availchar <- options()$width   # get console size (problematic update)
  
  # does collapsed vec contain only spaces? - to spot possible break columns
  is_empty_column <- function(str){  
    str <- paste(str, collapse="")
    str <- sub(' +$', '', str)
    str == ""
  }
  
  columnrows <- (nrow(mat.u.atomic) + 1):nrow(mat.um) # rows with column names
  break_output <- function(mat){
    possible.breaks.at <- which(apply(mat[columnrows, ],    # breaks possible 
                                      2, is_empty_column))  # at empty columns
    i <- which(possible.breaks.at > availchar - margin)[1]  # first break pos 
    if (identical(i, integer(0)))
      i <- NA
    breakat <- possible.breaks.at[i-1][1]   # NA if no break needed
    if (! is.na(breakat)) {
      mat.tmp <- mat[ , 1:(breakat-1)]
      out.tmp <- collapse_matrix(mat.tmp, collapse = "")  # collapse rows
      matrix_to_console(out.tmp)                          # print to console
      if (keeprows) {                 # rownames after each pagebreak?
        mat.residual <- mat[ , c(1:(widths.rownames + widths.sep2), 
                                    breakat:ncol(mat))] 
      } else {
        mat.residual <- mat[ , c(breakat:ncol(mat))]
      }
      break_output(mat.residual)                 # recursive call             
    } else {
      out <- collapse_matrix(mat, collapse="")   # collapse rows
      matrix_to_console(out)                     # print to console
    }
  }
  break_output(mat.out)
}


random_df <- function(nrow=ncol, ncol=nrow, wrow=6, wcol=10){
  x <- data.frame(replicate(ncol, sample(1:5, nrow, rep=TRUE)))
  rownames(x) <- replicate(nrow, randomSentence(wrow))
  colnames(x) <- replicate(ncol, randomSentence(wcol))
  x
}


# @param  x     a single char matrix
# @param  left  number of empoty columns added on left side (default 0)
# @param  right number of empty columns added at right side (default 0)
# @return matrix
# @keywords internal
add_empty_cols <- function(x, left=0, right=0){
  x <- cbind(matrix(" ", nrow=nrow(x), ncol=left), x)
  x <- cbind(x, matrix(" ", nrow=nrow(x), ncol=right))
  x
}


# Binds two single character matrices of different size horizontally
#
# Two matrices in atomci format are binded horizontally at a specified
# position. The matrices need to be in single char format, i.e. one character per cell
# only. If the dimensions are different, the margins of the matrices are filled up with
# empty cells.
#
# @param  um        upper matrix (must be single char matrix)
# @param  lm        lower matrix (must be single char matrix)
# @param  anchors   two integers specifying at which columns matrices are aligned
# @return matrix
# @keywords internal
# @examples \dontrun{
#   um <- matrix("u", ncol=10, nrow=5)
#   lm <- matrix("l", ncol=8, nrow=3)
#   bind_matrices_horizontally(um, lm, anchors=c(3,1))
# }
bind_matrices_horizontally <- function(um, lm, anchors=c(1,1)){
  diff.left <- diff(anchors)                  # add columns on left side
  if (diff.left <= 0) {             
    um.ncols.empty.left <- 0
    lm.ncols.empty.left <- abs(diff.left)
  } else {
    um.ncols.empty.left <- abs(diff.left)
    lm.ncols.empty.left <- 0
  }
  um <- add_empty_cols(um, left=um.ncols.empty.left)
  lm <- add_empty_cols(lm, left=lm.ncols.empty.left)
  
  diff.right <- diff(c(ncol(um), ncol(lm)))   # add columns on right side
  if (diff.right <= 0) {
    um.ncols.empty.right <- 0
    lm.ncols.empty.right <- abs(diff.right)
  } else {
    um.ncols.empty.right <- abs(diff.right)
    lm.ncols.empty.right <- 0
  }
  um <- add_empty_cols(um, right=um.ncols.empty.right)
  lm <- add_empty_cols(lm, right=lm.ncols.empty.right)
  
  rbind(um, lm)
}



###############################################################################
# 2nd version with Bremer Bertin style.
# Needs modification of mat.u, and extension to the left if necessary
# no changes in mat.main, mat.r
# make roof like output like Bremer Bertin matrix

# TODO: (mh) 
# - colstart argument "cl" and "cr" do not work properly
# - bug, when widths is too small. set keeprows=FALSE when too little space. Otherwise
#   endless recursion
# - trim
df_out <- function(df, 
                   just.rows="r",     # justification of row names
                   just.main="l",     # justification of body
                   max.char.rows=200, # max no of chars of row names to be printed
                   sep=" ",           # seperator symbol between columns
                   sep2="   ",        # seperator between row names and first column
                   equal=FALSE,       # equal width for columns (max column width)
                   prefix="",         # optional prefix before printed column name 
                                      # (e.g. "+---"). characters
                   keeprows=T,        # whether to show rows after each pagebreak
                   colstart="l",
                   margin=1,          # right mragin for linebreak
                   trim=c(NA,NA),     # maximum number of character for r/c entry.
                   cut=c(NA, NA),     # maximal number of chars left and right of main matrix
                   id=c(F,F),         # id numbers at beginning/end of each row/column
                   hatform=FALSE)     # column names in hat form 
{       
  if (length(trim) == 1)    # if only one parameter given, extend to the other
    trim <- recycle(trim, 2)
  if (length(cut) == 1)
    cut <- recycle(cut, 2)
  if (length(id) == 1)
    id <- recycle(id, 2)    
    
  # split output into parts if it does not fit console width  
  # does collapsed vec contain only spaces? - to spot possible break columns
  is_empty_column <- function(str){  
    str <- paste(str, collapse="")
    str <- sub(' +$', '', str)
    str == ""
  }
  
  # mat         single char matrix to break up
  # columnrows  rows in which empry cols are looked for. 
  #             Defaut is whole matrix mat
  # details: if available width is too small to properly break the matrix
  #          keeprows is set to FALSE to avoid endless recursion.   
  break_output <- function(mat, columnrows=1:nrow(mat), minwidth=options()$width){
    margin <- 0
    availchar <- options()$width                            # get console size (problematic update)
    if (availchar < minwidth + 10)                          # set FALSE to avoid endless recursion
      keeprows <- FALSE
    possible.breaks.at <- which(apply(mat[columnrows, ],    # breaks possible 
                                      2, is_empty_column))  # at empty columns
    i <- which(possible.breaks.at > availchar - margin)[1]  # first break pos 
    if (identical(i, integer(0)))
      i <- NA
    breakat <- possible.breaks.at[i-1][1]   # NA if no break needed
    if (! is.na(breakat)) {
      mat.tmp <- mat[ , 1:(breakat-1)]
      out.tmp <- collapse_matrix(mat.tmp, collapse = "")  # collapse rows
      matrix_to_console(out.tmp)                          # print to console
      cat("\n")   # empty line after print out tp seperate
      if (keeprows) {                 # rownames after each pagebreak?
        mat.residual <- mat[ , c(1:(widths.rownames + widths.sep2), 
                                    breakat:ncol(mat))] 
      } else {
        mat.residual <- mat[ , c(breakat:ncol(mat))]
      }
      Recall(mat.residual, columnrows)     # recursive call            
    } else {
      out <- collapse_matrix(mat, collapse="")   # collapse rows
      matrix_to_console(out)                     # print to console
    }
  }
  
  # break at any point possible
  break_output_2 <- function(mat, ncolkeep=14, keeprows=TRUE){
      availchar <- options()$width          # get console size (problematic update)
      #print(availchar)
      #if (availchar < ncolkeep)             # set FALSE to avoid endless recursion
      #  keeprows <- FALSE
      if (ncol(mat) >= availchar) {
        mat.tmp <- mat[ , 1:(availchar - 1)]
        out.tmp <- collapse_matrix(mat.tmp, collapse = "")  # collapse rows
        matrix_to_console(out.tmp)                          # print first part to console
        cat("\n")           # empty line after print out to seperate prints
       # if (keeprows) {     # rownames after each pagebreak?
      #    mat.residual <- mat[ , c(1:(ncolkeep), availchar:ncol(mat))] 
       # } else {
          mat.residual <- mat[ , c(availchar:ncol(mat))]
        #}
        Recall(mat.residual)     # recursive output call            
      } else {
        out <- collapse_matrix(mat, collapse="")   # collapse rows
        matrix_to_console(out)                     # print to console
      }
    }
  
  make_sep_mat_atomic <- function(sep, nr){
    sep.atomic <- strsplit(sep, "")[[1]]
    matrix(sep.atomic, nrow = nr, 
                       ncol=nchar(sep), byrow=TRUE)
  }
  
  # row names matrix mat.r
  make_mat_rows <- function(df){
    names.rows <- rownames(df)                      # extract rownames
    if (!is.na(trim[1]))                            # trim rownames
      names.rows <- substr(names.rows, 1, trim[1])
    if (id[1]) {                                    # add id number to each row
      ids <- paste("(", seq_along(names.rows), ")", sep="")
      names.rows <- paste(names.rows, ids)
    }
    names.rows <- substr(names.rows, 1 , max.char.rows)   # shorten rownames
    names.rows <- format(names.rows, justify=just.rows)   # justify rownames
    mat.r <- as.matrix(names.rows)
    mat.r
  }
  
  # main matrix mat.m 
  make_mat_main <- function(df) {
    mat.m <- sapply(df, as.character)   # convert to character for type security
    rownames(mat.m) <- NULL             # unnecessary 
    colnames(mat.m) <- NULL             # unnecessary 
    mat.m <- as.matrix(mat.m)           # convert to matrix
    nchar.column <- widths_matrix_columns(mat.m)   # no of chars per column
    if (equal) {                        # equal or dynamic column width
      mat.m <- format(mat.m, justify = just.main, width = max(nchar.column)) 
    } else {  
      mat.m <- trim_column_width(mat.m, just = just.main)
    }
    mat.m
  }
  
  mat.r <- make_mat_rows(df)
  mat.m <- make_mat_main(df)
  mat.r.atomic <- matrix_to_single_char_matrix(mat.r)
  mat.m.atomic <- matrix_to_single_char_matrix(mat.m, collapse=sep)
  
  # upper matrix mat.u
  #widths.rownames <- widths_matrix_columns(mat.r)  # width rownames column
  widths.columns <- widths_matrix_columns(mat.m)   # vector column widths
  widths.sep1 <- nchar(sep)
  widths.sep2 <- nchar(sep2)
  
  # where to place colnames in matrix upper
  columns.start.r <- cumsum(widths.columns + widths.sep1) - widths.sep1
  columns.start.l <- columns.start.r - widths.columns + 1
  columns.start.cl <- columns.start.l + floor((widths.columns + 1) / 2)
  columns.start.cr <- columns.start.l + ceiling((widths.columns + 1) / 2)

  # select column start vector
  if (colstart == "r")
    columns.start <- columns.start.r else
  if (colstart == "cl")
    columns.start <- columns.start.cl else
  if (colstart == "cr")
    columns.start <- columns.start.cr else 
    columns.start <- columns.start.l

  # maximal rows of mat.u is length of column name plus starting position (plus prefix)
  names.columns <- colnames(df)                         # extract colnames
  if (!is.na(trim[2]))                                  # trim colnames
    names.columns <- substr(names.columns, 1, trim[2])

  ### hat = FALSE   (upper matrix u in descending form)
  if (!hatform){
    if (id[2]) {                                    # add id number to each col
       ids <- paste(seq_along(names.columns), "-", sep=" ")
       names.columns <- paste(ids, names.columns)
    }

    names.columns <- paste(prefix, names.columns, sep="") # add prefix (default "")
    ncol.mat.columns <- max(columns.start + 
                            nchar(names.columns) - 1)     # min no columns mat.u
    nrow.mat.columns <- length(names.columns) + 1
    mat.u.atomic <- matrix(" ", nrow=nrow.mat.columns,    # empty matrix
                           ncol=ncol.mat.columns)              

    # fill matrix upper
    names.atomic.list <- strsplit(names.columns, "")
    lengths.colnames <- nchar(names.columns)
    for (j in seq_along(columns.start)){  # vertical lines ("|") at column starts
      mat.u.atomic[(j + 1):nrow(mat.u.atomic), columns.start[j]] <- "|"
      mat.u.atomic[j, columns.start[j]:(columns.start[j] + 
                   lengths.colnames[j] -1)] <- names.atomic.list[[j]]
    }
    extra.cols.left <- 0                              # to suit results of hat=TRUE part
  }
  
  
  ### hat = TRUE  (upper matrix u in hat form)
  if (hatform){
    ncol <- length(names.columns)                     # no of columns
    midcol <- ceiling((ncol + 1) / 2)                 # determine middle column
    index.cols.left <- 1:(midcol - 1)                 # index of left columns
    index.cols.right <- midcol:ncol                   # index of right columns
    colnames.left <- names.columns[index.cols.left]   # left hat side
    colnames.right <- names.columns[index.cols.right] # right hat side 

    if (id[2]) {                                      # add id number to each col
      ids.left <- seq_along(names.columns)[index.cols.left]
      ids.right <- seq_along(names.columns)[index.cols.right]
      colnames.left <- paste(colnames.left, ids.left, sep=" - ")
      colnames.right <- paste(ids.right, colnames.right, sep=" - ")
    }  
    
    # add prefix to both sides (default "")
    colnames.left <- paste(colnames.left, strReverse(prefix), sep="")  # left side has revesred prefix 
    colnames.right <- paste(prefix, colnames.right, sep="")
    colnames.leftright <- c(colnames.left, colnames.right)
    lengths.colnames <- nchar(colnames.leftright)
  
    minpos <- min(columns.start[index.cols.left] - nchar(colnames.left))    # min pos to left
    maxpos <- max(columns.start[index.cols.right] + nchar(colnames.right))  # max pos to right
    if (minpos < 0 ) {
      extra.cols.left <- abs(minpos) 
    } else {
      extra.cols.left <- 0
    }
    ncol.mat.upper <- extra.cols.left + maxpos                                    # ncol of upper matrix
    nrow.mat.upper <- max(c(length(colnames.left), length(colnames.right))) + 1   # nrow of upper matrix
    mat.u.atomic <- matrix(" ", nrow=nrow.mat.upper,    # empty upper matrix to get filled
                                ncol=ncol.mat.upper)
                                  
    names.atomic.list.left <- strsplit(colnames.left, "")
    names.atomic.list.right <- strsplit(colnames.right, "")
    names.atomic.list.leftright <- c(names.atomic.list.left,
                                     names.atomic.list.right)

    # fill matrix u and build vertical lines for left and right side
    bottom.row <- nrow(mat.u.atomic)
    nc <- length(columns.start)
    columns.start.offsetted <- extra.cols.left + columns.start
    for (j in seq_along(columns.start)) {  # vertical lines ("|") at column starts
      if (j < ceiling((nc + 1)/ 2)) {
        mat.u.atomic[(bottom.row - j + 1):bottom.row, 
                      columns.start.offsetted[j]] <- "|"
        mat.u.atomic[(bottom.row - j), 
                     (columns.start.offsetted[j] - lengths.colnames[j] + 1):
                      columns.start.offsetted[j]] <- 
                      names.atomic.list.leftright[[j]]
      } else { 
        mat.u.atomic[(bottom.row - (nc - j) - 1):bottom.row, 
                      columns.start.offsetted[j]] <- "|"   
        mat.u.atomic[(bottom.row - (nc - j) - 1), columns.start.offsetted[j]:
                     (columns.start.offsetted[j] + lengths.colnames[j] - 1)] <- 
                      names.atomic.list.leftright[[j]]
      }
    }   # TODO: right side one row too much, maybe erase
  }
  
  # same part for both types
  mat.sep2.atomic <- make_sep_mat_atomic(sep2, nr=nrow(mat.r.atomic))
  mat.lm.atomic <- cbind(mat.r.atomic, mat.sep2.atomic, mat.m.atomic)   # lower matrix lm

  # join upper and main matrix
  anchor.um <- extra.cols.left + 1
  anchor.lm <- ncol(mat.r.atomic) + ncol(mat.sep2.atomic) + 1
  mat.out.atomic <- bind_matrices_horizontally(mat.u.atomic, mat.lm.atomic,
                                               anchors=c(anchor.um, anchor.lm))
                                                                                             
  # cut output at sides if prompted
  diff.left <- diff(c(anchor.um, anchor.lm))
  if (diff.left <= 0) {             
    lm.empty.cols.left <- abs(diff.left)
  } else {
    lm.empty.cols.left <- 0
  }  
  start.main.at <- lm.empty.cols.left + ncol(cbind(mat.r.atomic, mat.sep2.atomic))
  end.main.at <- start.main.at + ncol(mat.m.atomic)
  
  if (!is.na(cut[1]) | !is.na(cut[2])){   
    if (is.na(cut[1])){
      end.left <- 1        
    } else {
      end.left <- trim_val(start.main.at - cut[1], minmax=c(1, 200))        
    }
    if (is.na(cut[2])){
      end.right <- ncol(mat.out.atomic)        
    } else {
      end.right <- trim_val(end.main.at  + cut[2],
                            minmax=c(1, ncol(mat.out.atomic)))        
    }
    mat.out.atomic <- mat.out.atomic[ , end.left:end.right]           
  }
  
  #columnrows <- (nrow(mat.u.atomic) + 1):nrow(mat.out.atomic) # rows with column names                        
  #break_output(mat.out.atomic, columnrows, minwidth=anchor.lm)
  break_output_2(mat.out.atomic)
  invisible(NULL)
}

#df <- random_df(10, 20)
#df_out(df)

###############################################################################

###############################################################################
# 3rd version with Bremer Bertin style.
# Needs modification of mat.u, and extension to the left if necessary
# no changes in mat.main, mat.r
# make roof like output like Bremer Bertin matrix

# TODO: (mh) 
# - colstart argument "cl" and "cr" do not work properly
# - bug, when widths is too small. set keeprows=FALSE when too little space. Otherwise
#   endless recursion
# - trim
df_out <- function(df, 
                   just.rows="r",     # justification of row names
                   just.main="l",     # justification of body
                   max.char.rows=200, # max no of chars of row names to be printed
                   sep=" ",           # seperator symbol between columns
                   sep2="   ",        # seperator between row names and first column
                   equal=FALSE,       # equal width for columns (max column width)
                   prefix="",         # optional prefix before printed column name 
                                      # (e.g. "+---"). characters
                   keeprows=T,        # whether to show rows after each pagebreak
                   colstart="l",
                   margin=1,          # right mragin for linebreak
                   trim=c(NA,NA),     # maximum number of character for r/c entry.
                   cut=c(NA, NA),     # maximal number of chars left and right of main matrix
                   id=c(F,F),         # id numbers at beginning/end of each row/column
                   hatform=FALSE)     # column names in hat form 
{       
  if (length(trim) == 1)    # if only one parameter given, extend to the other
    trim <- recycle(trim, 2)
  if (length(cut) == 1)
    cut <- recycle(cut, 2)
  if (length(id) == 1)
    id <- recycle(id, 2)    
    
  # break at any point possible
  break_output_2 <- function(mat, ncolkeep=14, keeprows=TRUE){
      availchar <- options()$width          # get console size (problematic update)
      #print(availchar)
      #if (availchar < ncolkeep)             # set FALSE to avoid endless recursion
      #  keeprows <- FALSE
      if (ncol(mat) >= availchar) {
        mat.tmp <- mat[ , 1:(availchar - 1)]
        out.tmp <- collapse_matrix(mat.tmp, collapse = "")  # collapse rows
        matrix_to_console(out.tmp)                          # print first part to console
        cat("\n")           # empty line after print out to seperate prints
       # if (keeprows) {     # rownames after each pagebreak?
      #    mat.residual <- mat[ , c(1:(ncolkeep), availchar:ncol(mat))] 
       # } else {
          mat.residual <- mat[ , c(availchar:ncol(mat))]
        #}
        Recall(mat.residual)     # recursive output call            
      } else {
        out <- collapse_matrix(mat, collapse="")   # collapse rows
        matrix_to_console(out)                     # print to console
      }
    }
  
  make_sep_mat_atomic <- function(sep, nr){
    sep.atomic <- strsplit(sep, "")[[1]]
    matrix(sep.atomic, nrow = nr, 
                       ncol=nchar(sep), byrow=TRUE)
  }
  
  # row names matrix mat.r
  make_mat_rows <- function(df){
    names.rows <- rownames(df)                      # extract rownames
    if (!is.na(trim[1]))                            # trim rownames
      names.rows <- substr(names.rows, 1, trim[1])
    if (id[1]) {                                    # add id number to each row
      ids <- paste("(", seq_along(names.rows), ")", sep="")
      names.rows <- paste(names.rows, ids)
    }
    names.rows <- substr(names.rows, 1 , max.char.rows)   # shorten rownames
    names.rows <- format(names.rows, justify=just.rows)   # justify rownames
    mat.r <- as.matrix(names.rows)
    mat.r
  }
  
  # main matrix mat.m 
  make_mat_main <- function(df) {
    mat.m <- sapply(df, as.character)   # convert to character for type security
    rownames(mat.m) <- NULL             # unnecessary 
    colnames(mat.m) <- NULL             # unnecessary 
    mat.m <- as.matrix(mat.m)           # convert to matrix
    nchar.column <- widths_matrix_columns(mat.m)   # no of chars per column
    if (equal) {                        # equal or dynamic column width
      mat.m <- format(mat.m, justify = just.main, width = max(nchar.column)) 
    } else {  
      mat.m <- trim_column_width(mat.m, just = just.main)
    }
    mat.m
  }
  
  mat.r <- make_mat_rows(df)
  mat.m <- make_mat_main(df)
  mat.r.atomic <- matrix_to_single_char_matrix(mat.r)
  mat.m.atomic <- matrix_to_single_char_matrix(mat.m, collapse=sep)
  
  widths.columns <- widths_matrix_columns(mat.m)   # vector column widths
  widths.sep1 <- nchar(sep)
  widths.sep2 <- nchar(sep2)
  
  # where to place colnames in matrix upper
  columns.start.r <- cumsum(widths.columns + widths.sep1) - widths.sep1
  columns.start.l <- columns.start.r - widths.columns + 1
  columns.start.cl <- columns.start.l + floor((widths.columns + 1) / 2)
  columns.start.cr <- columns.start.l + ceiling((widths.columns + 1) / 2)

  # select column start vector
  if (colstart == "r")
    columns.start <- columns.start.r else
  if (colstart == "cl")
    columns.start <- columns.start.cl else
  if (colstart == "cr")
    columns.start <- columns.start.cr else 
    columns.start <- columns.start.l

  # maximal rows of mat.u is length of column name plus starting position (plus prefix)
  names.columns <- colnames(df)                         # extract colnames
  if (!is.na(trim[2]))                                  # trim colnames
    names.columns <- substr(names.columns, 1, trim[2])

  ### hat = FALSE   (upper matrix u in descending form)
  if (!hatform){
    if (id[2]) {                                    # add id number to each col
       ids <- paste(seq_along(names.columns), "-", sep=" ")
       names.columns <- paste(ids, names.columns)
    }

    names.columns <- paste(prefix, names.columns, sep="") # add prefix (default "")
    ncol.mat.columns <- max(columns.start + 
                            nchar(names.columns) - 1)     # min no columns mat.u
    nrow.mat.columns <- length(names.columns) + 1
    mat.u.atomic <- matrix(" ", nrow=nrow.mat.columns,    # empty matrix
                           ncol=ncol.mat.columns)              

    # fill matrix upper
    names.atomic.list <- strsplit(names.columns, "")
    lengths.colnames <- nchar(names.columns)
    for (j in seq_along(columns.start)){  # vertical lines ("|") at column starts
      mat.u.atomic[(j + 1):nrow(mat.u.atomic), columns.start[j]] <- "|"
      mat.u.atomic[j, columns.start[j]:(columns.start[j] + 
                   lengths.colnames[j] -1)] <- names.atomic.list[[j]]
    }
    extra.cols.left <- 0                              # to suit results of hat=TRUE part
  }
  
  
  ### hat = TRUE  (upper matrix u in hat form)
  if (hatform){
    ncol <- length(names.columns)                     # no of columns
    midcol <- ceiling((ncol + 1) / 2)                 # determine middle column
    index.cols.left <- 1:(midcol - 1)                 # index of left columns
    index.cols.right <- midcol:ncol                   # index of right columns
    colnames.left <- names.columns[index.cols.left]   # left hat side
    colnames.right <- names.columns[index.cols.right] # right hat side 

    if (id[2]) {                                      # add id number to each col
      ids.left <- seq_along(names.columns)[index.cols.left]
      ids.right <- seq_along(names.columns)[index.cols.right]
      colnames.left <- paste(colnames.left, ids.left, sep=" - ")
      colnames.right <- paste(ids.right, colnames.right, sep=" - ")
    }  
    
    # add prefix to both sides (default "")
    colnames.left <- paste(colnames.left, strReverse(prefix), sep="")  # left side has revesred prefix 
    colnames.right <- paste(prefix, colnames.right, sep="")
    colnames.leftright <- c(colnames.left, colnames.right)
    lengths.colnames <- nchar(colnames.leftright)
  
    minpos <- min(columns.start[index.cols.left] - nchar(colnames.left))    # min pos to left
    maxpos <- max(columns.start[index.cols.right] + nchar(colnames.right))  # max pos to right
    if (minpos < 0 ) {
      extra.cols.left <- abs(minpos) 
    } else {
      extra.cols.left <- 0
    }
    ncol.mat.upper <- extra.cols.left + maxpos                                    # ncol of upper matrix
    nrow.mat.upper <- max(c(length(colnames.left), length(colnames.right))) + 1   # nrow of upper matrix
    mat.u.atomic <- matrix(" ", nrow=nrow.mat.upper,    # empty upper matrix to get filled
                                ncol=ncol.mat.upper)
                                  
    names.atomic.list.left <- strsplit(colnames.left, "")
    names.atomic.list.right <- strsplit(colnames.right, "")
    names.atomic.list.leftright <- c(names.atomic.list.left,
                                     names.atomic.list.right)

    # fill matrix u and build vertical lines for left and right side
    bottom.row <- nrow(mat.u.atomic)
    nc <- length(columns.start)
    columns.start.offsetted <- extra.cols.left + columns.start
    for (j in seq_along(columns.start)) {  # vertical lines ("|") at column starts
      if (j < ceiling((nc + 1)/ 2)) {
        mat.u.atomic[(bottom.row - j + 1):bottom.row, 
                      columns.start.offsetted[j]] <- "|"
        mat.u.atomic[(bottom.row - j), 
                     (columns.start.offsetted[j] - lengths.colnames[j] + 1):
                      columns.start.offsetted[j]] <- 
                      names.atomic.list.leftright[[j]]
      } else { 
        mat.u.atomic[(bottom.row - (nc - j) - 1):bottom.row, 
                      columns.start.offsetted[j]] <- "|"   
        mat.u.atomic[(bottom.row - (nc - j) - 1), columns.start.offsetted[j]:
                     (columns.start.offsetted[j] + lengths.colnames[j] - 1)] <- 
                      names.atomic.list.leftright[[j]]
      }
    }   # TODO: right side one row too much, maybe erase
  }
  
  # same part for both types
  mat.sep2.atomic <- make_sep_mat_atomic(sep2, nr=nrow(mat.r.atomic))
  mat.lm.atomic <- cbind(mat.r.atomic, mat.sep2.atomic, mat.m.atomic)   # lower matrix lm

  # join upper and main matrix
  anchor.um <- extra.cols.left + 1
  anchor.lm <- ncol(mat.r.atomic) + ncol(mat.sep2.atomic) + 1
  mat.out.atomic <- bind_matrices_horizontally(mat.u.atomic, mat.lm.atomic,
                                               anchors=c(anchor.um, anchor.lm))
                                                                                             
  # cut output at sides if prompted
  diff.left <- diff(c(anchor.um, anchor.lm))
  if (diff.left <= 0) {             
    lm.empty.cols.left <- abs(diff.left)
  } else {
    lm.empty.cols.left <- 0
  }  
  start.main.at <- lm.empty.cols.left + ncol(cbind(mat.r.atomic, mat.sep2.atomic))
  end.main.at <- start.main.at + ncol(mat.m.atomic)
  
  if (!is.na(cut[1]) | !is.na(cut[2])){   
    if (is.na(cut[1])){
      end.left <- 1        
    } else {
      end.left <- trim_val(start.main.at - cut[1], minmax=c(1, 200))        
    }
    if (is.na(cut[2])){
      end.right <- ncol(mat.out.atomic)        
    } else {
      end.right <- trim_val(end.main.at  + cut[2],
                            minmax=c(1, ncol(mat.out.atomic)))        
    }
    mat.out.atomic <- mat.out.atomic[ , end.left:end.right]           
  }
  
  #columnrows <- (nrow(mat.u.atomic) + 1):nrow(mat.out.atomic) # rows with column names                        
  #break_output(mat.out.atomic, columnrows, minwidth=anchor.lm)
  break_output_2(mat.out.atomic)
  invisible(NULL)
}

df <- random_df(10, 20)
df_out(df)




