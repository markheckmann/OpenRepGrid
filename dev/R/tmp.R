################################################################################





################################################################################
###                     OUTPUT OF BASIC MEASURES IN .TXT FILE
################################################################################
## writes the alls analysis into one text file
##
    
writeToTxtFile <- function(data=NA, filename=NA, dir = getwd(), ...)
{
    # check if a data object is supplied as a data frame
    if(is.na(data)) stop("Please specify a data object to be written to the file")

    # is a filename supplied. if not open file choose menu
    if (is.na(filename)){ 
        outFile <- choose.files(caption="Select one .txt file as output file",
                                multi=FALSE, filters = Filters, index = 8)
        if(!file.exists(outFile)) stop("No .txt file selected")
    } else  {
        outFile <- paste(dir, filename, sep="/")
    }
 
   # write txt file
    con <- file(outFile, "w")                                         # open an output file connection
    cat(headline, file = con, sep = "\n")                             # headline
    write.table(data, file = con, sep = "\t", row.names = FALSE)      # write analysis data
    close(con)
    print("File erstellt")
}


################################################################################
###                     OUTPUT OF BASIC MEASURES INTO .HTML FILE
################################################################################
## writes the analysis into a html file
##

writeToHtmlFile <- function(data=NA, filename=NA, dir = getwd(), ...)
{
    # check if a data object is supplied as a data frame
    if(is.list(data)){
      if(!all(unlist(lapply(l, function(x) is.data.frame(x) | is.matrix(x))))) stop("Please specify each list element as data frame or matrix")
    } else {
      if(!is.data.frame(data) & !is.matrix(data)) stop("Please specify a data object to be written to the file as data frame or matrix")
    }
    # check if headline is supplied. If it is use custom headline 
    if (hasArg(headline)){
        headline <- paste(list(...)$headline, "\n", sep="")
    } else {
        headline <- "Readabilty Indices Analysis \n" 
    }

    # is a filename supplied?
    if (is.na(filename)){
        stop("Bitte filename eingeben!")
    }
 
    require(R2HTML)                                           # for HTML output
    # write a html file
    HTMLStart(dir, filename ,HTMLframe=FALSE, Title="Readability Analysis")    
    HTML(as.title(headline))
    HTML(data)    # print data to console and to html
    HTMLStop()
}
