# let's create a method that writes our standard output to a textfile we choose
setGeneric("saveToTxt", function(x) {
	standardGeneric("saveToTxt") 
})

setMethod("saveToTxt", signature(x="repgrid"), function(x) { 
	require(tcltk)
	fileName <- tclvalue(tkgetSaveFile())
	
	# write txt file
	con <- file(fileName, "w")                                        # open an output file connection
	cat("==================================\n", file = con)
	cat("Data from Repertory Grid Interview\n", file = con)
	cat("==================================\n", file = con)
	if(!is.null(x@meta$id)) cat("Interview id: ", x@meta$id, "\n", file = con)		# print Meta data
	if(!is.null(x@meta$name)) cat("Interview partner: ", x@meta$name, "\n", file = con)
	cat("\n", file = con)
	cat("Collected Grid Data:\n", file=con)
	data <- cbind(x@constructs$left, 
				  as.data.frame(x@ratings[,,1]), 
				  x@constructs$right)
	#elementNames <- unlist(x@elements)
	elementNames <- paste("e", seq_along(x@elements), sep="")
	names(data) <- c("left pole", elementNames, "right pole")
	write.table(data, file = con, sep = "\t\t\t", row.names = FALSE)      # write analysis data
	cat("\n", file=con)
	legend <- data.frame(abbreviation=elementNames, "element"=unlist(x@elements))
	write.table(legend, file = con, sep = "\t\t\t", row.names = FALSE)      # write analysis data
	
	#cat("Method and order of evocation:\n", file=con)
	#write.table(object@evocation, file = con, sep = "\t", row.names = FALSE)      # write analysis data
	close(con)
	cat("File: ", fileName, " erstellt")

	return(fileName)
})


