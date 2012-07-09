################################################################################################################
# Defining the basic structure of the repgrid object.
# The repgrid object in which all the relevant grid data will be stored will
# have the following structure:
#													(meta contains all meta data for the repgrid, like:)
# 	repgrid ---+--- meta ---+--- number				(some administrative number of interview)
#			   |            +--- name				(name of interviewee)
#              |            +--- inputfilename	 	(name of inputfile)
#              |            +--- ...				(any other necessary meta data)
#			   |
#              +--- data							(a table containing the answers, like:)
#			   |									( 1 2 5 0 3 )
#              |									( 0 0 1 4 2 )
# 			   |									( 1 5 2 3 3 )
# 			   |									( this depend on the used format)
# 			   |
#			   +--- elements						(a vector of elements, e.g. "me now", "my mother" etc.)
#			   |
#              +--- constructs						(a table containing the name of the left and right pole e.g.:)
#			   |									("dirty"	"clean" )
#			   |									("honest" 	"liar"	)
#			   |
#			   +--- evocation						( a table containing evocation method and elements used in evocation)
#			   |									( "opposite"	"me now"	"mother")
#			   |									( "opposite"	"mother"	"father")
#			   |	
#			   +--- results
#
################################################################################################################

# Design: 	the objects will be defined using S4 classes. This might be a bit of a overkill for all this
# 			but I willgive it a shot.

# first we define the structure and a prototype of the S4 class repgrid
setClass( "repgrid", 
		  representation( meta = "list",
						  data = "data.frame",
						  elements = "vector",
						  constructs = "data.frame",
						  evocation = "data.frame",
						  results = "list"),
		  prototype( meta = list( 	no = NA,
								  	name = NA,
								  	scale.min = NA,
									scale.max = NA,
									scale.steps = NA),
					 data = data.frame(element1=NA, element2=NA, element3=NA, element4=NA),
					 elements = c("element1", "element2", "element3", "element4"),
					 constructs = data.frame(leftPole=c("construct1 left pole", "construct2 left pole"), rightPole=c("construct1 right pole", "construct2 right pole")),
					 evocation = data.frame(method=c("opposite"), element1= "element1", element2="element2", element3=NA),
					results = list())
		 )


# lets now define a new show method
setMethod("show", signature= "repgrid", function(object){
	cat("==================================\n")
	cat("Data from Repertory Grid Interview\n")
	cat("==================================\n")
	if(!is.na(object@meta$no)) cat("Interview number: ", object@meta$no, "\n")		# print Meta data
	if(!is.na(object@meta$name)) cat("Interviewee: ", object@meta$no, "\n")
	if(!is.na(object@meta$scale.min & object@meta$scale.max)) {cat("Scale used for ratings ranges from", object@meta$scale.min, 
									 "to", object@meta$scale.max,
									 "using steps of", object@meta$scale.steps,"\n")}
	
	cat("\n")
	cat("Collected Grid Data:\n")
	tmp <- as.data.frame(c(	subset(object@constructs, , 1), 
					  		object@data,
					  		subset(object@constructs, , 2) ))
	names(tmp) <- c("left pole", object@elements, "right pole")
	print(tmp) 
	cat("\n")
	cat("Method and order of evocation:\n")
	print(object@evocation)
})

# TODO
# let's creat a new method, e.g. a method that returns the grid data
setGeneric("bertin", function(object) {
	standardGeneric("bertin") 
})

setMethod("bertin", signature(object="repgrid"),
	function(object) { 
	tmp <- as.data.frame(c(subset(object@constructs, , 1), 
					  object@data,
					  subset(object@constructs, , 2) ))
	print(tmp)
})

# let's create a method that writes our standard output to a textfile we choose
setGeneric("saveToTxt", function(object) {
	standardGeneric("saveToTxt") 
})

setMethod("saveToTxt", signature(object="repgrid"), function(object) { 
	require(tcltk)
	fileName <- tclvalue(tkgetSaveFile())
	
	# write txt file
	con <- file(fileName, "w")                                        # open an output file connection
	cat("==================================\n", file = con)
	cat("Data from Repertory Grid Interview\n", file = con)
	cat("==================================\n", file = con)
	if(!is.na(object@meta$no)) cat("Interview number: ", object@meta$no, "\n", file = con)		# print Meta data
	if(!is.na(object@meta$name)) cat("Interviewee: ", object@meta$no, "\n", file = con)
	cat("\n", file = con)
	cat("Collected Grid Data:\n", file=con)
	tmp <- as.data.frame(c(subset(object@constructs, , 1), 
					  object@data,
					  subset(object@constructs, , 2) ))
	write.table(tmp, file = con, sep = "\t\t\t", row.names = FALSE)      # write analysis data
	cat("\n", file=con)
	cat("Method and order of evocation:\n", file=con)
	write.table(object@evocation, file = con, sep = "\t", row.names = FALSE)      # write analysis data
	close(con)
	cat("File: ", fileName, " erstellt")

	return(fileName)
})

###################################################################
###   					GENERATE WORD OUTPUT					###
###################################################################

setGeneric("saveToWord", function(object) {
	standardGeneric("saveToWord") 
})

setMethod("saveToWord", signature(object="repgrid"), function(object) { 
	require(tcltk)
	require(R2wd)
	
	#fileName <- tclvalue(tkgetSaveFile())
		
	wdGet()
	wdTitle("Repertory Grid Analysis Output",label="")
	wdSection("Meta data")

	if(!is.na(object@meta$no)) 	wdBody(cat("Interview number: ", object@meta$no, "\n", file = con))		
	wdSave("MyDoc")
	##
	wdQuit()

	## End(Not run)
	#cat("File: ", fileName, " erstellt")

	#return(fileName)
})


#if(!is.na(rg1@meta$no)) 	wdBody(cat("Interview number: ", rg1@meta$no, "\n"))	





