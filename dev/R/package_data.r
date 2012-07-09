###################################################################################
###  						     PACKAGE DATA	 								###
###################################################################################

rg1 <- new("repgrid")
# now we have the very basic structure to load the data into
# let's load some data into our structure:
rg1@meta$no <- 1						# Entering some meta data
rg1@meta$name <- "John Doe"				# Entering the grid data from the interview
rg1@meta$scale.min <- 0
rg1@meta$scale.max <- 5
rg1@meta$scale.steps <- 1 
rg1@data <- data.frame(	mother= c(1,0,0,4),
 						brother= c(1,3,4,5),
						sister= c(1,0,4,2), 
						son= c(1,2,1,1) )
rg1@elements <- c("mother", "brother", "sister", "son")
rg1@constructs <- data.frame(leftPole = c("clean", "healthy", "small", "simple"), 
							 rightPole = c("dirty", "sick", "big", "difficult")	,
							stringsAsFactors=FALSE )
rg1@evocation <- data.frame(methode=c("opposite", "opposite"), 
							element1=c("mother", "son"), 
							element2=c("sister", "brother"),
							element3=c(NA, NA) )


rg2 <- randomGrid(ncol=8, nrow=4)
rg2@meta$name <- "John Doe"				# Entering the grid data from the interview
rg2@elements <- c(	"mother", 
					"brother", 
					"sister",
					"son", 
					"aunt", 
					"uncle", 
					"daughter", 
					"someone I have wanted to meet for a long time")
rg2@constructs <- data.frame(leftPole = c(	"somewhat nice and clean", 
										  	"healthy looking", 
										   	"small", 
											"simple minde"), 
							 rightPole = c(	"distgusting because it's dirty and somewhat rogue", 
											"sick, in bad shape", 
											"big", 
											"difficult to hanlde"),
							stringsAsFactors=FALSE )

# data from: Meyer, Aderhold, Teich (). OPTIMIZATION OF SOCIAL STRUCTURE IN BUSINESS NETWORKS BY 

rg3 <- randomGrid(ncol=4, nrow=5, scale=c(1,5))
rg3@meta$name <- "Meyer, Aderhold, Teich"				# Entering the grid data from the interview
rg3@elements <- c(	"construction", 
					"controlling", 
					"computer",
					"design")
rg3@constructs <- data.frame(leftPole = c(	"co-operative", 
										  	"creative", 
										   	"communicative", 
											"conflictive",
											"dependent"), 
							 rightPole = c(	"obstinate", 
											"naive", 
											"silent", 
											"sensitive",
											"independent"),
							stringsAsFactors=FALSE )