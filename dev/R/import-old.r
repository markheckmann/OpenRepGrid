###################################################################################
###  		VARIUOS FUNCTIONS TO IMPORT GRID DATA FROM OTHER	 				###
###			GRID SOFTWARE 														###
###																				###
###			CURRENTLY IMPORTS FROM:  gridstat, gridcor							###
###			2010																###
###################################################################################


###################################################################################
### 							 GRIDSTAT										###
###################################################################################

# gridstat outpout has the following form:

# Heinz Boeker in Scheer & Catina (1996, p.163)
# 14 15
# balanced - get along with conflicts
# isolated - sociable
# closely integrated - excluded
# discursive - passive
# open minded - indifferent
# dreamy - dispassionate
# practically oriented - depressed
# playful - serious
# socially minded - selfish
# quarrelsome peaceful
# artistic - technical
# scientific - emotional
# introvert - extrovert
# wanderlust - home oriented
# self
# ideal self
# mother
# father
# kurt
# karl
# george
# martin
# elizabeth
# therapist
# irene
# childhood self
# self before illness
# self with delusion 
# self as dreamer
# 1 4 2 2 3 5 2 5 4 2 6 2 2 3 3
# 3 6 3 5 5 4 5 4 5 4 4 4 2 2 3
# 2 2 2 3 5 3 2 3 2 3 3 4 4 5 3
# 4 1 3 1 2 4 2 3 3 2 3 3 3 5 4
# 2 1 2 1 2 4 4 2 4 2 6 3 2 2 3
# 4 5 3 5 4 5 4 5 4 4 6 3 3 3 2
# 2 1 3 2 3 3 3 2 2 3 2 3 3 3 3
# 4 5 4 3 4 3 2 3 4 4 5 3 2 4 3
# 2 1 3 2 4 5 4 1 3 2 6 3 3 3 3
# 5 5 5 5 5 2 5 2 4 4 1 6 5 5 5
# 5 1 2 4 3 5 3 2 4 3 3 4 4 4 4
# 2 1 5 3 4 4 5 3 4 1 6 4 2 3 3
# 4 5 4 6 5 3 5 3 5 2 5 2 2 2 3
# 1 1 4 2 4 5 2 5 5 3 6 1 1 2 1

# 1) first line:  some description elements. 
# 2) second line: number of constructs and elements
# 3) next n lines: constructs, elements
# 4) matrix of ratings
# These have to be parsed. For this purpose they are read in line by line.

###################################################################################
# importGridstat() imports data files that are saved in the gridstat format
# file	filename including path if file is not in current working directory.
#		file can also be a complete URL.
# dir	alternative way to supply directory where file is located (default NULL)
# max	optional argument (numeric) for maximum rating value in grid
#
importGridstat <- function(file, dir=NULL, max=NULL){
	joinString <- function(x) paste(unlist(x), sep="", collapse=" ")
	trimBlanksInString <- function(x) sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
	
	if(!is.null(dir)) file <- paste(dir, file, sep="/", collapse="")
	# read meta info
	l <- list()																			# list object to store all data in
	datainfo <- scan(file = file, what = "raw", skip=0, nlines=1, quiet = TRUE)			# Read information line (first line)
	l$datainfo <- joinString(datainfo)													# join single items to long string
	noConstructAndElements <- scan(file = file, what = "integer", 
									skip=1, nlines=1, quiet = TRUE)  					# Read number of elements and constructs
	l$noConstructs <- as.numeric(noConstructAndElements)[1]								# no of constructs
	l$noElements <- as.numeric(noConstructAndElements)[2]								# no of elements
	l$maxValue <- NA
	
	# read constructs
	l$constructs <- list()
	l$emergentPoles <- list()
	l$contrastPoles <- list()
	for(i in 1:l$noConstructs){															
		tmp <- scan(file = file, what = "character", skip=2+i-1, nlines=1, quiet = TRUE)# read construct line by line
		l$constructs[[i]] <- joinString(tmp)											# make one string
		poles <- strsplit(l$constructs[[i]], "-")										# separate emergent and constrast pole by splitting at hyphene
		l$emergentPoles[[i]] <- trimBlanksInString(poles[[1]][1])						# save emergent pole
		l$contrastPoles[[i]] <- trimBlanksInString(poles[[1]][2])						# save constrast pole
	}

	# read elements
	l$elements <- list()
	for(i in 1:l$noElements){															
		tmp <- scan(file = file, what = "character", skip=2+l$noConstructs+(i-1), 
					nlines=1, quiet = TRUE)												# read elements line by line 
		l$elements[[i]] <- trimBlanksInString(joinString(tmp))
	}

	# read ratings
	l$ratings <- list()
	for(i in 1:l$noConstructs){
		tmp <- scan(file = file, what = "character", quiet = TRUE, 
					skip=2 + l$noElements + l$noConstructs+(i-1), nlines=1) 			# read ratings line by line
		l$ratings[[i]] <- as.numeric(tmp)
	}
	# infer maximum rating value if not provided in max argument
	if(is.null(max)) l$maxValue <-max(unlist(l$ratings), na.rm=TRUE) else l$maxValue <- max
	
	l
}

# NOT RUN:
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridstat.dat"
# rg <- importGridstat(file)
# 
# file <- "gridstat.dat"
# dir <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/"
# rg <- importGridstat(file, dir)
# 
# # load gridstat data from URL
# rg <- importGridstat("http://www.openrepgrid.uni-bremen.de/data/gridstat.dat")
# 
# # note that the gridstat data format does not contain infpormation about the maximum
# # possible value for rating. By default the maximum value is inferred by scanning
# # the rating and picking the maximum value. You can set the max value by hand using the
# # max argument
# rg <- importGridstat(file, dir, max=6)

###################################################################################







###################################################################################
### 							 GRIDCOR 										###
###################################################################################

# gridcor outpout has the following form:

#   14  15   6
# Heinz Boeker in Scheer & Catina (1996, p.163)                                   
# 122352542622334
# 335545454442236
# 223532323344532
# 431242332333541
# 221244242632231
# 435454544633325
# 232333223233331
# 443432344532435
# 232454132633331
# 555525244165555
# 524353243344441
# 253445341642331
# 446535352522235
# 142452553611211
# self
# mother
# father
# kurt
# karl
# george
# martin
# elizabeth
# therapist
# irene
# childhood self
# self before illness
# self with delusion
# self as dreamer
# ideal self
# balanced
# isolated
# closely integrated
# discursive
# open minded
# dreamy
# practically oriented
# playful
# socially minded
# quarrelsome
# artistic
# scientific
# introvert
# wanderlust
# get along with conflicts
# sociable
# excluded
# passive
# indifferent
# dispassionate
# depressed
# serious
# selfish
# peaceful
# technical
# emotional
# extrovert
# home oriented

# "As you can see in this sample file, the first line contains the number of constructs (10), 
# of elements (13) and the maximum scale range (7), separated by two spaces each. The second 
# line is the title of the analysis. Next is the data matrix itself, and the labels: first, 
# the element labels, then the labels of the right poles of the constructs, and finally the 
# left pole ones."
# as retrieved from http://www.terapiacognitiva.net/record/manualgri/man3.htm on 07/Sep/2010


###################################################################################
# importGridcor() imports data files that are saved in the gridcor format
# file	filename including path if file is not in current working directory.
#		file can also be a complete URL.
# dir	alternative way to supply directory where file is located (default NULL)
#

importGridcor <- function(file, dir=NULL){
	joinString <- function(x) paste(unlist(x), sep="", collapse=" ")
	trimBlanksInString <- function(x) sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
	
	if(!is.null(dir)) file <- paste(dir, file, sep="/", collapse="")
	# read meta info

	datainfo <- scan(file = file, what = "raw", skip=1, nlines=1, quiet = TRUE)			# Read information line (first line)
	l$datainfo <- joinString(datainfo)													# join single items to long string	
	meta <- scan(file = file, what = "integer", 
				 skip=0, nlines=1, quiet = TRUE)  										# Read number of elements and constructs
	l$noConstructs <- as.numeric(meta)[1]												# no of constructs
	l$noElements <- as.numeric(meta)[2]													# no of elements
	l$maxValue <- as.numeric(meta)[3]													# maximum value for Likert scale (min is 1)

	# read elements
	l$elements <- list()
	for(i in 1:l$noElements){																							
		tmp <- scan(file = file, what = "character", skip=2+l$noConstructs+(i-1), 
					nlines=1, quiet = TRUE)												# read elements line by line 
		l$elements[[i]] <- trimBlanksInString(joinString(tmp))
	}
	
	# read constructs
	l$constructs <- NA																	# poles come separately in gridcor files, no need to separate them
	l$emergentPoles <- list()
	for(i in 1:l$noConstructs){															
		tmp <- scan(file = file, what = "character", skip=2+l$noConstructs+l$noElements+(i-1), 
					nlines=1, quiet = TRUE)# read construct line by line
		l$emergentPoles[[i]] <- joinString(tmp)											# save emergent pole
	}
	
	l$contrastPoles <- list()
	for(i in 1:l$noConstructs){															
		tmp <- scan(file = file, what = "character", skip=2+l$noConstructs+l$noElements+
					l$noConstructs+(i-1), nlines=1, quiet = TRUE)						# read construct line by line
		l$contrastPoles[[i]] <- joinString(tmp)											# save contrast pole
	}
	
	# read ratings
	l$ratings <- list()
	for(i in 1:l$noConstructs){
		tmp <- scan(file = file, what = "character", quiet = TRUE, skip=2 +(i-1), nlines=1) # read ratings line by line
		l$ratings[[i]] <- strsplit(tmp, split="")
	}
	l
}

# NOT RUN:
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridcor.dat"
# rg <- importGridcor(file)
# 
# file <- "gridcor.dat"
# dir <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/"
# rg <- importGridcor(file, dir)
# 
# # load gridstat data from URL
# rg <- importGridcor("http://www.openrepgrid.uni-bremen.de/data/gridcor.dat")







###################################################################################
### 							 IDIOGRID										###
###################################################################################







###################################################################################
### 							 GRIDSUITE										###
###################################################################################
#
# on www.gridsuite.de there are example files and XSD schemes available.
# the developers propose XML as the standard grid format
#

importGridsuite <- function(file, dir=NULL){
	if(!is.null(dir)) file <- paste(dir, file, sep="/", collapse="")
	
	require(XML)																	# use of package XML
	xmlFile <- xmlTreeParse(file)													# parse XML file
	root <- xmlRoot(xmlFile)														# get root node

	### header node
	level.1.children <- xmlChildren(root)											# get header node
	level.1.header.children <- xmlChildren(level.1.children$header)					# get header child nodes
	l$topic <- xmlValue(level.1.header.children$topic)								# topic
	l$name <- xmlValue(level.1.header.children$name)								# name
	l$interviewer <- xmlValue(level.1.header.children$interviewer)					# interviewer
	l$date <- xmlValue(level.1.header.children$date)								# date
	l$comment <- xmlValue(level.1.header.children$comment)							# comment

	### elements node
	level.1.elements.children <- xmlChildren(level.1.children$elements)				# get elements node
	tmp <- lapply(level.1.elements.children, xmlValue)								# get element names (values)
	l$elements <- tmp[names(tmp) %in% "element"]									# 
	l$noElements <- length(l$elements)												# number of elements

	### constructs node
	level.1.constructs.children <- xmlChildren(level.1.children$constructs)
	l$noConstructs <- length(level.1.constructs.children)							# number of constructs
	l$leftPoles <- lapply(level.1.constructs.children, function(x)	xmlValue(xmlChildren(x)[[1]]) )
	l$rightPoles <- lapply(level.1.constructs.children, function(x)	xmlValue(xmlChildren(x)[[2]]) )
	l$constructComments <- lapply(level.1.constructs.children, function(x) xmlValue(xmlChildren(x)$comment))

	### ratings
	ratingMeta <- xmlAttrs(level.1.children$ratings)
	l$scaleMin <- ratingMeta["min"]
	l$scaleMax <- ratingMeta["max"]
	l$scaleLevel <- ratingMeta["scale"]

	# get ratings for each e and c
	# ratings are saved constructwise as a vector in a list
	level.1.ratings.children <- xmlChildren(level.1.children$ratings)
	ratings <- matrix(NA, ncol=l$noElements, nrow=l$noConstructs )					# matrix to save ratings
	for(i in seq_along(level.1.ratings.children)){
		x <- level.1.ratings.children[[i]]
		attrs <- xmlAttrs(x)
		col <- as.numeric(attrs["ele_id"])
		row <- as.numeric(attrs["con_id"])
		ratings[row, col] <- as.numeric(xmlValue(x))	
	}
	l$ratings <- split(ratings, row(ratings))   	# convert to list
	l	
}


# TODO: the elemet and construct ids are not used. Thus, if the output should be in different order
# the current mechanism will cause false assignments

# # NOT RUN:
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridsuite.xml"
# rg <- importGridsuite(file)
# 
# file <- "gridsuite.xml"
# dir <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/"
# rg <- importGridsuite(file, dir)

# # load gridstat data from URL: Does not work yet!
# #rg <- importGridsuite("http://www.openrepgrid.uni-bremen.de/data/gridsuite.xml")








###################################################################################
### 							 SCI:VESCO										###
###################################################################################

# scivesco saves single grids in .scires files which have an XML structure.
# Note: not all nodes are imported by importScivesco()
# Overview of file structure: 

# <Interview>
# 	<Expert>
# 	<Interviewer> 
# 	<Notes>
# 	<InterviewSettingData> 
# 		<Description>
# 		<Language>
# 		<ReceiveConstructType>
# 		<RatingType>
# 		<Elements>
# 			<ElementsCollection>
# 				<Element>
# 				<Element>
# 				<...>
# 	<DoneCombinations>
# 	... TODO
# 	<InterviewAdditionalQuestionAnswerResults>
# 	... TODO
# 	<InterviewResultTurnsCollection>
# 		<InterviewResultTurn>
# 			<UsedElementCombination>
# 				<ElementCombination>
# 					<Element>
# 					<Element>
# 					<Element>          # max three elements in case of triad method (also allows monadic and dyadic)
# 			<Pole1>
# 		 	<Pole2>
# 		 	<RatingPole1>				# note that sci:vesco uses decoupled ratings for the poles
# 				<Rating>
# 				<Rating>
# 				...
# 			<RatingPole2>				# due to Tetralemma field rating type
# 				<Rating>
# 				<Rating>
# 				...		 
        
file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/scivesco.scires"

require(XML)
xmlFile <- xmlTreeParse(file)
root <- xmlRoot(xmlFile)

### interview node
l <- list()
level.1.children <- xmlChildren(root)											# get interview top node
level.1.children$Interview
l$id <- xmlAttrs(level.1.children$Interview)["Id"]								# interview ID
l$date <- xmlAttrs(level.1.children$Interview)["StartDateTime"]					# interview time

node.2.interview <- xmlChildren(level.1.children$Interview)
l$interviewee <- xmlValue(node.2.interview$Expert)								# name of expert/interviewee
l$interviewer <- xmlValue(node.2.interview$Interviewer)							# name of interviewer
l$interviewNotes <- xmlValue(node.2.interview$Notes)							# interview notes

# InterviewSettingData node
level.3.InterviewSettingData <- xmlChildren(node.2.interview$InterviewSettingData)
l$interviewDescription <- xmlValue(level.3.InterviewSettingData$Description)
l$ratingType <- xmlValue(level.3.InterviewSettingData$RatingType)

level.4.Elements <- xmlChildren(level.3.InterviewSettingData$Elements)				# get Elements child nodes 
l$noElements <- as.numeric(xmlAttrs(level.4.Elements$ElementsCollection)["Count"])	# get number of elements
level.5.Elements <- xmlChildren(level.4.Elements$ElementsCollection)				# get number of elements
getElementIdAndName <- function(x){
	id <- xmlAttrs(x)["Id"]
	children <- xmlChildren(x)
	name <- xmlValue(children$Name)
	c(id, name=name)
}
l$elements <- lapply(level.5.Elements, getElementIdAndName)

# InterviewResultTurnsCollection
level.3.InterviewResultTurnsCollection <- xmlChildren(node.2.interview$InterviewResultTurnsCollection)
getEmergentPole <- function(x) xmlValue(xmlChildren(x)$Pole1)
getContrastPole <- function(x) xmlValue(xmlChildren(x)$Pole2)
l$emergentPoles <- lapply(level.3.InterviewResultTurnsCollection, getEmergentPole)
l$contrastPoles <- lapply(level.3.InterviewResultTurnsCollection, getContrastPole)
names(l$emergentPoles) <- rep("emergentPole", length(l$emergentPoles))				# cosmetic surgery to get nicer list names
names(l$contrastPoles) <- rep("contrastPole", length(l$contrastPoles))				# cosmetic surgery to get nicer list names

# get ratings for both poles
# Here, for each construct/comparison two separate ratings are collected resulting in two ratings list
# ratings1 and ratings2.
#
#
l$ratingsEmergent <- NA
l$ratingsContrast <- NA
x <- level.3.InterviewResultTurnsCollection[[1]]
ratingsList <- xmlChildren(xmlChildren(x)$RatingPole1)
rating <- lapply(ratingsList, xmlAttrs)


# substitute each elementId by its name
getElementNameFromId <- function(id, l){
	x["ElementId"]
}



# TODO. what happens if an element is not rated?




###################################################################################

# TODO: rewrite functions with readLines???
# file <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/foreign/gridcor.dat"
# readLines(file)
# unlink(file)















