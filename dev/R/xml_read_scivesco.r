library(XML)
filename <- "/Users/markheckmann/Documents/Magic Briefcase/DA openRepgrid/openrepgrid/basic/data/scivesco/20100610_181315_10_MRL.scires"
xmlFile <- xmlTreeParse(filename)

root <- xmlRoot(xmlFile)
node.1.interview <- xmlChildren(root)
node.1.interview[[1]]["Expert"]
node.1.interview[[1]]["Interviewer"]
results <- node.1.interview[[1]]["InterviewResultTurnsCollection"]

xmlSize(root)
xmlSize(root[[1]])
xmlApply(root, names)



xmlValue(node.1.interview[[1]]["Expert"][[1]])
xmlValue(node.1.interview[[1]]["Interviewer"][[1]])
xmlValue(results[[1]][[1]][["Pole1"]])		# read initial and gegenpol
xmlValue(results[[1]][[1]][["Pole2"]])
xmlValue(results[[1]][[2]][["Pole1"]])

xmlAttrs(results[[1]][[2]][["RatingPole1"]][[1]])["Value"]


xmlChildren(root[[1]][["InterviewResultTurnsCollection"]])




marks <- xmlChildren(first.level.children$marking)
lapply(marks,xmlAttrs)
lapply(marks,xmlValue)


lea.xml <- xmlTreeParse("/Users/karsten/Documents/lokales/otu.lea/DH23KL_1122334455.xml")
top.node <- xmlRoot(lea.xml)
first.level.children <- xmlChildren(top.node)
marks <- xmlChildren(first.level.children$marking)
lapply(marks,xmlAttrs)
lapply(marks,xmlValue)