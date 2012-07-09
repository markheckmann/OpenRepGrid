library(plyr)

# show.pattern will display a 0/1 matrix in a grid containing
# black and white cells
show.pattern <- function(df, border= "light gray", box=TRUE)
{
  	nr <- nrow(df); nc <- ncol(df)
	plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "",
		 xaxs="i", yaxs="i")
  		for (i in 1:nr){
		rect(0:(nc-1)/nc, (nr+1-i)/nr, 
			1:nc/nc, (nr-i)/nr, 
			col = unlist(df[i,]), border = border)			
	}
	if(box) box()
}
# show.pattern(matrix(0:1, 21,17))

# The first assumption is that each person has a stable construct systems. 
# In the moment of evocation each construct has a probability of being evoced.
# At another time different constructs may be evoced due to the assumption that 
# it is a question of probability. Here, this is modelled mathematically.
# 

# setting up a construct systems with probabalities of evocation
c.names <- paste("K", LETTERS[1:20], sep="")		# construct names
c.no <- 5											# number of constructs in the system
c.prob <- rep(c(.03,.08), each=10)					# probabilities for each construct beeing evocated
c.prob <- c.prob/sum(c.prob)						# standardizing the probabilities
sample(c.names, c.no, prob=c.prob)
plot(1:20, c.prob, type="s", ylim=c(0,0.1))

c.samples <- lapply(1:5, function(...) sample(c.names, c.no, prob=c.prob))
c.samples.augmented <- c(list(c.names), c.samples)
c.table <- table(unlist(c.samples))
c.table.augmented <- table(unlist(c.samples.augmented))

plot(c.table.augmented - 1)

c.data <- data.frame(matrix(1, 1, 4))
names(c.data) <- c.names

# construct system object
c.system <- list(c.names=c.names, c.prob=c.prob)

evoceFromConstructSystem <- function(c.system, n){
	if(missing(n)) print("test")
	sample(c.system, n)
}

# setting up a construct systems with probabalities of evocation
c.names <- paste("K", LETTERS[1:20], sep="")		# construct names
c.no <- 5											# number of constructs in the system
c.prob <- rep(c(.03,.08), each=10)					# probabilities for each construct beeing evocated
c.prob <- c.prob/sum(c.prob)						# standardizing the probabilities
sample(c.names, c.no, prob=c.prob)
plot(1:20, c.prob, type="s", ylim=c(0,0.1))

# sample erstellen und mit 0/1 in data frame anordnen
n <- 50
c.samples <- lapply(1:n, function(...) {
	c.sample <- sample(c.names, c.no, prob=c.prob)
	x <- data.frame(matrix(1, 1, length(c.sample)))
	names(x) <- c.sample
	x
	})
dummy <- data.frame(matrix(0, 1, length(c.names)))
names(dummy) <- c.names
x <- do.call(rbind.fill, c(list(dummy), c.samples))
x[is.na(x)] <- 0
x <- x[-1,]



show.pattern(x)

make.c.sample <- function(n, c.names, c.no, c.prob){
	lapply(1:n, function(...) {
		c.sample <- sample(c.names, c.no, prob=c.prob)
		x <- data.frame(matrix(1, 1, length(c.sample)))
		names(x) <- c.sample
		x
		})	
}

n <- 20
c.prob.1 <- rep(c(.03,.08), each=10)
c.prob.2 <- rep(c(.08,.03), each=10)
c.sample.1 <- make.c.sample(n, c.names, c.no, c.prob.1)
c.sample.2 <- make.c.sample(n, c.names, c.no, c.prob.2)

dummy <- data.frame(matrix(0, 1, length(c.names)))
names(dummy) <- c.names
x <- do.call(rbind.fill, c(list(dummy), c.sample.1, c.sample.2))
x[is.na(x)] <- 0
x <- x[-1,]

show.pattern(x)

plot(1:length(c.names), apply(x, 2, mean), type="h")


### CLUSTERING ###
# Ward Hierarchical Clustering
d <- dist(x, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=2, border="red")

