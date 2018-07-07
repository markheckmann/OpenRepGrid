

## 1.
# simple replacement function

foo <- function(x) x

`foo<-` <- function(x, position, value) {
  x[position] <- value
  x
}

v <- 1:3
foo(v)[2] <- NA
v

## 2. 
# now with two indexes, with m being a matrix
foo2 <- function(x) x

`foo2<-` <- function(x, i, j, value) {
  x[i, j] <- value
  x
}

# works for matrix
m <- matrix(1,3,3)
foo2(m)[2,3] <- 10
m

# .. and dataframes
m <- as.data.frame(m)
foo2(m)[2,3] <- NA
m


## 3.
# now for a list things go south
foo3 <- function(x) x

`foo3<-` <- function(x, i, j, value) {
  x[j][i] <- value
  x
}

l <- list(a=1:3, b=3:1)
foo3(l)[1] <- 1
# class(l) <- c("matrix", class(l))

foo3(l)[1,1] <- 10
l




