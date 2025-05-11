x <- fbb2003

preferredPoles(x) # no preferences assigned yet

# set preference by ideal rating
x <- preferredPolesByIdeal(x, ideal = "as I would love to be")
x <- preferredPolesByIdeal(x, ideal = 7) # same with element index
x

# set preferred poles manually
preferredPoles(x) <- c("left", "right", "left", "r", "l", "l", "l", "r", "r")
x

# change preferance for constructs 1 and 5
preferredPoles(x)[2] <- "left"
x

# remove prefernces
preferredPoles(x) <- NA
x
