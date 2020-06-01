m <- indexBieri(boeker)

# several output options
print(m)
print(m, output = "IC")  # construct matches

# extract the matrix of matches
m$constructs

# CAVEAT: Bieri's index changes when constructs are reversed
nr <- nrow(boeker)
l <- replicate(1000, swapPoles(boeker, sample(nr, sample(nr, 1))))
bieri <- sapply(l, function(x) indexBieri(x)$bieri)
hist(bieri, breaks = 50)
abline(v = mean(bieri), col = "red", lty = 2)

