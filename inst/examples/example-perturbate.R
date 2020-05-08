## All results for PVAFF index when ratings are slightly perturbated
p <- indexPvaff(boeker)
l <- grids_perturbate(boeker, n = 100, prop = .1)
pp <- sapply(l, indexPvaff)  # apply indexPvaff function to all perturbated grids
range(pp)   # min and max PVAFF
hist(pp, xlab = "PVAFF values")    # visualize
abline(v = p, col = "blue", lty = 2)
