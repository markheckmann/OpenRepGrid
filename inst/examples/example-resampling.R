## All results for PVAFF index when one construct is left out 
p <- indexPvaff(boeker)
l <- grids_leave_n_out(boeker, n = 1)
pp <- sapply(l, indexPvaff)  # apply indexPvaff function to all grids
range(pp)   # min and max PVAFF
hist(pp, xlab = "PVAFF values")    # visualize
abline(v = p, col = "blue", lty = 2)
