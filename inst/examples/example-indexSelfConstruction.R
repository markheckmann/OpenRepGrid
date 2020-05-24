# using distance measures
indexSelfConstruction(boeker, 1, 2, c(3:11), method = "euclidean")
indexSelfConstruction(boeker, 1, 2, c(3:11), method = "manhattan")
indexSelfConstruction(boeker, 1, 2, c(3:11), method = "minkowski", p = 3)

# using correlation measures
indexSelfConstruction(boeker, 1, 2, c(3:11), method = "pearson")
indexSelfConstruction(boeker, 1, 2, c(3:11), method = "spearman")

# using not-normalized distances
indexSelfConstruction(boeker, 1, 2, c(3:11), method = "euclidean", normalize = FALSE)

# printing the results (biplot only works with)
cp <- indexSelfConstruction(boeker, 1, 2, c(3:11))
cp$grid  # grid with self, ideal and others
biplot2d(cp$grid, center = 4)  # midopoint centering
