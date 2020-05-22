# using distance measures
indexCognitiveProfile(boeker, 1, 2, c(3:11), method = "euclidean")
indexCognitiveProfile(boeker, 1, 2, c(3:11), method = "manhattan")
indexCognitiveProfile(boeker, 1, 2, c(3:11), method = "minkowski", p = 3)

# using correlation measures
indexCognitiveProfile(boeker, 1, 2, c(3:11), method = "pearson")
indexCognitiveProfile(boeker, 1, 2, c(3:11), method = "spearman")

# printing the results (biplot only works with)
cp <- indexCognitiveProfile(boeker, 1, 2, c(3:11))
cp$grid  # grid with self, ideal and others
biplot2d(cp$grid, center = 4)  # midopoint centering
