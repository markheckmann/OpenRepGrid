\dontrun{
biplot3d(boeker)

biplot3d(boeker, e.sphere.show = 1:4)
biplot3d(boeker, e.sphere.show = 1:4, e.labels.show = 1:2)
biplot3d(boeker, c.axis.show = 1:2)

biplot3d(boeker, e.sphere.col = "red", c.text.col = "blue")
biplot3d(boeker, e.cex = 1)
biplot3d(boeker, col.sphere = "red")

biplot3d(boeker, g = 1, h = 1) # INGRID biplot
biplot3d(boeker, g = 1, h = 1, center = 4) # ESA biplot
}
