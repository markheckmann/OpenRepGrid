# sample grid from Walker et al. (1988), p. 67
file <- system.file("extdata", "dep_grid_walker_1988_2.xlsx" , package = "OpenRepGrid")
x <- importExcel(file)

indexDDI(x, ds = 2:5) 

# using named vector
ds = c("2"=2, "3"=3, "4"=4, "5"=5)  
indexDDI(x, ds)
