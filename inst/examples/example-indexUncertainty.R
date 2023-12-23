# sample grid from Bell (2001, p.231)
file <- system.file("extdata", "dep_grid_bell_2001.xlsx" , package = "OpenRepGrid")
x <- importExcel(file)

indexUncertainty(x) 
