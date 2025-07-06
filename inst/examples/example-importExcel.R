file <- system.file("extdata", "grid_01.xlsx", package = "OpenRepGrid")
rg_1 <- importExcel(file, sheet = "wide")
rg_2 <- importExcel(file, sheet = "long", format = "long")

# Open Sample file to inspect the two (requires Excel to be installed).
\dontrun{
browseURL(file) # may not work on all systems }

# Import more than one Excel file
files <- system.file("extdata", c("grid_01.xlsx", "grid_02.xlsx"), package = "OpenRepGrid")
rgs <- importExcel(files) # returns a list of grids
