file <- system.file("extdata", "grid_01.xlsx", package = "OpenRepGrid")
rg_1 <- importExcel(file, sheet = "wide")
rg_2 <- importExcel(file, sheet = "long", format = "long")

# Open sample file to inspect it (requires Excel to be installed).
\dontrun{
browseURL(file) # may not work on all systems }

# Import more than one Excel file
files <- system.file("extdata", c("grid_01.xlsx", "grid_02.xlsx"), package = "OpenRepGrid")
rgs <- importExcel(files) # returns a list of grids

# Impoort from several sheets at once (all must have same format)
file <- system.file("extdata", "grid_03.xlsx", package = "OpenRepGrid")
rgs <- importExcel(file, sheet = 1:3) # by index
rgs <- importExcel(file, sheet = c("grid 1", "grid 2", "grid 3")) # or by name
