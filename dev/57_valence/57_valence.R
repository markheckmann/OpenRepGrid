# feat: add construct pole valence (positive/negative/neutral) to repgrid object #57

devtools::load_all()


file <- system.file("extdata", "grid_01b.txt", package = "OpenRepGrid")
importTxt(file)


file <- system.file("extdata", "grid_01.xlsx", package = "OpenRepGrid")
importExcel(file)


file <- system.file("extdata", "grid_01b.xlsx", package = "OpenRepGrid")
importExcel(file)

