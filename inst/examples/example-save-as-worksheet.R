library(openxlsx)

wb <- createWorkbook()

# add grid to worksheet
saveAsWorksheet(boeker, wb, sheet = "boeker")

# add several grids (2nd has a name)
l <- list(boeker, "feixas' grid" = feixas2004) #
saveAsWorksheet(l, wb) # name sheet

file <- tempfile(fileext = ".xlsx")
saveWorkbook(wb, file)

\dontrun{
  browseURL(file) # may not work on all systems
}
