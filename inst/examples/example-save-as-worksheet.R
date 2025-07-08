library(openxlsx)

wb <- createWorkbook()

# add grid to worksheet
saveAsWorksheet(boeker, wb, sheet = "boeker")

# add several grids with explicit sheet names
l <- list(bell2010, feixas2004)
saveAsWorksheet(l, wb, sheet = c("Bell 2010", "Feixas 2004"))

# list names are used as sheet names. Without name, the default applies.
l <- list(boeker, "Fransella et al. 2003" = fbb2003)
saveAsWorksheet(l, wb)


# save as Excel file
file <- tempfile(fileext = ".xlsx")
saveWorkbook(wb, file)
\dontrun{
  browseURL(file) # may not work on all systems
}
