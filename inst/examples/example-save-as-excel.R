# save one grid in wide format (default)
file <- tempfile(fileext = ".xlsx")
saveAsExcel(boeker, file)
\dontrun{
browseURL(file) # open file, requires Excel, may not work on all system
}

# save one grid in log format
file <- tempfile(fileext = ".xlsx")
saveAsExcel(boeker, file, format = "long")
\dontrun{
browseURL(file)
}

# save a list of grids
file <- tempfile(fileext = ".xlsx")
l <- list(boeker, feixas2004, bell2010)
saveAsExcel(l, file)
\dontrun{
  browseURL(file)
}

# pass some sheet names (2nd with named sheet)
file <- tempfile(fileext = ".xlsx")
l <- list(boeker, "feixas' grid" = feixas2004)
saveAsExcel(l, file)
\dontrun{
  browseURL(file)
}
