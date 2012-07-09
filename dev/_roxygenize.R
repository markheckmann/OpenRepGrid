# run fpr package building and checking
library(roxygen2) 
library(devtools)
 
pkgdir <- "OpenRepGrid"

roxygenize(pkgdir)
build(pkg = pkgdir, binary = FALSE) 
check(pkg = pkgdir, document = TRUE)
install(pkg = pkgdir, reload = TRUE) 

load_all(pkgdir)   # source R files 

release(pkgdir)


