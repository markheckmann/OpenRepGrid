
# OpenRepGrid <a href="https://docu.openrepgrid.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/OpenRepGrid)](https://CRAN.R-project.org/package=OpenRepGrid)
[![](https://img.shields.io/badge/devel-v0.1.15-blue.svg)](https://github.com/markheckmann/OpenRepGrid)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/markheckmann/OpenRepGrid?branch=master&svg=true)](https://ci.appveyor.com/project/markheckmann/OpenRepGrid)
[![Codecov test coverage](https://codecov.io/gh/markheckmann/OpenRepGrid/branch/master/graph/badge.svg)](https://codecov.io/gh/markheckmann/OpenRepGrid?branch=master)
[![R-CMD-check](https://github.com/markheckmann/OpenRepGrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/markheckmann/OpenRepGrid/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->



>You love repertory grids? So do we! 
>Find out about a powerful tool to analyze your grids!


## Overview

**OpenRepGrid** is an R package for the analysis of repertory grid data. Its home is  [openrepgrid.org](https://openrepgrid.org/). 
                                                  
You can install the latest release version from [CRAN](https://cran.r-project.org/web/packages/OpenRepGrid/index.html) by typing

    install.packages("OpenRepGrid", dep = TRUE)
    
To install the latest development version from github you can use the `devtools` package.
    
    library(devtools)
    install_github("markheckmann/OpenRepGrid") 

To load the OpenRepGrid package after installation type

    library(OpenRepGrid) 

And for a first impression of the package features type

    demo(OpenRepGrid)


## Lifecycle

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`OpenRepGrid` is still in alpha phase. Please be aware that API changes may occur.


## Citation

If you use OpenRepGrid in your publications, you can cite it as follows. 

> Heckmann, M. (2023). OpenRepGrid: An R package for the analysis of repertory grids. *ZENODO*. doi:10.5281/zenodo.11623, R package version 0.0.15.

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11623.svg)](http://dx.doi.org/10.5281/zenodo.11623)


## Contributors

- [Mark Heckmann](https://markheckmann.de) (maintainer)
- Richard C. Bell
- Alejandro García Gutiérrez (@j4n7)
- Diego Vitali (@artoo-git)
- José Antonio González Del Puerto (@MindCartographer)
- Jonathan D. Raskin
