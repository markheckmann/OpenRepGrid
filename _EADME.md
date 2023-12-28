
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

# Introduction

**OpenRepGrid** is an R package to analyze and visualize [repertory grid](https://en.wikipedia.org/wiki/Repertory_grid) (often abbreviated *grid* or *repgrid*) data. The repertory grid technique (RGT) is a data collection method which originated from *Personal Construct Theory (PCT)* [@kelly_psychology_1955]. It was originally designed as an instrument for psychotherapy to shed light on a client’s construction of the world. Over subsequent decades, the technique has been adopted in many other fields, including market, organizational, political, educational and sensory research [@fransella_manual_2004]. 

The data the RGT generates is *qualitative* and *quantitative*. On the qualitative side, the technique elicits the repertory of bipolar attributes (e.g. *smart vs. dull*, so called *constructs* in PCT terminology) an individual uses to make distinctions between entities of the world (e.g. different people, so called *elements* in PCT terminolgy). On the quatitative side, it requires rating each element on each elicited personal construct (e.g. *Martin* gets a score of 2 on the *quarrelsome = 1 vs. peaceful = 6* construct, indicating that Martin is quite quarrelsome). The result of the data collection procedure is a data matrix. The constructs are usually presented as matrix rows, the elements as columns and each cell contains the corresponding rating score. Figure 1 depicts a repertory grid data set, with the rows (constructs) and columns (elements) being clustered by similarity (see below for details). A thorough introduction to the repertory grid technique is given by @fransella_manual_2004.


## Overview

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
