
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpenRepGrid: An R Package for the Analysis of Repertory Grid Data <a href="https://docu.openrepgrid.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/OpenRepGrid)](https://CRAN.R-project.org/package=OpenRepGrid)
[![](https://cranlogs.r-pkg.org/badges/OpenRepGrid)](https://cran.rstudio.com/web/packages/OpenRepGrid/index.html)
[![](https://img.shields.io/badge/devel-v0.1.15-blue.svg)](https://github.com/markheckmann/OpenRepGrid)
[![R-CMD-check](https://github.com/markheckmann/OpenRepGrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/markheckmann/OpenRepGrid/actions/workflows/R-CMD-check.yaml)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/markheckmann/OpenRepGrid?branch=master&svg=true)](https://ci.appveyor.com/project/markheckmann/OpenRepGrid)
[![Codecov test
coverage](https://codecov.io/gh/markheckmann/OpenRepGrid/branch/master/graph/badge.svg)](https://codecov.io/gh/markheckmann/OpenRepGrid?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

<br>

> You love repertory grids? So do we! Find out about a powerful tool to
> analyze your grids!

## About

**OpenRepGrid** is an R package to analyze and visualize [repertory
grid](https://en.wikipedia.org/wiki/Repertory_grid) data. The repertory
grid (often abbreviated *grid* or *repgrid*) is a data collection method
which originated from [Personal Construct
Theory](https://en.wikipedia.org/wiki/Personal_construct_theory) (Kelly
1955). It was originally designed as an instrument for psychotherapy.
Nowadays, it is used in many other fields, including market,
organizational, political, educational and sensory research. See tab
[Intro](articles/web/intro.html) for a brief introduction.

<figure>
<img src="man/figures/bertin-clustered.png"
alt="Figure 1. Example of a repertory grid dataset (with rows and columns clustered by similarity)." />
<figcaption aria-hidden="true"><strong>Figure 1.</strong> Example of a
repertory grid dataset (with rows and columns clustered by
similarity).</figcaption>
</figure>

<br>

## Installation

Install the latest release version from
[CRAN](https://cran.r-project.org/web/packages/OpenRepGrid/index.html)

    install.packages("OpenRepGrid", dep = TRUE)

Install the latest development version from github.

    library(devtools)
    install_github("markheckmann/OpenRepGrid") 

To load the OpenRepGrid package after installation type

    library(OpenRepGrid) 

And for a first impression of the package features type

    demo(OpenRepGrid)

## Contributors

- [Mark Heckmann](https://markheckmann.de) (package maintainer)
- Richard C. Bell
- Alejandro García Gutiérrez ([@j4n7](https://github.com/j4n7))
- Diego Vitali ([@artoo-git](https://github.com/artoo-git))
- José Antonio González Del Puerto
  ([@MindCartographer](https://github.com/MindCartographer))
- Jonathan D. Raskin

## References

Kelly, George Alexander. 1955. *The Psychology of Personal Constructs*.
New York: Norton.
