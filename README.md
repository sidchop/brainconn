---
title: "R Plotting tool for brain connectivity data"
author: "Sidhant Chopra"
output: 
  html_document:
    keep_md: true
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# brainconn <img src="man/img/logo.png" align="right" alt="" width="138.5" />  

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/LCBC-UiO/ggseg.svg?branch=master)](https://travis-ci.com/sidchop/ggconn)
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/LCBC-UiO/ggseg?branch=master&svg=true)](https://ci.appveyor.com/project/sidchop/ggconn)
  [![Coverage status](https://codecov.io/gh/sidchop/ggconn/branch/master/graph/badge.svg)](https://codecov.io/gh/sidchop/ggconn)
[![CRAN status](https://www.r-pkg.org/badges/version/ggconn)](https://CRAN.R-project.org/package=ggconn)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->


The purpose of this package is to allow for felxible, programattic and interective plotting of brain connecectivty data within Rstudio - negating the need to swap to other visualsation tools and allowing for reproduceble integration of visauisation with analysis scripts that are written in R. 

This package mainly contains two plotting functions: `brainconn()` and `brainconn3D()`. It also contails data.frames containing MNI centroid coordinates and region labels for  brain atlases.

The `brainconn()` function allows users to input a binary/weighted and dierected/undirected (i.e. symetric) connectivity matrix which can be plotted in MNI space onto several brain atlses provided with the package. 
The `brainconn3D()` allows users to input a binary and undirected connectivity matrix which is plotted in a 3D and interactive matter using [plottly](https://github.com/plotly) to 

The atleses currently included with in the packaage are: `aal116`, `aal90`, `craddock200`, `dk68`, `dkt6s`, `schafer300_n7`. Custom atleses can be added easily, see vignette [LINK]

## Installation
The package can be installed using devtools.


```r
install.packages("remotes")
remotes::install_github("sidchop/brainconn", build_vignettes = TRUE)
```

The functions are now installed, and you may load them when you want to use them.


## Use
The package also has a vignette, to help you get started. You can access it [ADD GIT LINK], or via R:

```r
library(ggconn)
#> 
#> Attaching package: 'ggconn'
#> The following object is masked from 'package:brainconn':
#> 
#>     build_plot
vignette("brainconn")
#> starting httpd help server ...
#>  done
```

The primary user input is a connectivity matrix. 

