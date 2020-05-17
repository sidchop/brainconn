  
---
title: "Plotting tool for brain connectivity data"
author: "Sidhant Chopra"
output: github_document
---





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
vignette("brainconn")
```

The primary user input is a connectivity matrix. 

```r
x <- read.csv("data/example/example_unweighted_undirected.txt", header = F)
```


```r
brainconn(atlas ="Stages_melbBrain", conmat=x)
```

<img src="man/img/README-unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="50%" />
\
Modifyble features for `brainconn` include: `view`, `node.size`, `node.color`, `edge.width`, `edge.color`, `edge.alpha`, `background.alpha`, `labels` and others (see vignette)



```r
x <- read.csv("data/example/example_unweighted_undirected.txt", header = F)
p <- brainconn3D(atlas ="Stages_melbBrain", conmat=x, show.legend = F)
p
```
Included below is a screenshot of the interactive output: 
![plot of chunk unnamed-chunk-7](man/img/README-ggseg3d_example.png)
\
Modifyble features for `brainconn3D` include: `node.size`, `node.color`, `edge.width`, `edge.color`, `adge.alpha`, `background.alpha`, `labels` and others (see vignette)


### Report bugs or requests  
Don't hesitate to ask for support using [github issues](https://github.com/sidchop/brainconn), or requesting new atlases. 
While we would love getting help in creating new atlases, you may also request atlases through the issues, and we will try to get to it. 

