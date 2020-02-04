
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package `lcra`

A user-friendly interface for doing joint Bayesian latent class and
regression analysis

## Overview

This `R` package provides a user-friendly interface for fitting Bayesian
joint lantend class and regression models. Unsing the standard R syntax,
the user can specify the form of the regression model and the desired
number of latent classes.

The technical details of the model implemented here are described in
Elliott, Michael R., Zhao, Zhangchen, Mukherjee, Bhramar, Kanaya, Alka,
Needham, Belinda L., “Methods to account for uncertainty in latent class
assignments when using latent classes as predictors in regression
models, with application to acculturation strategy measures” (2020) In
press at Epidemiology. <doi:10.1097/EDE.0000000000001139>

## Installation

This package uses the R interface for WinBUGS called `R2WinBUGS`. The
package `R2WinBUGS` in turn depends on the standalone Windows program
`WinBUGS`. Follow this link: [University of Cambridge MRC Biostatistics
Unit](https://www.mrc-bsu.cam.ac.uk/software/bugs/the-bugs-project-winbugs/)
for WinBUGS download and installation instructions. `WinBUGS` can also
be used on Mac using the additional software
[Wine](https://www.winehq.org/). Plenty of tutorials that demonstrate
install of WinBUGS on non-Windows machines are available on Google. With
a bit of work, unix users can run this software, too.

Once the standalone `WinBUGS` program is installed, open R and run:

If the devtools package is not yet installed, install it first:

``` r
install.packages('devtools')
library(devtools)
```

``` r
# install lcra from Github:
install_github('umich-biostatistics/lcra') 
library(lcra)
```

## Example usage

The main function for the Bayesian Bayesian joint lantend class and
regression model is `bayes_lca()`. Use `?bayes_lca()` for the R help
file.

Here is an example Bayesian latent class analysis on simulated data:

### Vignette

Once the package is installed, run the following to launch a vignette
with detailed examples of the package functions.

### Current Suggested Citation

“Methods to account for uncertainty in latent class assignments when
using latent classes as predictors in regression models, with
application to acculturation strategy measures” (2020) In press at
Epidemiology. <doi:10.1097/EDE.0000000000001139>
