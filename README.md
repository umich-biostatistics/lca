
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package `lcra`

[![](https://img.shields.io/badge/doi-10.1097/EDE.0000000000001139-yellow.svg)](https://doi.org/10.1097/EDE.0000000000001139)
[![](https://img.shields.io/badge/devel%20version-1.0.0.9000-blue.svg)](https://github.com/umich-biostatistics/lcra)
[![](https://img.shields.io/github/languages/code-size/umich-biostatistics/lcra.svg)](https://github.com/umich-biostatistics/lcra)

A user-friendly interface for doing joint Bayesian latent class and
regression analysis with binary and continuous outcomes.

Simply specify the regression model and number of classes and lcra
predicts class membership for each observation and accounts for
uncertainty in class membership in the estimation of the regression
parameters.

## Why use this package?

This is the only package available for joint latent class and regression
analysis. Other packages use a sequential procedure, where latent
classes are determined prior to fitting the usual regression model. The
only Bayesian alternative will require you to program the model by hand,
which can be time consuming.

## Overview

This `R` package provides a user-friendly interface for fitting Bayesian
joint latent class and regression models. Using the standard R syntax,
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

### Quick example:

``` r
inits = list(list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 2), 
                  alpha = rep(0, length = 3), tau = 0.5))
            
fit = lcra(formula = y ~ x1 + x2, family = "gaussian", data = express,
           nclasses = 3, inits = inits, manifest = paste0("Z", 1:5),
           n.chains = 1, n.iter = 500, parameters.to.save = c("beta", "true", "alpha"))
  
fit$mean$alpha
#  0.8544998  0.4502046 -0.2890094

fit$mean$beta
# -2.9268346  0.1501557

fit$median$true
```

NOTE: WinBUGS throws an error on Windows machines if you are not running
as administrator. The error reads: Error in file(con, “wb”) : cannot
open the connection.

Ignore the error. Once WinBUGS is finished drawing samples, follow the
promp and return to R.

### More examples:

The main function for the joint model is `lcra()`. Use `?lcra` for the R
help file.

Here is an example analysis on simulated data with continuous and
discrete outcomes:

``` r
data('paper_sim')
data('paper_sim_binary')

# Set initial values
inits =
  list(
    list(
      theta = c(0.33, 0.33, 0.34),
      beta = rep(0, length = 2),
      alpha = rep(0, length = 3),
      tau = 0.5
    ),
    list(
      theta = c(0.33, 0.33, 0.34),
      beta = rep(0, length = 2),
      alpha = rep(0, length = 3),
      tau = 0.5
    ),
    list(
      theta = c(0.33, 0.33, 0.34),
      beta = rep(0, length = 2),
      alpha = rep(0, length = 3),
      tau = 0.5
    )
  )

inits_binary =
  list(
    list(
      theta = c(0.33, 0.33, 0.34),
      beta = rep(0, length = 2),
      alpha = rep(0, length = 3)
    ),
    list(
      theta = c(0.33, 0.33, 0.34),
      beta = rep(0, length = 2),
      alpha = rep(0, length = 3)
    ),
    list(
      theta = c(0.33, 0.33, 0.34),
      beta = rep(0, length = 2),
      alpha = rep(0, length = 3)
    )
  )

# Fit model 1
fit.gaus_paper =
  lcra(
    formula = Y ~ X1 + X2,
    family = "gaussian",
    data = paper_sim,
    nclasses = 3,
    manifest = paste0("Z", 1:10),
    inits = inits,
    dir = tempdir(),
    n.chains = 3,
    n.iter = 5000,
    parameters.to.save = c("theta", "beta", "true", "alpha")
  )

# Model 1 results
print(fit.gaus_paper, digits = 3)
plot(fit.gaus_paper)

# Extract results
fit.gaus_paper$median$true
fit.gaus_paper$median$beta
fit.gaus_paper$median$alpha

# Fit model 2
fit.binom_paper = 
  lcra(
    formula = Y ~ X1 + X2,
    family = "binomial",
    data = paper_sim_binary,
    nclasses = 3,
    manifest = paste0("Z", 1:10),
    inits = inits_binary,
    dir = tempdir(),
    n.chains = 3,
    n.iter = 5000,
    parameters.to.save = c("theta", "beta", "true", "alpha")
  )

# Model 2 results
print(fit.binom_paper, digits = 3)
plot(fit.binom_paper)

# Extract results
fit.binom_paper$median$true
fit.binom_paper$median$beta
fit.binom_paper$median$alpha
```

For more examples, run `?lcra` at the console, and scroll to Examples in
the documentation file.

### Current Suggested Citation

“Methods to account for uncertainty in latent class assignments when
using latent classes as predictors in regression models, with
application to acculturation strategy measures” (2020) In press at
Epidemiology. <doi:10.1097/EDE.0000000000001139>
