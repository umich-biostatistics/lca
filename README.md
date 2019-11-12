# R package `lca`

This `R` package implements expectation maximization (EM) and Bayesian latent class analyses (insert reference).

# Dependencies

This package uses the R interface for WinBUGS called `R2WinBUGS`. The package `R2WinBUGS` in turn depends on the standalone Windows program `WinBUGS`. Follow this link: [University of Cambridge MRC Biostatistics Unit](https://www.mrc-bsu.cam.ac.uk/software/bugs/the-bugs-project-winbugs/) for download and installation instructions. `WinBUGS` can also be used on Mac using [Wine](https://www.winehq.org/). Plenty of tutorials that demonstrate install of WinBUGS on non-Windows machines are available on Google. 

Once the standalone `WinBUGS` program is installed, open R and run:

```r
library(devtools)
# may take some time:
install_github('umich-biostatistics/lca') 

library(lca)
```

### Example

Example code

The main function for Bayesian LCA is `bayes_lca()`. Use `?bayes_lca()` for the R
help file.

Here is an example Bayesian latent class analysis on simulated data:

```r

```

### Vignette

Once the package is installed, run the following to launch a vignette with detailed examples of the package functions.

```r
vignette('lca-learn')
```

### Current Suggested Citation

Reference
