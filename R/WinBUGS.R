
# print and plot methods from R2WinBUGS

#' Print a BUGS object
#' 
#' This print function was copied from the package R2WinBUGS because they
#' failed to export it, creating complications for our methods.
#'
#' @param x
#' @param digits.summary

print.bugs <- function(x, digits.summary = 1, ...)
{
  fround <- function(x, digits) { format(round(x, digits), nsmall=digits) }
  
  if(!is.null(x$model.file))
    cat("Inference for Bugs model at \"", x$model.file, "\", ", sep="")
  if(!is.null(x$program))
    cat("fit using ", x$program, ",", sep="")
  cat("\n ", x$n.chains, " chains, each with ", x$n.iter,
      " iterations (first ", x$n.burnin, " discarded)", sep = "")
  if(x$n.thin > 1) cat(", n.thin =", x$n.thin)
  cat("\n n.sims =", x$n.sims, "iterations saved\n")
  print(round(x$summary, digits.summary), ...)
  
  if(x$n.chains > 1) {
    cat("\nFor each parameter, n.eff is a crude measure of effective sample size,")
    cat("\nand Rhat is the potential scale reduction factor (at convergence, Rhat=1).\n")
  }
  
  if(x$isDIC) {
    msgDICRule <- ifelse(x$DICbyR,
                         "(using the rule, pD = var(deviance)/2)", ## Gelman tweak
                         "(using the rule, pD = Dbar-Dhat)")       ## BUGS
    cat(paste("\nDIC info ", msgDICRule, "\n", sep=""))
    if(length(x$DIC) == 1) {
      cat("pD =", fround(x$pD, 1), "and DIC =", fround(x$DIC, 1))
    } else if(length(x$DIC)>1) {
      print(round(x$DIC, 1))
    }
    cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n")
  }
  invisible(x)
}

#' Plot a BUGS object
#'
#' This plot function was copied from the package R2WinBUGS because they
#' failed to export it, creating complications for our methods.
#' 
#' @param x
#' @param display.parallel

plot.bugs <- function (x, display.parallel = FALSE, ...){
  mar.old <- par("mar")
  pty.old <- par(pty = "m")
  mfrow.old <- par("mfrow")
  if (is.R())
    layout(matrix(c(1,2),1,2))
  else
    par(mfrow = c(1,2))
  
  bugs.plot.summary (x, ...)
  bugs.plot.inferences (x, display.parallel, ...)
  header <- ""
  if(!is.null(x$model.file))
    header <- paste(header, "Bugs model at \"", x$model.file, "\", ", sep="")
  if(!is.null(x$program))
    header <- paste(header, "fit using ", x$program, ", ", sep="")
  header <- paste(header, x$n.chains, " chains, each with ",
                  x$n.iter, " iterations (first ", x$n.burnin, " discarded)", sep = "")
  mtext(header, outer = TRUE, line = -1, cex = 0.7)
  if (is.R())  par(pty = pty.old[[1]], mar = mar.old, mfrow = mfrow.old)
  else  invisible(par(pty = pty.old[[1]], mar = mar.old, mfrow = mfrow.old))
}
