
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

plot.bugs <- function (x, display.parallel = FALSE, ...){
  oldpar <- par(no.readonly = TRUE)    # code line i
  on.exit(par(oldpar))            # code line i + 1
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

"bugs.plot.summary" <-
  function (sims, ...){
    isDIC <- sims$isDIC
    oldpar <- par(no.readonly = TRUE)    # code line i
    on.exit(par(oldpar))            # code line i + 1
    if (.Device=="windows" ||
        (.Device=="null device" && options("device")=="windows")){
      cex.names <- .7
      cex.top <- .7
      cex.points <- .7
      max.length <- 50
      min.width <- .01
    }
    else {
      cex.names <- .7
      cex.top <- .7
      cex.points <- .3
      max.length <- 80
      min.width <- .005
    }
    summ <- sims$summary
    sims.array <- sims$sims.array
    n.chains <- sims$n.chains
    n.parameters <- nrow(summ)
    
    J0 <- unlist(lapply(sims$long.short, length))
    if (isDIC) J0 <- J0[1:(length(J0)-1)]  # don't display deviance summaries
    J <- J0
    total <- ceiling(sum(J+.5))
    while ((total > max.length) && max(J)>1){### vielleicht optimieren ...
      J[J==max(J)] <- max(J)-1
      total <- ceiling(sum(J+.5))
    }
    if (is.R()){
      pos <- -1
    } else {
      pos <- -1.5
    }
    ypos <- NULL
    id <- NULL
    ystart <- NULL
    jj <- 1:J[1]
    n.roots <- length(sims$root.short)
    if (isDIC) n.roots <- n.roots-1        # don't display deviance summaries
    ystart <- numeric(n.roots)
    for (k in 1:n.roots){
      ystart[k] <- pos
      if (is.R()) {
        ypos <- c(ypos, pos - seq(0, J[k]-1))
      } else {
        # In S-PLUS, increase the vertical spacing
        ypos <- c(ypos, pos - 1.5*seq(0, J[k]-1))
      }
      id <- c(id, 1:J[k])
      if (is.R()) {
        pos <- pos - J[k] -.5
      } else {
        pos <- pos - 1.5*J[k] -0.75
      }
      if (k>1) jj <- c(jj, sum(J0[1:(k-1)]) + (1:J[k]))
    }
    if (is.R()){
      bottom <- min(ypos)-1  
    } else {
      bottom <- min(ypos)-1.5  
    }
    med <- numeric(sum(J))
    i80 <- matrix( , sum(J), 2)
    i80.chains <- array (NA, c(sum(J), n.chains, 2))
    for (j in 1:sum(J)){
      med[j] <- median (sims.array[,,jj[j]])
      i80[j,] <- quantile (sims.array[,,jj[j]], c(.1,.9))
      for (m in 1:n.chains)
        i80.chains[j,m,] <- quantile (sims.array[,m,jj[j]], c(.1,.9))
    }
    rng <- range (i80, i80.chains)
    p.rng <- pretty(rng, n = 2)
    b <- 2 / (max(p.rng) - min(p.rng))
    a <- -b * p.rng[1]
    
    par (mar=c(0,0,1,3))
    plot (c(0,1), c(min(bottom, -max.length)-3,2.5),
          ann=FALSE, bty="n", xaxt="n", yaxt="n", type="n")
    
    W <- max(strwidth(unlist(dimnames(summ)[[1]]), cex=cex.names))
    B <- (1-W)/3.6
    A <- 1-3.5*B
    B <- (1-A)/3.5
    b <- B*b
    a <- A + B*a
    text (A+B*1, 2.5, "80% interval for each chain", cex=cex.top)
    lines (A+B*c(0,2), c(0,0))
    lines (A+B*c(0,2), rep(bottom,2))  
    if(n.chains > 1){
      text (A+B*3, 2.6, "R-hat", cex=cex.top)
      lines (A+B*c(2.5,3.5), c(0,0))
      lines (A+B*c(2.5,3.5), rep(bottom,2))
    }
    #
    # line at zero
    #
    if (min(p.rng)<0 & max(p.rng)>0)
      lines (rep(a,2), c(0,bottom), lwd=.5, col="gray")
    
    for (x in p.rng){
      text (a+b*x, 1, x, cex=cex.names)
      lines (rep(a+b*x,2), c(0,-.2))
      text (a+b*x, bottom-1, x, cex=cex.names)
      lines (rep(a+b*x,2), bottom+c(0,.2))
    }
    if(n.chains > 1)
      for (x in seq(1,2,.5)){
        text (A+B*(1.5+seq(1,2,.5)), rep(1,3), c("1","1.5","2+"), cex=cex.names)
        lines (A+B*rep(1.5+x,2), c(0,-.2))
        text (A+B*(1.5+seq(1,2,.5)), rep(bottom-1,3), c("1","1.5","2+"),
              cex=cex.names)
        lines (A+B*rep(1.5+x,2), bottom+c(0,.2))
      }
    for (j in 1:sum(J)){
      name <- dimnames(summ)[[1]][jj[j]]
      if (id[j]==1)
        if (is.R()) {
          text (0, ypos[j], name, adj=0, cex=cex.names)
        } else {
          # in S-PLUS, strwidth is an upper bound on the length of the string,
          # so we must align the brackets differently than in R
          pos <- as.vector(regexpr("[[]", name))
          text (0, ypos[j], substring(name, 1, pos-1), adj=0, cex=cex.names)
          text (strwidth(substring(name,1,pos-1),cex=cex.names),
                ypos[j], substring(name, pos, nchar(name)), adj=0, cex=cex.names)
        }
      else {
        pos <- as.vector(regexpr("[[]", name))
        text (strwidth(substring(name,1,pos-1),cex=cex.names),
              ypos[j], substring(name, pos, nchar(name)), adj=0, cex=cex.names)
      }
      for (m in 1:n.chains){
        interval <- a + b*i80.chains[j,m,]
        if (interval[2]-interval[1] < min.width)
          interval <- mean(interval) + c(-1,1)*min.width/2
        lines (interval, rep(ypos[j]-.1*(m-(n.chains+1)/2),2), lwd=1, col=m+1)
        if(n.chains > 1) 
          points (A+B*(1.5 + min(max(summ[jj[j],"Rhat"],1),2)), ypos[j], pch=20, cex=cex.points)
      }
    }
    for (k in 1:n.roots){
      if (J[k]<J0[k]) text (-.015, ystart[k], "*", cex=cex.names,
                            col="red")
    }
    if (sum(J!=J0)>0) text (0, bottom-3,
                            "*  array truncated for lack of space", adj=0, cex=cex.names, col="red")
  }

"bugs.plot.inferences" <-
  function (sims, display.parallel, ...){
    oldpar <- par(no.readonly = TRUE)    # code line i
    on.exit(par(oldpar))            # code line i + 1
    if (.Device=="windows" ||
        (.Device=="null device" && options("device")=="windows")){
      cex.names <- .7
      cex.axis <- .6
      cex.tiny <- .4
      cex.points <- .7
      standard.width <- 30
      max.width <- 40
      min.width <- .02
    }
    else {
      cex.names <- .7
      cex.axis <- .6
      cex.tiny <- .4
      cex.points <- .3
      standard.width <- 30
      max.width <- 40
      min.width <- .01
    }
    rootnames <- sims$root.short
    n.roots <- length(rootnames)
    sims.array <- sims$sims.array
    n.chains <- sims$n.chains
    dimension.short <- sims$dimension.short
    indexes.short <- sims$indexes.short
    long.short <- sims$long.short
    height <- .6
    par (mar=c(0,0,1,0))
    
    plot (c(0,1), c(-n.roots-.5,-.4),
          ann=FALSE, bty="n", xaxt="n", yaxt="n", type="n")
    
    W <- max(strwidth(rootnames, cex=cex.names))
    B <- (1-W)/3.8
    A <- 1-3.5*B
    if (display.parallel)
      text (A, -.4, "80% interval for each chain", adj=0, cex=cex.names)
    else
      text (A, -.4, "medians and 80% intervals", adj=0, cex=cex.names)
    num.height <- strheight (1:9, cex=cex.tiny)
    for (k in 1:n.roots){
      text (0, -k, rootnames[k], adj=0, cex=cex.names)
      J <- min (length(long.short[[k]]), max.width)
      if (k==1)
        index <- 1:J
      else
        index <- sum (unlist(lapply(long.short,length))[1:(k-1)]) + 1:J
      spacing <- 3.5/max(J,standard.width)
      med <- numeric(J)
      i80 <- matrix( , J, 2)
      med.chains <- matrix( , J, sims$n.chains)
      i80.chains <- array(NA, c(J, sims$n.chains, 2))
      for (j in 1:J){
        med[j] <- median(sims.array[,,index[j]])
        i80[j,] <- quantile(sims.array[,,index[j]], c(.1,.9))
        for (m in 1:n.chains){
          med.chains[j,m] <- quantile (sims.array[,m,index[j]], .5)
          i80.chains[j,m,] <- quantile (sims.array[,m,index[j]], c(.1,.9))
        }
      }
      rng <- range (i80, i80.chains)
      p.rng <- pretty(rng, n = 2)
      b <- height/(max(p.rng) - min(p.rng))
      a <- -(k+height/2) - b*p.rng[1]
      lines (A+c(0,0), -k+c(-height/2,height/2))
      #
      # plot a line at zero (if zero is in the range of the mini-plot)
      #    
      if (min(p.rng)<0 & max(p.rng)>0)
        lines (A+B*spacing*c(0,J+1), rep (a,2), lwd=.5, col="gray")
      
      for (x in p.rng){
        text (A-B*.2, a+b*x, x, cex=cex.axis)
        lines (A+B*c(-.05,0), rep(a+b*x,2))
      }
      for (j in 1:J){
        if (display.parallel){
          for (m in 1:n.chains){
            interval <- a + b*i80.chains[j,m,]
            if (interval[2]-interval[1] < min.width)
              interval <- mean(interval) + c(-1,1)*min.width/2
            lines (A+B*spacing*rep(j+.6*(m-(n.chains+1)/2)/n.chains,2),
                   interval, lwd=.5, col=m+1)
          }
        }
        else {
          lines (A+B*spacing*rep(j,2), a + b*i80[j,], lwd=.5)
          for (m in 1:n.chains)
            #        points (A+B*spacing*j, a + b*med[j], pch=20, cex=cex.points)
            points (A+B*spacing*j, a + b*med.chains[j,m], pch=20, cex=cex.points,
                    col=m+1)
        }
        dk <- dimension.short[k]
        if (dk>0){
          for (m in 1:dk){
            index0 <- indexes.short[[k]][[j]][m]
            if (j==1)
              text(A+B*spacing*j, -k-height/2-.05-num.height*(m-1), index0,
                   cex=cex.tiny)
            else if (index0!=indexes.short[[k]][[j-1]][m] &
                     (index0%%(floor(log10(index0)+1))==0))
              text(A+B*spacing*j, -k-height/2-.05-num.height*(m-1), index0,
                   cex=cex.tiny)
          }
        }
      }
      if (J<length(long.short[[k]])) text (-.015, -k, "*",
                                           cex=cex.names, col="red")
    }
  }
